package de.sciss.lucre.matrix

import java.{util => ju}

import at.iem.sysson.WorkspaceResolver
import de.sciss.file._
import de.sciss.filecache.Limit
import de.sciss.fscape.lucre.Cache
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.{IntObj, StringObj}
import de.sciss.lucre.matrix.Implicits._
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.stm.{Disposable, Durable, TxnLike}
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.{Folder, GenContext, WorkspaceHandle}
import org.scalatest.{Matchers, Outcome, fixture}
import ucar.{ma2, nc2}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.language.implicitConversions

/*
  to run only this test:
  test-only de.sciss.lucre.matrix.CacheSpec
 */
class CacheSpec extends fixture.FlatSpec with Matchers {
  type S            = Durable
  type FixtureParam = (AudioFileCache, GenContext[S])

  initTypes()
  Cache.init(File.createTemp(directory = true), Limit())

  // XXX TODO --- this should be an option in WorkspaceHandle.Implicits
  // we must have fresh instances because of caching
  private class DummyImpl[T <: Sys[T]] extends WorkspaceHandle[T] {
    def addDependent   (dep: Disposable[T#Tx])(implicit tx: TxnLike): Unit = ()
    def removeDependent(dep: Disposable[T#Tx])(implicit tx: TxnLike): Unit = ()

    def dependents(implicit tx: TxnLike): Iterable[Disposable[T#Tx]] = Nil

    def root(implicit tx: T#Tx): Folder[T] =
      throw new UnsupportedOperationException("No root folder on a dummy workspace handle")
  }

  def withFixture(test: OneArgTest): Outcome = {
    implicit val system: S = Durable(BerkeleyDB.tmp())
    val cache = AudioFileCache()
    try {
      implicit val ws: WorkspaceHandle[S] = new DummyImpl // WorkspaceHandle.Implicits.dummy
      val context = system.step { implicit tx =>
        GenContext[S]
      }
      test((cache, context))
    }
    finally {
      system.close()
    }
  }

  implicit def mkIntConst   (i: Int   )(implicit tx: S#Tx): IntObj   .Const[S] = IntObj   .newConst(i)
  implicit def mkStringConst(s: String)(implicit tx: S#Tx): StringObj.Const[S] = StringObj.newConst(s)

  "A Zeros Matrix" should "sing while you sell." in { args =>
    implicit val (cache, context) = args
    import context.cursor

    val Seq(fm, f0, f1) = cursor.step { implicit tx =>
      val _z = Matrix.zeros(13, 21)
      (-1 to 1).map(_z.prepareReader)
    }

    implicit val resolver : DataSource.Resolver [S] = DataSource.Resolver.empty
//    implicit val ws       : WorkspaceHandle     [S] = WorkspaceHandle.Implicits.dummy

    def testFactory(factory: Matrix.ReaderFactory[S], numCh: Int, numFr: Int): Unit = {
      val fut   = cursor.step { implicit tx =>
        cache.acquire(factory)
      }
      val value = Await.result(fut, Duration.Inf)

      assert(value.spec.numChannels === numCh)
      assert(value.spec.numFrames   === numFr)

      val af = AudioFile.openRead(value.file)
      assert(af.spec.copy(byteOrder = None) === value.spec)

      val buf = af.buffer(numFr)
      af.read(buf)
      for (ch <- 0 until numCh) assert(buf(ch).toVector === Vector.fill(numFr)(0f))
    }

    showLog = true
    for (_ <- 1 to 2) {
      testFactory(fm, numCh = 13 * 21, numFr = 1)
      testFactory(f0, numCh = 21, numFr = 13)
      testFactory(f1, numCh = 13, numFr = 21)
    }
    showLog = false
  }

  // cf. http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/tutorial/NetcdfWriting.html
  def createData(): File = {
    val f         = File.createTemp(suffix = ".nc")
    val location  = f.path
    val writer    = nc2.NetcdfFileWriter.createNew(nc2.NetcdfFileWriter.Version.netcdf3, location, null)
    val latDim    = writer.addDimension(null, "lat", 13 /*  64 */)
    val lonDim    = writer.addDimension(null, "lon", 21 /* 128 */)
    val dimsTemp  = new ju.ArrayList[nc2.Dimension]()
    dimsTemp.add(latDim)
    dimsTemp.add(lonDim)
    // use float instead of double, because sysson plot in previous version restricted to float
    val vrTemp = writer.addVariable(null, "temperature", ma2.DataType.FLOAT /* DOUBLE */, dimsTemp)
    vrTemp.addAttribute(new nc2.Attribute("units", "K"))
    val data      = ma2.Array.factory(classOf[Int], Array(3), Array(1, 2, 3))
    vrTemp.addAttribute(new nc2.Attribute("scale", data))
    // add a string-valued variable: char svar(80)
    /* val sVarLen = */ writer.addDimension(null, "svar_len", 80)
    writer.addVariable(null, "svar", ma2.DataType.CHAR, "svar_len")
    // add a 2D string-valued variable: char names(names, 80)
    /* val names = */ writer.addDimension(null, "names", 3)
    writer.addVariable(null, "names", ma2.DataType.CHAR, "names svar_len")
    // add a scalar variable
    writer.addVariable(null, "scalar", ma2.DataType.DOUBLE, new ju.ArrayList[nc2.Dimension]())
    // add global attributes
    writer.addGroupAttribute(null, new nc2.Attribute("yo", "face"))
    writer.addGroupAttribute(null, new nc2.Attribute("versionD", 1.2))
    writer.addGroupAttribute(null, new nc2.Attribute("versionF", 1.2f))
    writer.addGroupAttribute(null, new nc2.Attribute("versionI", 1))
    writer.addGroupAttribute(null, new nc2.Attribute("versionS", 2.toShort))
    writer.addGroupAttribute(null, new nc2.Attribute("versionB", 3.toByte))

    val dimsLon = new ju.ArrayList[nc2.Dimension]()
    dimsLon.add(lonDim)
    val vrLon = writer.addVariable(null, "lon", ma2.DataType.FLOAT /* DOUBLE */, dimsLon)
    vrLon.addAttribute(new nc2.Attribute("units", "degrees east"))

    // create the file
    writer.create()

    // write data to variable lon
    {
      val v = writer.findVariable("lon")
      val shape = v.getShape
      val A = new ma2.ArrayFloat /*  ArrayDouble */ .D1(shape(0))
      val ima = A.getIndex
      for (i <- 0 until shape(0)) {
        A.setFloat /* .setDouble */ (ima.set(i), (i * 15).toFloat)
      }
      val origin = new Array[Int](1)
      writer.write(v, origin, A)
    }

    // write data to variable temperature
    {
      val v = writer.findVariable("temperature")
      val shape = v.getShape
      val A = new ma2.ArrayFloat /*  ArrayDouble */ .D2(shape(0), shape(1))
      val ima = A.getIndex
      for (i <- 0 until shape(0)) {
        for (j <- 0 until shape(1)) {
          A.setFloat /* .setDouble */ (ima.set(i, j), (i * 100 + j).toFloat /* .toDouble */)
        }
      }
      val origin = new Array[Int](2)
      writer.write(v, origin, A)
    }

    writer.close()
    f
  }

  "A NetCDF Matrix" should "sing while you sell.." in { args =>
    val f   = createData()
    val ncf = nc2.NetcdfFile.open(f.path)

    implicit val (cache, context) = args
    implicit val resolver: DataSource.Resolver[S] = DataSource.Resolver.seq(ncf)
    import context.cursor

    val Seq(fm, f0, f1) = cursor.step { implicit tx =>
      val loc = ArtifactLocation.newConst[S](f.parent)
      val art = Artifact(loc, f)
      val ds  = DataSource(art)
      val _z  = ds.variables.find(_.name == "temperature").get
      (-1 to 1).map(_z.prepareReader)
    }

    def testFactory(factory: Matrix.ReaderFactory[S], numCh: Int, numFr: Int, data: Vec[Float]): Unit = {
      val fut   = cursor.step { implicit tx => cache.acquire(factory) }
      val value = Await.result(fut, Duration.Inf)

      assert(value.spec.numChannels === numCh)
      assert(value.spec.numFrames   === numFr)

      val af = AudioFile.openRead(value.file)
      assert(af.spec.copy(byteOrder = None) === value.spec)

      val buf = af.buffer(numFr)
      af.read(buf)
      val flat = buf.flatten.toVector

      assert(flat === data)
    }

    showLog = true
    val dm = (0 until 13).flatMap(lat => (0 until 21).map(lon => (lat * 100 + lon).toFloat))
    val d0 = (0 until 21).flatMap(lon => (0 until 13).map(lat => (lat * 100 + lon).toFloat))
    val d1 = dm
    for (_ <- 1 to 2) {
      testFactory(fm, numCh = 13 * 21, numFr =  1, data = dm)
      testFactory(f0, numCh = 21     , numFr = 13, data = d0)
      testFactory(f1, numCh = 13     , numFr = 21, data = d1)
    }
    showLog = false
  }

  "A Reduced NetCDF Matrix" should "sing while you sell..." in { args =>
    val f   = createData()
    val ncf = nc2.NetcdfFile.open(f.path)

    implicit val (cache, context) = args
    implicit val resolver = DataSource.Resolver.seq[S](ncf)
    import context.cursor

    val Seq(fm, f0, f1) = cursor.step { implicit tx =>
      val loc = ArtifactLocation.newConst[S](f.parent)
      val art = Artifact(loc, f)
      val ds  = DataSource(art)
      val v   = ds.variables.find(_.name == "temperature").get
      val v0  = Reduce(v , Dimension.Selection.Name("lon"), Reduce.Op.Slice[S](2, 16))
      val v1  = Reduce(v0, Dimension.Selection.Name("lon"), Reduce.Op.Stride[S](3))
      val _z  = Reduce(v1, Dimension.Selection.Name("lat"), Reduce.Op.Slice [S](3, 8))
      (-1 to 1).map(_z.prepareReader)
    }

    def testFactory(factory: Matrix.ReaderFactory[S], numCh: Int, numFr: Int, data: Vec[Float]): Unit = {
      val fut   = cursor.step { implicit tx => cache.acquire(factory) }
      val value = Await.result(fut, Duration.Inf)

      assert(value.spec.numChannels === numCh)
      assert(value.spec.numFrames   === numFr)

      val af = AudioFile.openRead(value.file)
      assert(af.spec.copy(byteOrder = None) === value.spec)

      val buf = af.buffer(numFr)
      af.read(buf)
      val flat = buf.flatten.toVector

      assert(flat === data)
    }

    showLog = true
    val latRange = sampleRange(0 until 13, 3 to 8)
    val lonRange = sampleRange(0 until 21, 2 to 16 by 3)
    val dm = latRange.flatMap(lat => lonRange.map(lon => (lat * 100 + lon).toFloat))
    val d0 = lonRange.flatMap(lon => latRange.map(lat => (lat * 100 + lon).toFloat))
    val d1 = dm
    for (_ <- 1 to 2) {
      testFactory(fm, numCh = 6 * 5, numFr = 1, data = dm)
      testFactory(f0, numCh = 5    , numFr = 6, data = d0)
      testFactory(f1, numCh = 6    , numFr = 5, data = d1)
    }
    showLog = false
  }


  "A 1D NetCDF Matrix" should "be reducible through averaging" in { args =>
    val f   = createData()
    // val ncf = nc2.NetcdfFile.open(f.path)

    implicit val (cache, context) = args
    import context.{cursor, workspaceHandle}
    implicit val resolver = WorkspaceResolver[S] // .Resolver.seq[S](ncf)

    import scala.concurrent.ExecutionContext.Implicits._

    val futReader: Future[Matrix.Reader] = cursor.step { implicit tx =>
      val loc = ArtifactLocation.newConst[S](f.parent)
      val art = Artifact(loc, f)
      val ds  = DataSource(art)
      val v   = ds.variables.find(_.name == "lon").get
      val _z  = Reduce[S](v , Dimension.Selection.Name("lon"), Reduce.Op.Average[S])
      _z.reader(-1)
    }

    val value = Await.result(futReader, Duration.Inf)

    assert(value.numChannels === 1)
    assert(value.numFrames   === 1)

    val buf = new Array[Double](1)
    value.readDouble1D(buf, 0, 1)
    val avg       = buf(0)
    val expected  = (0 until 21).map(_ * 15f).sum / 21

    assert(avg === expected)
  }
}