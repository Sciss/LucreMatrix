package de.sciss.lucre.matrix

import de.sciss.file._
import de.sciss.synth.io.AudioFile
import org.scalatest.{fixture, Outcome, Matchers}
import de.sciss.lucre.event.Durable
import de.sciss.lucre.expr
import expr.Expr
import de.sciss.lucre.stm.store.BerkeleyDB
import Implicits._
import ucar.{ma2, nc2}

import java.{util => ju}

import scala.concurrent.duration.Duration
import scala.concurrent.Await
import scala.language.implicitConversions

/*
  to run only this test:
  test-only de.sciss.lucre.matrix.CacheSpec
 */
class CacheSpec extends fixture.FlatSpec with Matchers {
  type S            = Durable
  type FixtureParam = (Durable, AudioFileCache)

  def withFixture(test: OneArgTest): Outcome = {
    val system  = Durable(BerkeleyDB.tmp())
    val cache   = AudioFileCache()
    try {
      test((system, cache))
    }
    finally {
      system.close()
    }
  }

  implicit def mkConst(i: Int): Expr.Const[S, Int] = expr.Int.newConst(i)

  "A Zeros Matrix" should "sing while you sell" in { args =>
    implicit val (cursor, cache) = args

    val Seq(km, k0, k1) = cursor.step { implicit tx =>
      val _z = Matrix.zeros(13, 21)
      (-1 to 1).map(_z.getKey)
    }

    implicit val resolver = DataSource.Resolver.empty[S]

    def testKey(key: Matrix.Key, numCh: Int, numFr: Int): Unit = {
      val fut   = cursor.step { implicit tx => cache.acquire(key) }
      val value = Await.result(fut, Duration.Inf)

      assert(value.spec.numChannels === numCh)
      assert(value.spec.numFrames   === numFr)

      val af = AudioFile.openRead(value.file)
      assert(af.spec.copy(byteOrder = None) === value.spec)

      val buf = af.buffer(numFr)
      af.read(buf)
      for (ch <- 0 until numCh) assert(buf(ch).toVector === Vector.fill(numFr)(0f))
    }

    testKey(km, numCh = 13 * 21, numFr = 1)
    testKey(k0, numCh = 21, numFr = 13)
    testKey(k1, numCh = 13, numFr = 21)
  }

  // cf. http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/tutorial/NetcdfWriting.html
  def createData(): File = {
    val f         = File.createTemp(suffix = ".nc")
    val location  = f.path
    val writer    = nc2.NetcdfFileWriter.createNew(nc2.NetcdfFileWriter.Version.netcdf3, location, null)
    val latDim    = writer.addDimension(null, "lat",  64)
    val lonDim    = writer.addDimension(null, "lon", 128)
    val dims      = new ju.ArrayList[nc2.Dimension]()
    dims.add(latDim)
    dims.add(lonDim)
    val t         = writer.addVariable(null, "temperature", ma2.DataType.FLOAT /* DOUBLE */, dims)
    t.addAttribute(new nc2.Attribute("units", "K"))
    val data      = ma2.Array.factory(classOf[Int], Array(3), Array(1, 2, 3))
    t.addAttribute(new nc2.Attribute("scale", data))
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

    // create the file
    writer.create()

    // write data to variable
    val v = writer.findVariable("temperature")
    val shape = v.getShape
    val A = new ma2.ArrayFloat /*  ArrayDouble */ .D2(shape(0), shape(1))
    val ima = A.getIndex
    for (i <- 0 until shape(0)) {
      for (j <- 0 until shape(1)) {
        A.setFloat /* .setDouble */ (ima.set(i, j), (i * 1000000 + j * 1000).toFloat /* .toDouble */)
      }
    }
    val origin = new Array[Int](2)
    writer.write(v, origin, A)

    writer.close()
    f
  }

  "A NetCDF Matrix" should "sing while you sell" in { args =>
    val f = createData()

  }
}