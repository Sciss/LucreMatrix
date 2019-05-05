package de.sciss.lucre.matrix

import de.sciss.file._
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.{IntObj, StringObj}
import de.sciss.lucre.matrix.DataSource.Resolver
import de.sciss.lucre.stm.{InMemory, Workspace}
import de.sciss.synth.proc.GenContext
import ucar.nc2

import scala.concurrent.Future
import scala.util.Success

object NewReadTest {
  def main(args: Array[String]): Unit = NewReadTest

  val p: File = userHome / "IEM" / "SysSon" / "Data" / "201211" / "RO_Data" / "ROdata__011995_to_122008__months.nc"
  require (p.isFile)

  type S = InMemory

  initTypes()

  val ncf               : nc2.NetcdfFile      = nc2.NetcdfFile.open(p.path)
  implicit val system   : S                   = InMemory()
  implicit val resolver : Resolver.Seq    [S] = Resolver.seq(ncf)
  implicit val ws       : Workspace       [S] = Workspace.Implicits.dummy
  implicit val context  : GenContext      [S] = system.step { implicit tx => GenContext[S] }

  import system.step

  val (dimTemp, redLatH, redLatVarH, redLatStH, redLatFromH, redLatToH, redLatStepH) = step { implicit tx =>
    val loc     = ArtifactLocation.newConst[S](p.parent)
    val art     = Artifact(loc, p)
    val ds      = DataSource[S](art)
    val full    = ds.variables.find(_.name == "temp").get
    val dim     = full.dimensions.indexWhere(_.name == "time")
    assert(dim >= 0)
    val redAlt  = Reduce(full, Dimension.Selection.Name(StringObj.newConst("plev")),
      Reduce.Op.Apply(IntObj.newConst(6)))
    val redLatFrom = IntObj.newVar[S](IntObj.newConst(0))
    val redLatTo   = IntObj.newVar[S](IntObj.newConst(17))
    val redLatStep = IntObj.newVar[S](IntObj.newConst(2))
    val redLatSl  = Reduce.Op.Slice (redLatFrom, redLatTo)
    val redLatSt  = Reduce.Op.Stride(/* redLatFrom, redLatTo, */ redLatStep)
    val redLatVar = Reduce.Op.Var(redLatSl)
    val redLat    = Reduce(redAlt, Dimension.Selection.Name(StringObj.newConst("lat")), redLatVar)

    (dim, tx.newHandle(redLat), tx.newHandle(redLatVar), tx.newHandle(redLatSt: Reduce.Op[S]),
      tx.newHandle(redLatFrom), tx.newHandle(redLatTo), tx.newHandle(redLatStep)) // IntelliJ highlight bug
  }

  def show(r: Matrix.Reader): Unit = {
    val numCh   = r.numChannels
    val bufSize = math.min(r.numFrames, 32).toInt
    val buf     = Array.ofDim[Float](numCh, bufSize)

    var pos = 0
    while (pos < r.numFrames) {
      val len = math.min(r.numFrames - pos, bufSize).toInt
      println("----READ----")
      r.readFloat2D(buf, 0, len)
      for (i <- 0 until len) {
        val ln = Vector.tabulate(numCh) { ch =>
          val x = buf(ch)(i)
          f"$x%1.1f"
        } .mkString(s"$pos: ", " ", "")
        println(ln)
        pos += 1
      }
      // pos += len
    }
  }

  import scala.concurrent.ExecutionContext.Implicits.global

  private val f1 = locally {
    val rFut    = step { implicit tx => redLatH().reader(streamDim = dimTemp)}
    rFut.andThen { case Success(r) =>
      val numCh   = r.numChannels
      assert(numCh == 18)
      assert(r.numFrames == 168)
      println("\n:::: all latitudes ::::")
      show(r)
    }
  }

  private val f2 = locally {
    val rFut = step { implicit tx =>
      redLatFromH().update(IntObj.newConst(1))
      redLatToH  ().update(IntObj.newConst(4))
      redLatH().reader(streamDim = dimTemp)
    }
    rFut.andThen { case Success(r) =>
      println("\n:::: latitude indices 1 to 4 ::::")
      show(r)
    }
  }

  private val f3 = locally {
    val rFut = step { implicit tx =>
      redLatVarH().update(redLatStH())
      redLatH().reader(streamDim = dimTemp)
    }
    rFut.andThen { case Success(r) =>
      println("\n:::: latitude indices 1 to 4 by 2 ::::")
      show(r)
    }
  }

  Future.sequence(List(f1, f2, f3)).onComplete {
    _ => ncf.close()
  }
}