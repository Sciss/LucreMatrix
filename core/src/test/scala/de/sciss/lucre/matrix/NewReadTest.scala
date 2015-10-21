package de.sciss.lucre.matrix

import de.sciss.file._
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.{StringObj, IntObj}
import de.sciss.lucre.stm.InMemory
import ucar.nc2

object NewReadTest extends App {
  val p = userHome / "IEM" / "SysSon" / "Data" / "201211" / "RO_Data" / "ROdata__011995_to_122008__months.nc"
  type S = InMemory

  initTypes()

  val ncf = nc2.NetcdfFile.open(p.path)
  implicit val resolver = DataSource.Resolver.seq[S](ncf)

  val system = InMemory()
  import system.step

  val (dimTemp, redLatH, redLatVarH, redLatStH, redLatFromH, redLatToH, redLatStepH) = step { implicit tx =>
    val loc     = ??? : ArtifactLocation[S] // ArtifactLocation(p.parent)
    val art     = ??? : Artifact[S] // loc.add(p)
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
      tx.newHandle(redLatFrom), tx.newHandle(redLatTo), tx.newHandle(redLatStep))
  }

  def show(r: Matrix.Reader): Unit = {
    val numCh   = r.numChannels
    val bufSize = math.min(r.numFrames, 32).toInt
    val buf     = Array.ofDim[Float](numCh, bufSize)

    var pos = 0
    while (pos < r.numFrames) {
      val len = math.min(r.numFrames - pos, bufSize).toInt
      println("----READ----")
      r.read(buf, 0, len)
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

  locally {
    val r       = step { implicit tx => redLatH().reader(streamDim = dimTemp)}
    val numCh   = r.numChannels
    assert(numCh == 18)
    assert(r.numFrames == 168)
    println("\n:::: all latitudes ::::")
    show(r)
  }

  locally {
    val r = step { implicit tx =>
      redLatFromH().update(IntObj.newConst(1))
      redLatToH  ().update(IntObj.newConst(4))
      redLatH().reader(streamDim = dimTemp)
    }
    println("\n:::: latitude indices 1 to 4 ::::")
    show(r)
  }

  locally {
    val r = step { implicit tx =>
      redLatVarH().update(redLatStH())
      redLatH().reader(streamDim = dimTemp)
    }
    println("\n:::: latitude indices 1 to 4 by 2 ::::")
    show(r)
  }

  ncf.close()
}