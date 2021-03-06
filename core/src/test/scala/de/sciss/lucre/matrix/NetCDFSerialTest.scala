package de.sciss.lucre.matrix

import de.sciss.file._
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.stm.Durable
import de.sciss.lucre.stm.store.BerkeleyDB

object NetCDFSerialTest extends App {
  type S = Durable
  implicit val system: S = Durable(BerkeleyDB.tmp())

  initTypes()

  val f: File = {
    val name = "25_pr_Amon_MPI-ESM-LR_rcp45_r1i1p1_200601-230012.nc"
    val p0 = userHome / "IEM" / "SysSon" / "Data" / "201211" / "gcm" / "New_Deutschlandradio_MPI_M" / "pr"
    val f0 = p0 / name
    if (f0.isFile) f0 else {
      val p1 = file("/data") / "IEM" / "SysSon" / "data"
      val f1 = p1 / name
      require (f1.isFile)
      f1
    }
  }

  try {
    val net = ucar.nc2.NetcdfFile.open(f.path)
    try {
      implicit val resolver: DataSource.Resolver[S] = DataSource.Resolver.seq[S](net)

      val dsv = system.step { implicit tx =>
        val loc = ArtifactLocation.newConst[S](f.parent)
        val art = Artifact(loc, f)
        val ds  = DataSource(art)
        val id  = tx.newId()
        tx.newVar[DataSource[S]](id, ds)
      }

      val vrv = system.step { implicit tx =>
        val ds  = dsv()
        val vr  = ds.variables.head
        val id  = tx.newId()
        tx.newVar[Matrix[S]](id, vr)
      }

      system.step { implicit tx =>
        val vr = vrv()
        println(vr)
      }

      val mvv = system.step { implicit tx =>
        val mv = Matrix.Var(vrv())
        val id = tx.newId()
        tx.newVar[Matrix[S]](id, mv)
      }

      system.step { implicit tx =>
        val Matrix.Var(mv) = mvv()
        println(mv)
        mv() = Reduce(mv(), Dimension.Selection.Index(IntObj.newConst(0)), Reduce.Op.Apply(IntObj.newConst(0)))
      }

      system.step { implicit tx =>
        val Matrix.Var(mv) = mvv()
        println(mv())
      }

    } finally {
      net.close()
    }

  } finally {
    system.close()
  }
}
