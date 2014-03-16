package de.sciss.lucre.matrix

import de.sciss.lucre.event.Durable
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.file._
import de.sciss.lucre.expr

object NetCDFSerialTest extends App {
  type S = Durable
  implicit val system = Durable(BerkeleyDB.tmp())

  val f = userHome / "IEM" / "SysSon" / "Data" / "201211" / "gcm" / "New_Deutschlandradio_MPI_M" / "pr" /
    "25_pr_Amon_MPI-ESM-LR_rcp45_r1i1p1_200601-230012.nc"

  try {
    val net = ucar.nc2.NetcdfFile.open(f.path)
    try {
      implicit val resolver = DataSource.Resolver.seq[S](net)

      val dsv = system.step { implicit tx =>
        val ds  = DataSource(f)
        val id  = tx.newID()
        tx.newVar[DataSource[S]](id, ds)
      }

      val vrv = system.step { implicit tx =>
        val ds  = dsv()
        val vr  = ds.variables.head
        val id  = tx.newID()
        tx.newVar[Matrix[S]](id, vr)
      }

      system.step { implicit tx =>
        val vr = vrv()
        println(vr)
      }

      val mvv = system.step { implicit tx =>
        val mv = Matrix.Var(vrv())
        val id = tx.newID()
        tx.newVar[Matrix[S]](id, mv)
      }

      system.step { implicit tx =>
        val Matrix.Var(mv) = mvv()
        println(mv)
        mv() = Reduce(mv(), Dimension.Selection.Index(expr.Int.newConst(0)), Reduce.Op.Apply(expr.Int.newConst(0)))
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