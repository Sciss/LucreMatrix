package de.sciss.lucre.matrix

import de.sciss.lucre.event.Durable
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.file._

object NetCDFSerialTest extends App {
  type S = Durable
  implicit val system = Durable(BerkeleyDB.tmp())

  val f = userHome / "IEM" / "SysSon" / "Data" / "201211" / "gcm" / "New_Deutschlandradio_MPI_M" / "pr" /
    "25_pr_Amon_MPI-ESM-LR_rcp45_r1i1p1_200601-230012.nc"

  try {
    val net = ucar.nc2.NetcdfFile.open(f.path)
    try {
      implicit val resolver = DataSource.Resolver.seq[S](net)

      val dsH = system.step { implicit tx =>
        val ds = DataSource(f)
        tx.newHandle(ds)
      }

      val vrH = system.step { implicit tx =>
        val ds = dsH()
        val vr = ds.variables.head
        tx.newHandle[Matrix[S]](vr)
      }

      system.step { implicit tx =>
        val vr = vrH()
        println(vr)
      }

    } finally {
      net.close()
    }

  } finally {
    system.close()
  }
}
