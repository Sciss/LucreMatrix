package de.sciss.lucre.matrix

import de.sciss.file._
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.stm.InMemory

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object WindowReadTest {
  def main(args: Array[String]): Unit = run()

  def run(): Unit = {
    type S = InMemory
    implicit val system = InMemory()

    initTypes()

    val f = userHome / "sysson" / "nc" / "5x30-climatology_2001-05-01_2016-05-01_ta_clean.nc"

    try {
      val net = ucar.nc2.NetcdfFile.open(f.path)
      try {
        implicit val resolver = DataSource.Resolver.seq[S](net)

        val dsv = system.step { implicit tx =>
          val loc = ArtifactLocation.newConst[S](f.parent)
          val art = Artifact(loc, f)
          val ds  = DataSource(art)
          val id  = tx.newID()
          tx.newVar[DataSource[S]](id, ds)
        }

        val vrv = system.step { implicit tx =>
          val ds        = dsv()
          val Some(vr)  = ds.variables.find(_.name == "Temperature")
          assert(vr.shape == Vector(180, 12, 36, 601)) // [Time][Longitude][Latitude][Altitude]
          val r1        = Reduce(vr, Dimension.Selection.Name[S]("Longitude"), Reduce.Op.Apply[S](6 ))
          val r2        = Reduce(r1, Dimension.Selection.Name[S]("Latitude" ), Reduce.Op.Slice[S](18, 19))
          val r3        = Reduce(r2, Dimension.Selection.Name[S]("Time"     ), Reduce.Op.Slice[S](10, 16))
          val r4        = Reduce(r3, Dimension.Selection.Name[S]("Altitude" ), Reduce.Op.Slice[S](100, 104))
          assert(r4.shape == Vector(7, 1, 2, 5))
          // r4.reader(-1)
          val id = tx.newID()
          tx.newVar[Matrix[S]](id, r4)
        }

        val data1 = Array(
          Array(238.68216, 240.33311, 240.29453, 239.26369, 238.63297, 238.73473, 238.04228),
          Array(237.8761 , 239.54564, 239.5016 , 238.4729 , 237.89272, 237.9607 , 237.30122),
          Array(237.08833, 238.7646 , 238.70535, 237.68289, 237.1597 , 237.19106, 236.57042),
          Array(236.31773, 237.99855, 237.90671, 236.89223, 236.43344, 236.42879, 235.8515 ),
          Array(235.55916, 237.23782, 237.105  , 236.09918, 235.70695, 235.66777, 235.14084)
        )

        val data2 = Array(
          Array(239.51402, 240.23567, 239.74763, 239.79013, 238.76927, 238.71835, 239.07732),
          Array(238.73468, 239.40346, 238.98271, 239.00453, 237.9738 , 237.93555, 238.2944 ),
          Array(237.9652 , 238.56984, 238.2201 , 238.2225 , 237.18947, 237.15935, 237.51498),
          Array(237.19894, 237.74808, 237.44017, 237.44537, 236.41159, 236.38472, 236.74347),
          Array(236.42651, 236.93643, 236.64626, 236.66455, 235.6329 , 235.60703, 235.96756)
        )

        val win1a = data1.flatten
        val win2a = data2.flatten
        val win1b = data1.transpose.flatten
        val win2b = data2.transpose.flatten

        val dimsA = Array(3, 0)   // right-most iteration is time, left-most is altitude
        val dimsB = Array(0, 3)   // and vice versa

        def assertSame(name: String, a: Array[Double], b: Array[Double], tol: Double = 1.0e-4): Unit = {
          assert(a.length == b.length, s"$name - a.length = ${a.length} vs b.length = ${b.length}")
          var i = 0
          while (i < a.length) {
            val aa = a(i)
            val bb = b(i)
            val d  = math.abs(aa - bb)
            assert(d < tol, s"$name - a($i) = $aa vs b($i) = $bb; |d| = $d")
            i += 1
          }
        }

        import scala.concurrent.ExecutionContext.Implicits.global

        def run(name: String, win1: Array[Double], win2: Array[Double], dims: Array[Int]): Future[Unit] = {
          val rFut = system.step { implicit tx =>
            val vr = vrv()
            vr.reader(-1)
          }
          rFut.map { reader =>
            val buf = new Array[Double](win1.length)
            reader.readWindowDouble1D(dims, buf, 0)
            assertSame(s"$name-1", win1, buf)
            reader.readWindowDouble1D(dims, buf, 0)
            assertSame(s"$name-2", win2, buf)
            ()
          }
        }

        val futTot = for {
          _ <- run("A", win1a, win2a, dimsA)
          _ <- run("B", win1b, win2b, dimsB)
        } yield ()

        Await.result(futTot, Duration.Inf)

      } finally {
        net.close()
      }

    } finally {
      system.close()
    }
  }
}