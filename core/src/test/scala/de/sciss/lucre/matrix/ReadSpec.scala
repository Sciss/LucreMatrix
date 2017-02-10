package de.sciss.lucre.matrix

import java.{util => ju}

import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.matrix.Implicits._
import de.sciss.lucre.stm.InMemory
import org.scalatest.{Matchers, Outcome, fixture}

import scala.language.implicitConversions

/*
  to run only this test:
  test-only de.sciss.lucre.matrix.ReadSpec
 */
class ReadSpec extends fixture.FlatSpec with Matchers {
  type S            = InMemory
  type FixtureParam = InMemory

  initTypes()

  def withFixture(test: OneArgTest): Outcome = {
    val system = InMemory()
    try {
      test(system)
    }
    finally {
      system.close()
    }
  }

  implicit def mkConst(i: Int)(implicit tx: S#Tx): IntObj.Const[S] = IntObj.newConst(i)

  private def fillBuf(numFrames: Int, numChannels: Int, value: Float = 0f) = {
    val arr = Array.ofDim[Float](numChannels, numFrames)
    for (ch <- 0 until numChannels) ju.Arrays.fill(arr(ch), value)
    arr
  }

  implicit val DummyRes: DataSource.Resolver.Seq[S] = DataSource.Resolver.empty

  "A Zeros Matrix" should "read data" in { cursor =>
    cursor.step { implicit tx =>
      val _z = Matrix.zeros(4, 3, 2)
      assert(_z.rank === 3)
      assert(_z.reducedRank === 3)
      assert(_z.shape === Vec(4, 3, 2))
      assert(_z.reducedShape === Vec(4, 3, 2))

      val r2 = _z.reader(2)
      assert(r2.numFrames   ===  2)
      assert(r2.numChannels === 12)
      val b2 = fillBuf(numFrames = 4, numChannels = 12, value = 1f)
      r2.readFloat2D(b2, 1, 2)
      for (ch2 <- 0 until 12) {
        val b2c = b2(ch2)
        assert(b2c.to[Vector] === Vector(1f, 0f, 0f, 1f))
      }

      val r1 = _z.reader(1)
      assert(r1.numFrames   ===  3)
      assert(r1.numChannels ===  8)
      val b1 = fillBuf(numFrames = 5, numChannels = 8, value = 1f)
      r1.readFloat2D(b1, 1, 3)
      for (ch1 <- 0 until 8) {
        val b1c = b1(ch1)
        assert(b1c.to[Vector] === Vector(1f, 0f, 0f, 0f, 1f))
      }

      val r0 = _z.reader(0)
      assert(r0.numFrames   ===  4)
      assert(r0.numChannels ===  6)
      val b0 = fillBuf(numFrames = 6, numChannels = 6, value = 1f)
      r0.readFloat2D(b0, 1, 4)
      for (ch0 <- 0 until 6) {
        val b0c = b0(ch0)
        assert(b0c.to[Vector] === Vector(1f, 0f, 0f, 0f, 0f, 1f))
      }

      val rm = _z.reader(-1)
      assert(rm.numFrames   ===  1)
      assert(rm.numChannels ===  24)
      val bm = fillBuf(numFrames = 3, numChannels = 24, value = 1f)
      rm.readFloat2D(bm, 1, 1)
      for (chm <- 0 until 24) {
        val bmc = bm(chm)
        assert(bmc.to[Vector] === Vector(1f, 0f, 1f))
      }
    }
  }

  "A Constant Matrix" should "read data" in { cursor =>
    cursor.step { implicit tx =>
      val _z = Matrix.newConst3D("M",
        Vec(
          Vec(Vec( 0f,  1f), Vec( 2f,  3f), Vec( 4f,  5f)),
          Vec(Vec( 6f,  7f), Vec( 8f,  9f), Vec(10f, 11f)),
          Vec(Vec(12f, 13f), Vec(14f, 15f), Vec(16f, 17f)),
          Vec(Vec(18f, 19f), Vec(20f, 21f), Vec(22f, 23f))
        )
      )
      assert(_z.rank === 3)
      assert(_z.reducedRank === 3)
      assert(_z.shape === Vec(4, 3, 2))
      assert(_z.reducedShape === Vec(4, 3, 2))

      val r2 = _z.reader(2)
      assert(r2.numFrames   ===  2)
      assert(r2.numChannels === 12)
      val b2 = fillBuf(numFrames = 4, numChannels = 12, value = 99f)
      r2.readFloat2D(b2, 1, 2)

      for (ch <- 0 until 12)
        assert(b2(ch).to[Vector] === Vector(99f, (ch * 2).toFloat, (ch * 2 + 1).toFloat, 99f))

      val r1 = _z.reader(1)
      assert(r1.numFrames   ===  3)
      assert(r1.numChannels ===  8)
      val b1 = fillBuf(numFrames = 5, numChannels = 8, value = 99f)
      r1.readFloat2D(b1, 1, 3)

      def assertCh1(ch: Int)(xs: Int*) =
        assert(b1(ch).toVector === (99f +: xs.map(_.toFloat).to[Vector] :+ 99f))

      assertCh1(0)( 0,  2,  4)
      assertCh1(1)( 1,  3,  5)
      assertCh1(2)( 6,  8, 10)
      assertCh1(3)( 7,  9, 11)
      assertCh1(4)(12, 14, 16)
      assertCh1(5)(13, 15, 17)
      assertCh1(6)(18, 20, 22)
      assertCh1(7)(19, 21, 23)

      val r0 = _z.reader(0)
      assert(r0.numFrames   ===  4)
      assert(r0.numChannels ===  6)
      val b0 = fillBuf(numFrames = 6, numChannels = 6, value = 99f)
      r0.readFloat2D(b0, 1, 4)

      for (ch <- 0 until 6)
        assert(b0(ch).to[Vector] === (99f +: Vector.tabulate(4)(i => i * 6 + ch) :+ 99f))

      val rm = _z.reader(-1)
      assert(rm.numFrames   ===  1)
      assert(rm.numChannels ===  24)
      val bm = fillBuf(numFrames = 3, numChannels = 24, value = 99f)
      rm.readFloat2D(bm, 1, 1)

      for (ch <- 0 until 24)
        assert(bm(ch).to[Vector] === Vector(99f, ch.toFloat, 99f))
    }
  }
}