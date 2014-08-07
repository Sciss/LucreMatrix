package de.sciss.lucre.matrix

import java.{util => ju}

import de.sciss.lucre.event.InMemory
import de.sciss.lucre.expr
import de.sciss.lucre.expr.Expr
import org.scalatest.{Outcome, Matchers, fixture}
import Implicits._
import language.implicitConversions

/*
  to run only this test:
  test-only de.sciss.lucre.matrix.ReadSpec
 */
class ReadSpec extends fixture.FlatSpec with Matchers {
  type S = InMemory
  type FixtureParam = InMemory

  def withFixture(test: OneArgTest): Outcome = {
    val system = InMemory()
    try {
      test(system)
    }
    finally {
      system.close()
    }
  }

  implicit def mkConst(i: Int): Expr.Const[S, Int] = expr.Int.newConst(i)

  private def fillBuf(numFrames: Int, numChannels: Int, value: Float = 0f) = {
    val arr = Array.ofDim[Float](numChannels, numFrames)
    for (ch <- 0 until numChannels) ju.Arrays.fill(arr(ch), value)
    arr
  }

  implicit val DummyRes = DataSource.Resolver.empty[S]

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
      r2.read(b2, 1, 2)
      for (ch2 <- 0 until 12) {
        val b2c = b2(ch2)
        assert(b2c.to[Vector] === Vector(1f, 0f, 0f, 1f))
      }

      val r1 = _z.reader(1)
      assert(r1.numFrames   ===  3)
      assert(r1.numChannels ===  8)
      val b1 = fillBuf(numFrames = 5, numChannels = 8, value = 1f)
      r1.read(b1, 1, 3)
      for (ch1 <- 0 until 8) {
        val b1c = b1(ch1)
        assert(b1c.to[Vector] === Vector(1f, 0f, 0f, 0f, 1f))
      }

      val r0 = _z.reader(0)
      assert(r0.numFrames   ===  4)
      assert(r0.numChannels ===  6)
      val b0 = fillBuf(numFrames = 6, numChannels = 6, value = 1f)
      r0.read(b0, 1, 4)
      for (ch0 <- 0 until 6) {
        val b0c = b0(ch0)
        assert(b0c.to[Vector] === Vector(1f, 0f, 0f, 0f, 0f, 1f))
      }

      val rm = _z.reader(-1)
      assert(rm.numFrames   ===  1)
      assert(rm.numChannels ===  24)
      val bm = fillBuf(numFrames = 3, numChannels = 24, value = 1f)
      rm.read(bm, 1, 1)
      for (chm <- 0 until 24) {
        val bmc = bm(chm)
        assert(bmc.to[Vector] === Vector(1f, 0f, 1f))
      }
    }
  }
}