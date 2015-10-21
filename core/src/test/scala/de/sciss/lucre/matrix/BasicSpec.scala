package de.sciss.lucre.matrix

import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.matrix.Implicits._
import de.sciss.lucre.stm.Durable
import de.sciss.lucre.stm.store.BerkeleyDB
import org.scalatest.{Matchers, Outcome, fixture}

import scala.language.implicitConversions

/*
  to run only this test:
  test-only de.sciss.lucre.matrix.BasicSpec
 */
class BasicSpec extends fixture.FlatSpec with Matchers {
  type S            = Durable
  type FixtureParam = Durable

  initTypes()

  def withFixture(test: OneArgTest): Outcome = {
    val system = Durable(BerkeleyDB.tmp())
    try {
      test(system)
    }
    finally {
      system.close()
    }
  }

  implicit def mkConst(i: Int)(implicit tx: S#Tx): IntObj.Const[S] = IntObj.newConst[S](i)

  "A Zeros Matrix" should "sing while you sell" in { cursor =>
    val z = cursor.step { implicit tx =>
      val _z = Matrix.zeros(13, 21)
      assert(_z.rank         === 2)
      assert(_z.reducedRank  === 2)
      assert(_z.shape        === Vec(13, 21))
      assert(_z.reducedShape === Vec(13, 21))
      tx.newHandle(_z)
    }

    val m1 = cursor.step { implicit tx =>
      val _m1 = z().slice(0, 10)
      assert(_m1.rank         === 2)
      assert(_m1.reducedRank  === 1)
      assert(_m1.shape        === Vec( 1, 21))
      assert(_m1.reducedShape === Vec    (21))
      tx.newHandle(_m1)
    }

    cursor.step { implicit tx =>
      z ().dispose()
      m1().dispose()
    }
  }

  "Matrix Reductions" should "be observable" in { cursor =>
    val (mv, si, sv, oi, ov) = cursor.step { implicit tx =>
      val _z  = Matrix.zeros(13, 21)
      val _mv = Matrix.Var(_z)
      val _si = IntObj.newVar[S](0)
      val _sv = Dimension.Selection.Var(Dimension.Selection.Index(_si))
      val _oi = IntObj.newVar[S](0)
      val _ov = Reduce.Op.Var(Reduce.Op.Apply(_oi))
      (tx.newHandle(_mv), tx.newHandle(_si), tx.newHandle(_sv), tx.newHandle(_oi), tx.newHandle(_ov))
    }

    val m2 = cursor.step { implicit tx =>
      val _m2 = Reduce(mv(), sv(), ov())
      tx.newHandle(_m2)
    }

    cursor.step { implicit tx =>
      m2().dispose()
    }
  }

  ignore /* "Matrix Reductions" */ should "yield correct cell data" in { cursor =>
    cursor.step { implicit tx =>
      val m0 = Matrix.newConst3D("M", Vec(
        Vec(
          Vec( 1,  2 , 3,  4),
          Vec( 5,  6,  7,  8),
          Vec( 9, 10, 11, 12)
        ),
        Vec(
          Vec(13, 14, 15, 16),
          Vec(17, 18, 19, 20),
          Vec(21, 22, 23, 24)
        )
      ))

      //      val b0 = Array.ofDim[Float](24, 1)
      //      m0.reader(-1).read(b0, 0, 24)
      //      assert (b0.flatten.toVector === (1 to 24))
      assert (m0.debugFlatten === (1 to 24))

      val si  = IntObj.newVar[S](0)
      val sv  = Dimension.Selection.Var(Dimension.Selection.Index(si))
      val oi  = IntObj.newVar[S](0)
      val ov  = Reduce.Op.Var(Reduce.Op.Apply(oi))
      val m1  = Reduce(m0, sv, ov)

      assert (m1.debugFlatten === ( 1 to 12))

      oi() = 1
      assert (m1.debugFlatten === (13 to 24))

      si() = 1
      oi() = 0
      assert (m1.debugFlatten === (1 to  4) ++ (13 to 16))
      oi() = 1
      assert (m1.debugFlatten === (5 to  8) ++ (17 to 20))
      oi() = 2
      assert (m1.debugFlatten === (9 to 12) ++ (21 to 24))

      si() = 2
      oi() = 0
      assert (m1.debugFlatten === Vec(1, 5,  9, 13, 17, 21))
      oi() = 1
      assert (m1.debugFlatten === Vec(2, 6, 10, 14, 18, 22))
      oi() = 2
      assert (m1.debugFlatten === Vec(3, 7, 11, 15, 19, 23))
      oi() = 3
      assert (m1.debugFlatten === Vec(4, 8, 12, 16, 20, 24))
    }
  }
}
