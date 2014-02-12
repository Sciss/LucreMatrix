/*
 *  ZeroMatrixImpl.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre.matrix
package impl

import de.sciss.lucre.expr.Expr
import de.sciss.lucre.{event => evt}
import evt.EventLike
import de.sciss.lucre.matrix.Matrix.Update
import de.sciss.lucre.synth.expr.ExprImplicits

object ZeroMatrixImpl {
  def apply[S <: Sys[S]](shape: Vec[Int])(implicit tx: S#Tx): Matrix[S] = {
    val vec: Vec[Dimension[S]] = shape.zipWithIndex.map { case (sz, idx) =>
      new RangeDimensionImpl[S](s"d$idx", 0 until sz)
    }
    val nameEx  = Strings.newConst[S](s"zeros${shape.mkString("[","][","]")}")
    val shapeEx = DimensionVecType.newConst[S](vec)
    //    val sizeEx: Expr[S, Long] = {
    //      val identity: Expr[S, Long] = Longs.newConst(1L)
    //      val imp = ExprImplicits[S]
    //      import imp._
    //      (identity /: shape.value)(_ * _.size)
    //    }
    val sizeEx  = Longs.newConst[S]((0L /: shape)(_ * _))
    val rankEx  = Ints .newConst[S](shape.size)
    new Impl(nameEx, shape = shapeEx, size = sizeEx, rank = rankEx)
  }

  private final class Impl[S <: Sys[S]](val name: Expr[S, String], val shape: Expr[S, Vec[Dimension[S]]],
                                        val size: Expr[S, Long], val rank: Expr[S, Int])
    extends Matrix[S] {

    def changed: EventLike[S, Update[S]] = evt.Dummy.apply

    def flatten(implicit tx: S#Tx): Vec[Double] = {
      val sz = size.value
      require(sz <= 0x7FFFFFFF)
      val szI = sz.toInt
      Vec.fill(szI)(0.0)
    }
  }
}