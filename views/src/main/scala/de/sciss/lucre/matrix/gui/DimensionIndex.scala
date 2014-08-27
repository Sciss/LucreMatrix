/*
 *  DimensionIndex.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014 by Hanns Holger Rutz.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre.matrix
package gui

import de.sciss.lucre.stm.Disposable
import de.sciss.model.Model
import impl.{DimensionIndexImpl => Impl}

import scala.concurrent.ExecutionContext

object DimensionIndex {
  sealed trait Update
  case object Ready extends Update

  def apply[S <: Sys[S]](dim: Matrix[S])(implicit tx: S#Tx, resolver: DataSource.Resolver[S],
                                         exec: ExecutionContext): DimensionIndex[S] = Impl(dim)

  //  def expr[S <: Sys[S]](dim: Matrix[S], index: Expr[S, Int])(fun: Int => Unit): DimensionIndex[S] =
  //    Impl.expr(dim, index)(fun)
}
trait DimensionIndex[S <: Sys[S]] extends Disposable[S#Tx] with Model[DimensionIndex.Update] {
  def size: Int
  def value(index: Int): Option[Double]
  def format(value: Double): String
}