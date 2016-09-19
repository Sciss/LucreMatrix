/*
 *  DimensionIndex.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 by Hanns Holger Rutz.
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
import scala.util.Try

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

  /** Tries to format the value at the given `index`. If the value is available,
    * returns the unit-formatted representation. If the value is pending, returns
    * `None`. If the values cannot be read due to an IO error, returns
    * the exception message.
    */
  def tryFormat(index: Int): Option[Try[String]]
}