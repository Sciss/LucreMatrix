/*
 *  Matrix.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.event.Publisher
import de.sciss.lucre.expr.Expr

object Matrix {
  trait Var[S <: Sys[S]] extends Matrix[S] with stm.Sink[S, Matrix[S]] with stm.Source[S, Matrix[S]]

  trait Update[S <: Sys[S]]
  // ...
}
trait Matrix[S <: Sys[S]] extends Publisher[S, Matrix.Update[S]] {
  def name: Expr[S, String]

  def rank: Expr[S, Int]
  def size: Expr[S, Long]

  def shape: Expr[S, Vec[Dimension[S]]]   // ...or use a mutable collection?

  def flatten(implicit tx: S#Tx): Vec[Double]

  // def read(...) = ...

  // def view[I <: Sys[S]](implicit bridge: S#Tx => I#Tx): Matrix[I]
}
