/*
 *  Dimension.scala
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

package de.sciss.lucre
package matrix

import de.sciss.lucre.event.Publisher
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm

object Dimension {
  trait Var[S <: Sys[S]] extends Dimension[S] with matrix.Var[S, Dimension[S]]

  object Selection {
    trait Index[S <: Sys[S]] extends Selection[S] {
      def expr: Expr[S, Int]
    }

    trait Name[S <: Sys[S]] extends Selection[S] {
      def expr: Expr[S, String]
    }

    trait Var[S <: Sys[S]] extends Selection[S] with matrix.Var[S, Selection[S]]

    trait Update[S <: Sys[S]]
  }
  sealed trait Selection[S <: Sys[S]] extends Publisher[S, Selection.Update[S]]

  trait Update[S <: Sys[S]]
}
trait Dimension[S <: Sys[S]] extends Publisher[S, Dimension.Update[S]] {
  def name: Expr[S, String]
  def size: Expr[S, Int]

  def flatten(implicit tx: S#Tx): Vec[Double]
}
