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
import de.sciss.serial.{DataInput, Writable}
import de.sciss.lucre.stm.Disposable

object Dimension {
  trait Var[S <: Sys[S]] extends Dimension[S] with matrix.Var[S, Dimension[S]]

  object Selection {
    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Selection[S] = ???

    object Index {
      def apply[S <: Sys[S]](expr: Expr[S, Int])(implicit tx: S#Tx): Index[S] = ???
    }
    trait Index[S <: Sys[S]] extends Selection[S] {
      def expr: Expr[S, Int]
    }

    object Name {
      def apply[S <: Sys[S]](expr: Expr[S, String])(implicit tx: S#Tx): Name[S] = ???
    }
    trait Name[S <: Sys[S]] extends Selection[S] {
      def expr: Expr[S, String]
    }

    object Var {
      def apply[S <: Sys[S]](init: Selection[S])(implicit tx: S#Tx): Var[S] = ???
    }
    trait Var[S <: Sys[S]] extends Selection[S] with matrix.Var[S, Selection[S]]

    trait Update[S <: Sys[S]]
  }
  sealed trait Selection[S <: Sys[S]]
    extends Writable with Disposable[S#Tx] with Publisher[S, Selection.Update[S]]

  case class Value(name: String, size: Int)
}
trait Dimension[S <: Sys[S]] extends Expr[S, Dimension.Value] {
  def name: Expr[S, String]
  def size: Expr[S, Int   ]

  def flatten(implicit tx: S#Tx): Vec[Double]
}
