/*
 *  Dimension.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre
package matrix

import de.sciss.lucre.{event => evt}
import de.sciss.lucre.expr.Expr
import de.sciss.serial.{DataInput, Writable}
import de.sciss.lucre.stm.Disposable
import impl.{DimensionImpl => Impl}

object Dimension {
  // trait Var[S <: Sys[S]] extends Dimension[S] with matrix.Var[S, Dimension[S]]

  object Selection {
    final val typeID = 0x30003

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Selection[S] =
      serializer[S].read(in, access)

    implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Selection[S]] = Impl.selSerializer[S]

    object Index {
      final val opID  = 0

      def apply[S <: Sys[S]](expr: Expr[S, Int])(implicit tx: S#Tx): Index[S] = Impl.applySelIndex(expr)
    }
    trait Index[S <: Sys[S]] extends Selection[S] with evt.Node[S] {
      def expr: Expr[S, Int]
    }

    object Name {
      final val opID  = 1

      def apply[S <: Sys[S]](expr: Expr[S, String])(implicit tx: S#Tx): Name[S] = Impl.applySelName(expr)
    }
    trait Name[S <: Sys[S]] extends Selection[S] with evt.Node[S] {
      def expr: Expr[S, String]
    }

    object Var {
      def apply[S <: Sys[S]](init: Selection[S])(implicit tx: S#Tx): Var[S] = Impl.applySelVar(init)

      implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Selection.Var[S]] = Impl.selVarSerializer[S]
    }
    trait Var[S <: Sys[S]] extends Selection[S] with matrix.Var[S, Selection[S]]

    case class Update[S <: Sys[S]](selection: Selection[S])
  }
  sealed trait Selection[S <: Sys[S]]
    extends Writable with Disposable[S#Tx] with evt.Publisher[S, Selection.Update[S]]

  case class Value(name: String, size: Int)

  //  trait Value {
  //    def name: String
  //    def size: Int
  //  }
}
//trait Dimension[S <: Sys[S]] extends Expr[S, Dimension.Value] {
//  def name: Expr[S, String]
//  def size: Expr[S, Int   ]
//
//  // def flatten(implicit tx: S#Tx): Vec[Double]
//}
