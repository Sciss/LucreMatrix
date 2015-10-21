/*
 *  Dimension.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 by Hanns Holger Rutz.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre
package matrix

import de.sciss.lucre.expr.{IntObj, StringObj}
import de.sciss.lucre.matrix.impl.{DimensionImpl => Impl}
import de.sciss.lucre.stm.Elem
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, Serializer}

object Dimension {
  object Selection extends Elem.Type {
    // ---- Elem.Type ----

    final val typeID = 0x30003

    def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S] =
      Impl.readIdentifiedSelection(in, access)

    // ----

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Selection[S] =
      serializer[S].read(in, access)

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Selection[S]] = Impl.selSerializer[S]

    object Index {
      final val opID  = 0

      def apply[S <: Sys[S]](expr: IntObj[S])(implicit tx: S#Tx): Index[S] = Impl.applySelIndex(expr)
    }
    trait Index[S <: Sys[S]] extends Selection[S] with evt.Node[S] {
      def expr: IntObj[S]
    }

    object Name {
      final val opID  = 1

      def apply[S <: Sys[S]](expr: StringObj[S])(implicit tx: S#Tx): Name[S] = Impl.applySelName(expr)
    }
    trait Name[S <: Sys[S]] extends Selection[S] with evt.Node[S] {
      def expr: StringObj[S]
    }

    object Var {
      def apply[S <: Sys[S]](init: Selection[S])(implicit tx: S#Tx): Var[S] = Impl.applySelVar(init)

      implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Selection.Var[S]] = Impl.selVarSerializer[S]
    }
    trait Var[S <: Sys[S]] extends Selection[S] with matrix.Var[S, Selection[S]]

    case class Update[S <: Sys[S]](selection: Selection[S])
  }
  sealed trait Selection[S <: Sys[S]]
    extends Elem[S] with evt.Publisher[S, Selection.Update[S]] {

    final def tpe: Elem.Type = Selection
  }

  case class Value(name: String, size: Int)
}