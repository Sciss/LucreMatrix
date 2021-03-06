/*
 *  Reduce.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2019 by Hanns Holger Rutz.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre
package matrix

import de.sciss.lucre.event.Publisher
import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.matrix.impl.{ReduceImpl => Impl}
import de.sciss.lucre.stm.Elem
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, Serializer}

object Reduce {
  final val opId = 2

  def apply[S <: Sys[S]](in : Matrix[S], dim: Dimension.Selection[S], op: Op[S])(implicit tx: S#Tx): Reduce[S] =
    Impl(in, dim, op)

  def unapply[S <: Sys[S]](m: Matrix[S]): Option[(Matrix[S], Dimension.Selection[S], Op[S])] = m match {
    case r: Reduce[S] => Some((r.in, r.dim, r.op))
    case _ => None
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Reduce[S]] = Impl.serializer[S]

  private[matrix] def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                 (implicit tx: S#Tx): Reduce[S] =
    Impl.readIdentified(in, access, targets)

  object Op extends Elem.Type {
    // ---- Elem.Type ----

    final val typeId = 0x30002

    def readIdentifiedObj[S <: stm.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S] =
      Impl.readIdentifiedOp(in, access)

    // ----

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Op[S] =
      serializer[S].read(in, access)

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Op[S]] = Impl.opSerializer[S]

    object Var {
      def apply[S <: Sys[S]](init: Op[S])(implicit tx: S#Tx): Var[S] = Impl.applyOpVar(init)

      implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Op.Var[S]]= Impl.opVarSerializer[S]
    }
    /** A variable operation, that is a mutable cell storing another operation. */
    trait Var[S <: Sys[S]] extends Op[S] with matrix.Var[S, Op[S]]

    object Apply {
      final val opId = 0

      def apply[S <: Sys[S]](index: IntObj[S])(implicit tx: S#Tx): Apply[S] = Impl.applyOpApply(index)
    }
    /** A single point selection or 'index'. */
    trait Apply[S <: Sys[S]] extends Op[S] with evt.Node[S] {
      def index: IntObj[S]
    }

    object Slice {
      final val opId = 1

      /** Creates a new slice operation.
        *
        * @param from the start index
        * @param to   the end index (inclusive!)
        */
      def apply[S <: Sys[S]](from: IntObj[S], to: IntObj[S])(implicit tx: S#Tx): Slice[S] =
        Impl.applyOpSlice(from, to)
    }
    /** A contiguous range selection.. */
    trait Slice[S <: Sys[S]] extends Op[S] with evt.Node[S] {
      def from : IntObj[S]
      def to   : IntObj[S]
    }

    object Stride {
      final val opId = 3

      def apply[S <: Sys[S]](/* from: IntObj[S], to: IntObj[S], */ step: IntObj[S])(implicit tx: S#Tx): Stride[S] =
        Impl.applyOpStride(/* from = from, to = to, */ step = step)
    }
    /** A range selection with gaps or strides. */
    trait Stride[S <: Sys[S]] extends Op[S] with evt.Node[S] {
      // def from : IntObj[S]
      // def to   : IntObj[S]
      def step : IntObj[S]
    }

    object Average {
      final val opId = 4

      def apply[S <: Sys[S]](implicit tx: S#Tx): Average[S] = Impl.applyOpAverage
    }
    /** A single point selection or 'index'. */
    trait Average[S <: Sys[S]] extends Op[S]

    final case class Update[S <: Sys[S]](op: Op[S])
  }
  sealed trait Op[S <: Sys[S]]
    extends Elem[S] with Publisher[S, Op.Update[S]] {

    final def tpe: Elem.Type = Op

    def size(in: Int)(implicit tx: S#Tx): Int
  }
}
trait Reduce[S <: Sys[S]] extends Matrix[S] {
  def in : Matrix[S]
  def dim: Dimension.Selection[S]
  def op : Reduce.Op[S]

  def indexOfDim(implicit tx: S#Tx): Int
}