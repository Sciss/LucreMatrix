package de.sciss.lucre
package matrix

import de.sciss.lucre.expr.Expr
import de.sciss.lucre.{event => evt}
import evt.Publisher
import de.sciss.serial.{DataInput, Writable, Serializer}
import de.sciss.lucre.stm.Disposable
import impl.{ReduceImpl => Impl}

object Reduce {
  final val opID = 1

  def apply[S <: Sys[S]](in : Matrix[S], dim: Dimension.Selection[S], op: Op[S])(implicit tx: S#Tx): Reduce[S] =
    Impl(in, dim, op)

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Reduce[S]] = Impl.serializer

  private[matrix] def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                 (implicit tx: S#Tx): Reduce[S] =
    Impl.readIdentified(in, access, targets)

  object Op {
    final val typeID = 0x30002

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Op[S] =
      serializer[S].read(in, access)

    implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Op[S]] = Impl.opSerializer

    object Var {
      def apply[S <: Sys[S]](init: Op[S])(implicit tx: S#Tx): Var[S] = Impl.applyOpVar(init)

      implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Var[S]]= Impl.opVarSerializer
    }
    trait Var[S <: Sys[S]] extends Op[S] with matrix.Var[S, Op[S]]

    object Apply {
      final val opID = 0

      def apply[S <: Sys[S]](index: Expr[S, Int])(implicit tx: S#Tx): Apply[S] = Impl.applyOpApply(index)
    }
    trait Apply[S <: Sys[S]] extends Op[S] with evt.Node[S] {
      def index: Expr[S, Int]
    }
    object Slice {
      final val opID = 1

      def apply[S <: Sys[S]](from: Expr[S, Int], until: Expr[S, Int])(implicit tx: S#Tx): Slice[S] =
        Impl.applyOpSlice(from, until)
    }
    trait Slice[S <: Sys[S]] extends Op[S] with evt.Node[S] {
      def from : Expr[S, Int]
      def until: Expr[S, Int]
    }
    // case class Mean[S <: Sys[S]]() extends Op[S]

    case class Update[S <: Sys[S]](op: Op[S])
  }
  sealed trait Op[S <: Sys[S]]
    extends Writable with Disposable[S#Tx] with Publisher[S, Op.Update[S]]
}
trait Reduce[S <: Sys[S]] extends Matrix[S] with evt.Node[S] {
  def in : Matrix[S]
  def dim: Dimension.Selection[S]
  def op : Reduce.Op[S]
}
