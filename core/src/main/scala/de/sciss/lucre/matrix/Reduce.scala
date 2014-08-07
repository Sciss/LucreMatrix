package de.sciss.lucre
package matrix

import de.sciss.lucre.expr.Expr
import de.sciss.lucre.{event => evt}
import evt.Publisher
import de.sciss.serial.{DataInput, Writable, Serializer}
import de.sciss.lucre.stm.Disposable
import impl.{ReduceImpl => Impl}

object Reduce {
  final val opID = 2

  def apply[S <: Sys[S]](in : Matrix[S], dim: Dimension.Selection[S], op: Op[S])(implicit tx: S#Tx): Reduce[S] =
    Impl(in, dim, op)

  def unapply[S <: Sys[S]](m: Matrix[S]): Option[(Matrix[S], Dimension.Selection[S], Op[S])] = m match {
    case r: Reduce[S] => Some((r.in, r.dim, r.op))
    case _ => None
  }

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Reduce[S]] = Impl.serializer[S]

  private[matrix] def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                 (implicit tx: S#Tx): Reduce[S] =
    Impl.readIdentified(in, access, targets)

  object Op {
    final val typeID = 0x30002

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Op[S] =
      serializer[S].read(in, access)

    implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Op[S]] = Impl.opSerializer[S]

    object Var {
      def apply[S <: Sys[S]](init: Op[S])(implicit tx: S#Tx): Var[S] = Impl.applyOpVar(init)

      implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Var[S]]= Impl.opVarSerializer[S]
    }
    /** A variable operation, that is a mutable cell storing another operation. */
    trait Var[S <: Sys[S]] extends Op[S] with matrix.Var[S, Op[S]]

    object Apply {
      final val opID = 0

      def apply[S <: Sys[S]](index: Expr[S, Int])(implicit tx: S#Tx): Apply[S] = Impl.applyOpApply(index)
    }
    /** A single point selection or 'index'. */
    trait Apply[S <: Sys[S]] extends Op[S] with evt.Node[S] {
      def index: Expr[S, Int]
    }

    object Slice {
      final val opID = 1

      def apply[S <: Sys[S]](from: Expr[S, Int], to: Expr[S, Int])(implicit tx: S#Tx): Slice[S] =
        Impl.applyOpSlice(from, to)
    }
    /** A contiguous range selection.. */
    trait Slice[S <: Sys[S]] extends Op[S] with evt.Node[S] {
      def from : Expr[S, Int]
      def to   : Expr[S, Int]
    }

    object Stride {
      final val opID = 2

      def apply[S <: Sys[S]](from: Expr[S, Int], to: Expr[S, Int], step: Expr[S, Int])(implicit tx: S#Tx): Stride[S] =
        ??? // Impl.applyOpSlice(from, to)
    }
    /** A range selection with gaps or strides. */
    trait Stride[S <: Sys[S]] extends Op[S] with evt.Node[S] {
      def from : Expr[S, Int]
      def to   : Expr[S, Int]
      def step : Expr[S, Int]
    }

    case class Update[S <: Sys[S]](op: Op[S])
  }
  sealed trait Op[S <: Sys[S]]
    extends Writable with Disposable[S#Tx] with Publisher[S, Op.Update[S]] {

    def size(in: Int)(implicit tx: S#Tx): Int

    def map(r: Matrix.Reader, shape: Vec[Int], redDim: Int, streamDim: Int)(implicit tx: S#Tx): Matrix.Reader = ???
  }
}
trait Reduce[S <: Sys[S]] extends Matrix[S] with evt.Node[S] {
  def in : Matrix[S]
  def dim: Dimension.Selection[S]
  def op : Reduce.Op[S]

  def indexOfDim(implicit tx: S#Tx): Int
}