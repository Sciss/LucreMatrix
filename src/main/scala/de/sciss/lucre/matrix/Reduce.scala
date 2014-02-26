package de.sciss.lucre
package matrix

import de.sciss.lucre.expr.Expr
import de.sciss.lucre.{event => evt}
import evt.Publisher
import de.sciss.serial.{DataInput, Writable}
import de.sciss.lucre.stm.Disposable
import impl.{ReduceImpl => Impl}

object Reduce {
  final val opID = 1

  def apply[S <: Sys[S]](in : Matrix[S], dim: Dimension.Selection[S], op: Op[S])(implicit tx: S#Tx): Reduce[S] =
    Impl(in, dim, op)

  // def unapply[S <: Sys[S]](m: Matrix[S]): Option[Reduce[S]]

  private[matrix] def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                 (implicit tx: S#Tx): Reduce[S] =
    Impl.readIdentified(in, access, targets)

  object Op {
    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Op[S] = ???

    object Var {
      def apply[S <: Sys[S]](init: Op[S])(implicit tx: S#Tx): Var[S] = ???
    }
    trait Var[S <: Sys[S]] extends Op[S] with matrix.Var[S, Op[S]]

    // case class Mean[S <: Sys[S]]() extends Op[S]
    object Apply {
      def apply[S <: Sys[S]](index: Expr[S, Int])(implicit tx: S#Tx): Apply[S] = ???
    }
    trait Apply[S <: Sys[S]] extends Op[S] {
      def index: Expr[S, Int]
    }
    object Slice {
      def apply[S <: Sys[S]](from: Expr[S, Int], until: Expr[S, Int])(implicit tx: S#Tx): Slice[S] = ???
    }
    trait Slice[S <: Sys[S]] extends Op[S] {
      def from : Expr[S, Int]
      def until: Expr[S, Int]
    }

    trait Update[S <: Sys[S]] {
      def op: Op[S]
    }
  }
  sealed trait Op[S <: Sys[S]]
    extends Writable with Disposable[S#Tx] with Publisher[S, Op.Update[S]]
}
trait Reduce[S <: Sys[S]] extends Matrix[S] with evt.Node[S] {
  def in : Matrix[S]
  def dim: Dimension.Selection[S]
  def op : Reduce.Op[S]
}
