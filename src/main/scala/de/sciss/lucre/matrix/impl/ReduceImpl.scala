package de.sciss.lucre
package matrix
package impl

import Reduce.Op
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.matrix.Dimension.Selection
import de.sciss.lucre.event.EventLike
import de.sciss.lucre.matrix.Matrix.Update

object ReduceImpl {
  def apply[S <: Sys[S]](in : Matrix[S], dim: Dimension.Selection[S], op: Op[S])(implicit tx: S#Tx): Reduce[S] =
    new Impl(in, dim, op)

  private final class Impl[S <: Sys[S]](val in: Matrix[S], val dim: Dimension.Selection[S], val op: Op[S])
    extends Reduce[S] with MatrixProxy[S] {

    protected def matrixPeer: Matrix[S] = in

    def changed: EventLike[S, Update[S]] = ???

    override def flatten(implicit tx: S#Tx): Vec[Double] = ???

    override def shape: Expr[S, Vec[Dimension[S]]] = ???

    override def size: Expr[S, Long] = ???

    override def rank: Expr[S, Int] = ???
  }
}
