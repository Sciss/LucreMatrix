package de.sciss.lucre
package matrix
package impl

import Reduce.Op
import de.sciss.lucre.{expr, event => evt}
import expr.Expr
import evt.EventLike
import de.sciss.lucre.matrix.Matrix.Update
import Dimension.Selection
import scala.annotation.tailrec
import de.sciss.serial.DataOutput
import de.sciss.model.Change

object ReduceImpl {
  def apply[S <: Sys[S]](in : Matrix[S], dim: Selection[S], op: Op[S])(implicit tx: S#Tx): Reduce[S] = {
    new Impl[S](in, dim, op) {
      reduce =>

      protected val targets = evt.Targets[S]

      override object size extends expr.impl.NodeImpl[S, Long] {
        final val opID = 0x2000

        def value(implicit tx: S#Tx): Long = {
          val sh = in.shape.value

          @tailrec def indexOfDim(sel: Selection[S]): Int = sel match {
            case si: Selection.Index[S] => si.expr.value
            case sn: Selection.Name [S] => sh.indexWhere(_.name.value == sn.expr.value)
            case sv: Selection.Var  [S] => indexOfDim(sv())
          }

          val idx   = indexOfDim(dim)
          val valid = idx >= 0 && idx < sh.size

          if (valid) {

            ???
          } else {
            in.size.value   // or throw exception?
          }
        }

        protected def writeData(out: DataOutput): Unit = {
          out.writeByte(1)            // unary-op
          out.writeInt(Longs.typeID)  // ...evaluating to Long
          out.writeInt(opID)
          reduce.write(out)
        }

        protected def targets: evt.Targets[S] = ???

        protected def reader: evt.Reader[S, Expr[S, Long]] = Longs.serializer

        def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Change[Long]] = ???

        def connect()(implicit tx: S#Tx): Unit = {
          in .changed ---> this
          dim.changed ---> this
          op .changed ---> this
        }

        def disconnect()(implicit tx: S#Tx): Unit = ???
      }
    }
  }

  private abstract class Impl[S <: Sys[S]](val in: Matrix[S], val dim: Selection[S], val op: Op[S])
    extends Reduce[S] with MatrixProxy[S] with evt.impl.StandaloneLike[S, Update[S], Reduce[S]] {

    protected def matrixPeer: Matrix[S] = in

    def changed: EventLike[S, Update[S]] = ???

    override def flatten(implicit tx: S#Tx): Vec[Double] = ???

    override def shape: Expr[S, Vec[Dimension[S]]] = ???

    override def rank: Expr[S, Int] = ???

    protected def disposeData()(implicit tx: S#Tx): Unit = ???

    protected def writeData(out: DataOutput): Unit = ???

    protected def reader: evt.Reader[S, Reduce[S]] = ???

    def connect   ()(implicit tx: S#Tx): Unit = ???
    def disconnect()(implicit tx: S#Tx): Unit = ???

    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Update[S]] = ???
  }
}