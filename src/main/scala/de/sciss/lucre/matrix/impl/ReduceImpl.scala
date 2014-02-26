/*
 *  ReduceImpl.scala
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
package impl

import Reduce.Op
import de.sciss.lucre.{expr, event => evt}
import evt.EventLike
import Dimension.Selection
import scala.annotation.tailrec
import de.sciss.serial.{DataInput, DataOutput}

object ReduceImpl {
  final val opID = 1
  
  def apply[S <: Sys[S]](in : Matrix[S], dim: Selection[S], op: Op[S])(implicit tx: S#Tx): Reduce[S] = {
    val targets = evt.Targets[S]
    new Impl[S](targets, in, dim, op)
  }
  
  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Reduce[S] = {
    val targets = evt.Targets.read(in, access)
    val tpe     = in.readInt()
    require (tpe == Matrix.typeID, s"Unexpected type id (found $tpe, expected ${Matrix.typeID}")
    val cookie  = in.readInt()
    require (cookie == opID, s"Unexpected operator id (found $cookie, expected $opID)")
    readIdentified[S](in, access, targets)
  }
  
  private[matrix] def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                         (implicit tx: S#Tx): Reduce[S] = {
    val matrix  = Matrix    .read(in, access)
    val dim     = Selection .read(in, access)
    val op      = Op        .read(in, access)
    new Impl(targets, matrix, dim, op)
  }

  private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S], val in: Matrix[S],
                                           val dim: Selection[S], val op: Op[S])
    extends Reduce[S]
    with MatrixProxy[S]
    with evt.impl.StandaloneLike[S, Matrix.Update[S], Reduce[S]] {

    protected def matrixPeer: Matrix[S] = in

    override def flatten(implicit tx: S#Tx): Vec[Double] = ???

    override def shape(implicit tx: S#Tx): Vec[Int] = {
      val sh = in.shape
      val (idx, sz) = indexAndSize
      if (sz == 0) Vec.empty // or  throw exception?
      else sh.updated(idx, sz)
    }

    private def indexAndSize(implicit tx: S#Tx): (Int, Int) = {
      @tailrec def indexOfDim(sel: Selection[S]): Int = sel match {
        case si: Selection.Index[S] => si.expr.value
        case sn: Selection.Name [S] => in.dimensions.indexWhere(_.name == sn.expr.value)
        case sv: Selection.Var  [S] => indexOfDim(sv())
      }

      val idx   = indexOfDim(dim)
      val valid = idx >= 0 && idx < in.rank
      if (!valid) return (-1, -1)   // or throw exception?

      @tailrec def sizeOfDim(_op: Op[S]): Int = _op match {
        case oa: Op.Apply[S] => 1
        case os: Op.Slice[S] =>
          val lo  = math.max(0, os.from .value)
          val hi  = math.min(in.shape.apply(idx), math.max(lo + 1, os.until.value))
          hi - lo

        case ov: Op.Var  [S] => sizeOfDim(ov())
      }

      val sz = sizeOfDim(op)
      (idx, sz)
    }

    protected def writeData(out: DataOutput): Unit = {
      out writeInt Matrix.typeID
      out writeInt opID
      in  write out
      dim write out
      op  write out
    }

    protected def disposeData()(implicit tx: S#Tx) = ()

    // ---- event ----

    def changed: EventLike[S, Matrix.Update[S]] = this

    protected def reader: evt.Reader[S, Reduce[S]] = ???

    def connect()(implicit tx: S#Tx): Unit = {
      in .changed ---> this
      dim.changed ---> this
      op .changed ---> this
    }

    def disconnect()(implicit tx: S#Tx): Unit = {
      in .changed -/-> this
      dim.changed -/-> this
      op .changed -/-> this
    }

    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Matrix.Update[S]] = {
      val e0 =       pull.contains(in .changed) && pull(in .changed).isDefined
      val e1 = e0 || pull.contains(dim.changed) && pull(dim.changed).isDefined
      val e2 = e1 || pull.contains(op .changed) && pull(op .changed).isDefined
      if (e2) Some(Matrix.Update(this)) else None
    }
  }
}