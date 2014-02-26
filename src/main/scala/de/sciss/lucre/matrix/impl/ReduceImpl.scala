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
import de.sciss.lucre.{event => evt,expr}
import expr.Expr
import evt.EventLike
import Dimension.Selection
import scala.annotation.{switch, tailrec}
import de.sciss.serial.{DataInput, DataOutput}
import de.sciss.lucre.matrix.Reduce.Op.Update

object ReduceImpl {
  final val opID = 1
  
  def apply[S <: Sys[S]](in : Matrix[S], dim: Selection[S], op: Op[S])(implicit tx: S#Tx): Reduce[S] = {
    val targets = evt.Targets[S]
    new Impl[S](targets, in, dim, op)
  }

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Reduce[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[evt.InMemory]

  private final class Ser[S <: Sys[S]] extends evt.EventLikeSerializer[S, Reduce[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Reduce[S] = {
      val tpe     = in.readInt()
      require (tpe == Matrix.typeID, s"Unexpected type id (found $tpe, expected ${Matrix.typeID}")
      val cookie  = in.readInt()
      require (cookie == opID, s"Unexpected operator id (found $cookie, expected $opID)")
      readIdentified[S](in, access, targets)
    }

    def readConstant(in: DataInput)(implicit tx: S#Tx): Reduce[S] =
      sys.error("Unsupported constant reduce matrix")
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Reduce[S] = serializer[S].read(in, access)
  
  private[matrix] def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                         (implicit tx: S#Tx): Reduce[S] = {
    val matrix  = Matrix    .read(in, access)
    val dim     = Selection .read(in, access)
    val op      = Op        .read(in, access)
    new Impl(targets, matrix, dim, op)
  }

  implicit def opSerializer[S <: Sys[S]]: evt.Serializer[S, Op[S]] = anyOpSer.asInstanceOf[OpSer[S]]

  private val anyOpSer = new OpSer[evt.InMemory]

  implicit def opVarSerializer[S <: Sys[S]]: evt.Serializer[S, Op.Var[S]] = anyOpVarSer.asInstanceOf[OpVarSer[S]]

  private val anyOpVarSer = new OpVarSer[evt.InMemory]

  private final class OpSer[S <: Sys[S]] extends evt.EventLikeSerializer[S, Op[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Op[S] with evt.Node[S] = {
      (in.readByte(): @switch) match {
        case 0      => readIdentifiedOpVar(in, access, targets)
        case 1      => readNode(in, access, targets)
        case other  => sys.error(s"Unsupported cookie $other")
      }
    }

    private def readNode(in: DataInput, access: S#Acc, targets: evt.Targets[S])
                        (implicit tx: S#Tx): Op[S] with evt.Node[S] = {
      val tpe   = in.readInt()
      require(tpe == Op.typeID, s"Unexpected type id (found $tpe, expected ${Op.typeID})")
      val opID  = in.readInt()
      (opID: @switch) match {
        case Op.Apply.opID =>
          val index = Ints.readExpr(in, access)
          new OpApplyImpl[S](targets, index)

        case Op.Slice.opID =>
          val from  = Ints.readExpr(in, access)
          val until = Ints.readExpr(in, access)
          new OpSliceImpl[S](targets, from, until)

        case _ => sys.error(s"Unsupported operator id $opID")
      }
    }

    def readConstant(in: DataInput)(implicit tx: S#Tx): Op[S] = sys.error("Unknown constant op")
  }

  private final class OpVarSer[S <: Sys[S]] extends evt.EventLikeSerializer[S, Op.Var[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Op.Var[S] = {
      val cookie = in.readByte()
      require(cookie == 0, s"Unexpected cookie (found $cookie, expected 0)")
      readIdentifiedOpVar(in, access, targets)
    }

    def readConstant(in: DataInput)(implicit tx: S#Tx): Op.Var[S] =
      sys.error("Unsupported constant op variable")
  }

  private def readIdentifiedOpVar[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                              (implicit tx: S#Tx): Op.Var[S] = {
    val ref = tx.readVar[Op[S]](targets.id, in)
    new OpVarImpl[S](targets, ref)
  }

  def applyOpVar[S <: Sys[S]](init: Op[S])(implicit tx: S#Tx): Reduce.Op.Var[S] = {
    val targets = evt.Targets[S]
    val ref     = tx.newVar(targets.id, init)
    new OpVarImpl[S](targets, ref)
  }

  def applyOpApply[S <: Sys[S]](index: Expr[S, Int])(implicit tx: S#Tx): Op.Apply[S] = {
    val targets = evt.Targets[S]
    new OpApplyImpl[S](targets, index)
  }

  def applyOpSlice[S <: Sys[S]](from: Expr[S, Int], until: Expr[S, Int])(implicit tx: S#Tx): Op.Slice[S] = {
    val targets = evt.Targets[S]
    new OpSliceImpl[S](targets, from = from, until = until)
  }

  // ---- actual implementations ----

  private final class OpVarImpl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                             protected val ref: S#Var[Op[S]])
    extends Op.Var[S] with VarImpl[S, Op[S], Op.Update[S]] {

    protected def mapUpdate(in: Update[S]): Op.Update[S] = in.copy(op = this)

    protected def mkUpdate(v: Op[S]): Op.Update[S] = Op.Update(this)

    protected def reader: evt.Reader[S, Op[S]] = Op.serializer
  }

  private final class OpApplyImpl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                               val index: Expr[S, Int])
    extends Op.Apply[S] with evt.impl.StandaloneLike[S, Op.Update[S], Op[S]] {

    override def toString() = s"Apply$id($index)"

    protected def writeData(out: DataOutput): Unit = {
      out writeInt Op.typeID
      out writeInt Op.Apply.opID
      index write out
    }

    protected def disposeData()(implicit tx: S#Tx) = ()

    // ---- event ----

    def changed: EventLike[S, Op.Update[S]] = this

    protected def reader: evt.Reader[S, Op[S]] = Op.serializer

    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Update[S]] =
      pull(index.changed).map(_ => Op.Update(this))

    def connect   ()(implicit tx: S#Tx): Unit = index.changed ---> this
    def disconnect()(implicit tx: S#Tx): Unit = index.changed -/-> this
  }

  private final class OpSliceImpl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                               val from: Expr[S, Int], val until: Expr[S, Int])
    extends Op.Slice[S] with evt.impl.StandaloneLike[S, Op.Update[S], Op[S]] {

    override def toString() = s"Slice$id($from, $until)"

    protected def writeData(out: DataOutput): Unit = {
      out writeInt Op.typeID
      out writeInt Op.Slice.opID
      from  write out
      until write out
    }

    protected def disposeData()(implicit tx: S#Tx) = ()

    // ---- event ----

    def changed: EventLike[S, Op.Update[S]] = this

    protected def reader: evt.Reader[S, Op[S]] = Op.serializer

    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Op.Update[S]] = {
      val e0 =       pull.contains(from .changed) && pull(from .changed).isDefined
      val e1 = e0 || pull.contains(until.changed) && pull(until.changed).isDefined

      if (e1) Some(Op.Update(this)) else None
    }

    def connect()(implicit tx: S#Tx): Unit = {
      from .changed ---> this
      until.changed ---> this
    }

    def disconnect()(implicit tx: S#Tx): Unit = {
      from .changed -/-> this
      until.changed -/-> this
    }
  }

  private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S], val in: Matrix[S],
                                        val dim: Selection[S], val op: Op[S])
    extends Reduce[S]
    with MatrixProxy[S]
    with evt.impl.StandaloneLike[S, Matrix.Update[S], Matrix[S]] {

    override def toString() = s"Reduce$id($in, $dim, $op)"

    protected def matrixPeer(implicit tx: S#Tx): Matrix[S] = in

    override def flatten(implicit tx: S#Tx): Vec[Double] = ???

    override def shape(implicit tx: S#Tx): Vec[Int] = {
      val sh = in.shape
      val (idx, sz) = indexAndSize
      if (sz == 0) Vec.empty  // or throw exception?
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

    protected def reader: evt.Reader[S, Matrix[S]] = Matrix.serializer

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