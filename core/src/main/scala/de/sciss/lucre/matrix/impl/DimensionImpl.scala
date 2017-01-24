/*
 *  DimensionImpl.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2017 by Hanns Holger Rutz.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre
package matrix
package impl

import de.sciss.lucre.event.Targets
import de.sciss.lucre.expr.{Expr, IntObj, StringObj}
import de.sciss.lucre.matrix.Dimension.Selection
import de.sciss.lucre.stm.{Copy, Elem, NoSys}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.annotation.switch

object DimensionImpl {
  def applySelVar[S <: Sys[S]](init: Selection[S])(implicit tx: S#Tx): Selection.Var[S] = {
    val targets = evt.Targets[S]
    val ref     = tx.newVar(targets.id, init)
    new SelVarImpl[S](targets, ref).connect()
  }

  def applySelIndex[S <: Sys[S]](expr: IntObj[S])(implicit tx: S#Tx): Selection.Index[S] = {
    val targets = evt.Targets[S]
    new SelIndexImpl[S](targets, expr).connect()
  }

  def applySelName[S <: Sys[S]](expr: StringObj[S])(implicit tx: S#Tx): Selection.Name[S] = {
    val targets = evt.Targets[S]
    new SelNameImpl[S](targets, expr).connect()
  }

  def selSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Selection[S]] = anySelSer.asInstanceOf[SelSer[S]]
  
  private val anySelSer = new SelSer[NoSys]

  def selVarSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Selection.Var[S]] = anySelVarSer.asInstanceOf[SelVarSer[S]]

  private val anySelVarSer = new SelVarSer[NoSys]

  def readIdentifiedSelection[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Selection[S] = {
    val targets = Targets.read(in, access)

    def readNode()(implicit tx: S#Tx): Selection[S] = {
      val tpe = in.readInt()
      require(tpe == Selection.typeID, s"Unexpected type (found $tpe, expected ${Selection.typeID}")
      val opID = in.readInt()
      (opID: @switch) match {
        case Selection.Index.opID =>
          val ex = IntObj.read(in, access)
          new SelIndexImpl[S](targets, ex)

        case Selection.Name .opID =>
          val ex = StringObj.read(in, access)
          new SelNameImpl[S](targets, ex)

        case _ => sys.error(s"Unknown operation id $opID")
      }
    }

    def readIdentifiedSelVar()(implicit tx: S#Tx): Selection.Var[S] = {
      val ref = tx.readVar[Selection[S]](targets.id, in)
      new SelVarImpl[S](targets, ref)
    }

    (in.readByte(): @switch) match {
      case 0      => readIdentifiedSelVar()
      case 1      => readNode()
      case other  => sys.error(s"Unexpected cookie $other")
    }
  }

  private final class SelSer[S <: Sys[S]] extends stm.impl.ElemSerializer[S, Selection[S]] {
    protected def tpe: Elem.Type = Selection
  }

  private final class SelVarSer[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Selection.Var[S]] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Selection.Var[S] =
      Selection.read(in, access) match {
        case sv: Selection.Var[S] => sv
        case other => sys.error(s"Type mismatch, expected Selection.Var, found $other")
      }

    def write(v: Selection.Var[S], out: DataOutput): Unit = v.write(out)
  }

  // ---- actual implementations ----

  private final class SelVarImpl[S <: Sys[S]](protected val targets: Targets[S],
                                              protected val ref: S#Var[Selection[S]])
    extends Selection.Var[S] with VarImpl[S, Selection.Update[S], Selection[S], Selection.Update[S]] {

//    def mkCopy()(implicit tx: S#Tx): Selection[S] = {
//      val tgt = evt.Targets[S]
//      val peerCpy = tx.newVar(tgt.id, ref().mkCopy())
//      new SelVarImpl[S](tgt, peerCpy)
//    }

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      val targetsOut  = Targets[Out]
      val refOut      = txOut.newVar(targetsOut.id, context(ref()))
      new SelVarImpl[Out](targetsOut, refOut).connect()
    }

    override def toString() = s"Selection.Var$id"

    protected def mapUpdate(in: Selection.Update[S]): Selection.Update[S] = in.copy(selection = this)

    protected def mkUpdate(before: Selection[S], now: Selection[S]): Selection.Update[S] =
      Selection.Update(this)

    // protected def reader: evt.Reader[S, Selection[S]] = Selection.serializer
  }

  private trait SelTuple1Op[S <: Sys[S], T1]
    extends evt.impl.SingleNode[S, Selection.Update[S]] {

    self: Selection[S] =>

    protected def opID: Int
    protected def _1: Expr[S, T1]

    protected def writeData(out: DataOutput): Unit = {
      out writeByte 1   // cookie
      out writeInt Selection.typeID
      out writeInt opID
      _1 write out
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = disconnect()

    // ---- event ----

    // def changed: EventLike[S, Selection.Update[S]] = this
    object changed extends Changed {
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Selection.Update[S]] = {
        val e0 = pull.contains(_1.changed) && pull(_1.changed).isDefined
        if (e0) Some(Selection.Update(self)) else None
      }
    }

    final def connect   ()(implicit tx: S#Tx): this.type = {
      _1.changed ---> changed
      this
    }

    private def disconnect()(implicit tx: S#Tx): Unit = _1.changed -/-> changed
  }

  private final class SelIndexImpl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                                val expr: IntObj[S])
    extends Selection.Index[S] with SelTuple1Op[S, Int] {

//    def mkCopy()(implicit tx: S#Tx): Selection[S] = {
//      val tgt   = evt.Targets[S]
//      val exprCpy = expr match {
//        case IntObj.Var(vr) => IntObj.newVar(vr())
//        case other => other
//      }
//      new SelIndexImpl[S](tgt, exprCpy)
//    }

    def copy[Out <: stm.Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      val targetsOut  = Targets[Out]
      val exprOut     = context(expr)
      new SelIndexImpl(targetsOut, exprOut).connect()
    }

    override def toString() = s"Index$id($expr)"

    protected def _1: IntObj[S] = expr

    protected def opID: Int = Selection.Index.opID
  }

  private final class SelNameImpl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                                val expr: StringObj[S])
    extends Selection.Name[S] with SelTuple1Op[S, String] {

//    def mkCopy()(implicit tx: S#Tx): Selection[S] = {
//      val tgt = evt.Targets[S]
//      val exprCpy = expr match {
//        case StringObj.Var(vr) => StringObj.newVar(vr())
//        case other => other
//      }
//      new SelNameImpl[S](tgt, exprCpy)
//    }

    def copy[Out <: stm.Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      val targetsOut  = Targets[Out]
      val exprOut     = context(expr)
      new SelNameImpl(targetsOut, exprOut).connect()
    }

    override def toString() = s"Name$id($expr)"

    protected def _1: StringObj[S] = expr

    protected def opID: Int = Selection.Name.opID
  }
}
