/*
 *  DimensionView.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 by Hanns Holger Rutz.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre.matrix
package gui
package impl

import de.sciss.desktop.UndoManager
import de.sciss.lucre.expr.{IntObj, StringObj}
import de.sciss.lucre.stm
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{View, deferTx}
import de.sciss.swingplus.PopupMenu

import scala.swing.{Action, BoxPanel, Button, Component, Label, MenuItem, Orientation}

object DimensionView {
  def apply[S <: Sys[S]](varOpt: Option[Matrix.Var[S]], name: String)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], undo: UndoManager): DimensionView[S] = {
    val red         = ReductionsView[S](name)
    val varOptH     = varOpt.map(tx.newHandle(_))
    val res         = new Impl[S](varOptH, name, red)
    val redIsEmpty  = red.isEmpty
    val redIsLeaf   = red.isLeaf
    deferTx(res.guiInit(redIsEmpty0 = redIsEmpty, redIsLeaf0 = redIsLeaf))
    res
  }

  private final class Impl[S <: Sys[S]](varOptH: Option[stm.Source[S#Tx, Matrix.Var[S]]], name: String,
                                        val reductions: ReductionsView[S])
                                       (implicit cursor: stm.Cursor[S], undo: UndoManager)
    extends DimensionView[S] with ComponentHolder[Component] {

    private def performAdd(opV: ReduceOpEnum): Unit = varOptH.foreach { varH =>
      val edit = cursor.step { implicit tx =>
        val vr    = varH()
        val prev  = vr()
        val dim   = Dimension.Selection.Name[S](StringObj.newConst(name))
        val op    = opV match {
          case ReduceOpEnum.Apply =>
            Reduce.Op.Apply[S](IntObj.newVar(IntObj.newConst(0)))
          case ReduceOpEnum.Slice =>
            val toVal = prev.dimensions.find(_.name == name).map(_.size.toInt - 1).getOrElse(0)
            Reduce.Op.Slice[S](
              from = IntObj.newVar(IntObj.newConst(0)),
              to   = IntObj.newVar(IntObj.newConst(toVal /* Int.MaxValue - 1 */))
            )
          case ReduceOpEnum.Stride =>
            Reduce.Op.Stride[S](
              // from = IntObj.newVar(IntObj.newConst(0)),
              // to   = IntObj.newVar(IntObj.newConst(Int.MaxValue - 1)),
              step = IntObj.newVar(IntObj.newConst(1))
            )
        }
        val newRed = Reduce(prev, dim, op)
        EditVar[S, Matrix[S], Matrix.Var[S]](s"Add ${opV.name} to $name", vr, newRed)
      }
      undo.add(edit)
    }

    private def performAdd(): Unit = {
      val pop = new PopupMenu
      ReduceOpEnum.seq.foreach { op =>
        pop.contents += new MenuItem(new Action(op.name) {
          def apply(): Unit = performAdd(op)
        })
      }
      pop.show(ggAdd, ggAdd.peer.getWidth /* 0 */, 0 /* ggAdd.peer.getHeight */)
    }

    private def performRemove(): Unit = varOptH.foreach { varH =>
      val editOpt = cursor.step { implicit tx =>
        val numRed = reductions.size
        if (numRed == 0) None else {
          val redV  = reductions(numRed - 1)
          val red   = redV.reduction

          /* @tailrec */ def loop(vr: Matrix.Var[S], m: Matrix[S]): Option[(Matrix.Var[S], Matrix[S])] = m match {
            case r : Reduce[S] =>
              val in = r.in
              if (r == red) Some((vr, in))
              else loop(vr, in).map { case (vr1, newIn) =>
                (vr1, Reduce(newIn, r.dim, r.op))
              }

            case vr1: Matrix.Var[S] =>
              val in = vr1()
              loop(vr1, in)

            case _ => None
          }

          val vr0 = varH()
          loop(vr0, vr0()).map { case (vr2, m2) =>
            // vr2() = m2
            EditVar(s"Remove Reduction in $name", vr2, m2)
          }
        }
      }
      editOpt.foreach(undo.add)
    }

    private lazy val addAction = new Action(null) {
      icon = PlusIcon
      def apply(): Unit = performAdd()
    }
    private lazy val ggAdd = new Button(addAction)

    def guiInit(redIsEmpty0: Boolean, redIsLeaf0: Boolean): Unit = {
      val lb    = new Label(name)
      val lbd   = lb.preferredSize
      lbd.width = 96
      lb.preferredSize  = lbd
      lb.minimumSize    = lbd
      lb.maximumSize    = lbd
      val c1  = lb :: reductions.component :: Nil
      val c2  = if (varOptH.isEmpty) c1 else {
        val removeAction = new Action(null) {
          icon = MinusIcon
          def apply(): Unit = performRemove()
        }
        val ggRemove  = new Button(removeAction)
        ggRemove.borderPainted = false
        ggRemove.tooltip = "Remove Reduction"
        ggAdd.borderPainted = false
        ggAdd.tooltip = "Add New Reduction"

        // dynamically hide minus-button
        ggAdd   .visible  = !redIsLeaf0
        ggRemove.visible  = !redIsEmpty0
        reductions.addListener {
          case u: ReductionsView.Update =>
            val visRemove = !u.isEmpty
            val visAdd    = !u.isLeaf
            val remChange = visRemove != ggRemove.visible
            val addChange = visAdd    != ggAdd   .visible
            if (remChange) ggRemove.visible = visRemove
            if (addChange) ggAdd   .visible = visAdd
            if (remChange || addChange) {
              component.revalidate()
              component.repaint()
            }
        }

        c1 ++ List(ggRemove, ggAdd)
      }

      component = new BoxPanel(Orientation.Horizontal) {
        contents ++= c2
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      reductions.dispose()
    }
  }
}
trait DimensionView[S <: Sys[S]] extends View[S] {
  def reductions: ReductionsView[S]
}