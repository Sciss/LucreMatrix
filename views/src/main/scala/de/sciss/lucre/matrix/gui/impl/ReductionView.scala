/*
 *  ReductionView.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
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

import javax.swing.event.{ChangeEvent, ChangeListener}

import de.sciss.audiowidgets.DualRangeModel
import de.sciss.desktop.UndoManager
import de.sciss.lucre.stm
import de.sciss.lucre.swing.{IntRangeSliderView, View}

import scala.annotation.tailrec
import scala.swing.{Label, Orientation, BoxPanel}

object ReductionView {
  def apply[S <: Sys[S]](dimVal: Dimension.Value, /* editable: Option[Matrix.Var[S]], */ red: Reduce[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], undo: UndoManager): ReductionView[S] = {
    @tailrec def loopOp(op: Reduce.Op[S], vr: Option[Reduce.Op.Var[S]]):
    (ReduceOpEnum, View[S], Reduce.Op[S], Option[Reduce.Op.Var[S]]) = op match {

      case oi: Reduce.Op.Apply[S] =>
        // val viewIdx = IntSpinnerView(oi.index, s"Index in $dimName")
        val rm        = DualRangeModel(minimum = 0, maximum = dimVal.size - 1)
        val viewIdx   = IntRangeSliderView(rm, s"Index in ${dimVal.name}")
        viewIdx.value = Some(oi.index)
        val view    = View.wrap[S] {
          val cl = new ChangeListener {
            def stateChanged(e: ChangeEvent): Unit = viewIdx.component.tooltip = rm.value.toString
          }
          rm.addChangeListener(cl)
          cl.stateChanged(null)

          new BoxPanel(Orientation.Horizontal) {
            contents += new Label("Index")
            contents += viewIdx.component
          }
        }
        (ReduceOpEnum.Apply, view, oi, vr)

      case os: Reduce.Op.Slice[S] =>
        // val view = StringFieldView(dn.expr, "Dimension Name", columns = 6)
        // val viewLo = IntSpinnerView(os.from , s"Slice in ${dimVal.name}")
        // val viewHi = IntSpinnerView(os.until, s"Slice in ${dimVal.name}")
        val rm        = DualRangeModel(minimum = 0, maximum = dimVal.size - 1)
        val viewSlice = IntRangeSliderView(rm, s"Slice in ${dimVal.name}")
        viewSlice.rangeLo = Some(os.from)
        viewSlice.rangeHi = Some(os.to  )
        val view      = View.wrap[S] {
          val cl = new ChangeListener {
            def stateChanged(e: ChangeEvent): Unit = {
              val lo  = rm.rangeLo
              val hi  = rm.rangeHi
              val txt = if (hi == lo) lo.toString else s"$lo to $hi"
              viewSlice.component.tooltip = txt // s"<HTML><BODY>$txt<BR><I>Yo Chuck!</I></BODY>"
            }
          }
          rm.addChangeListener(cl)
          cl.stateChanged(null)

          new BoxPanel(Orientation.Horizontal) {
            contents += new Label("Slice")
            contents += viewSlice.component
          }
        }
        (ReduceOpEnum.Slice, view, os, vr)

      case os: Reduce.Op.Stride[S] =>
        val rm        = DualRangeModel(minimum = 0, maximum = dimVal.size - 1)
        val viewSlice = IntRangeSliderView(rm, s"Stride in ${dimVal.name}")
        viewSlice.rangeLo = Some(os.from)
        viewSlice.rangeHi = Some(os.to  )
        viewSlice.value   = Some(os.step)
        val view      = View.wrap[S] {
          val cl = new ChangeListener {
            def stateChanged(e: ChangeEvent): Unit = {
              val lo   = rm.rangeLo
              val hi   = rm.rangeHi
              val txt0 = if (hi == lo) lo.toString else s"$lo to $hi"
              val txt  = s"$txt0 by ${math.max(1, rm.value)}"
              viewSlice.component.tooltip = txt
            }
          }
          rm.addChangeListener(cl)
          cl.stateChanged(null)

          new BoxPanel(Orientation.Horizontal) {
            contents += new Label("Stride")
            contents += viewSlice.component
          }
        }
        (ReduceOpEnum.Stride, view, os, vr)

      case dv: Reduce.Op.Var[S] =>
        loopOp(dv(), Some(dv))
    }

    val (_, opView, _, _) = loopOp(red.op, None)
    val res = new Impl(opView, tx.newHandle(red))
    res
  }

  private final class Impl[S <: Sys[S]](peer: View[S], redH: stm.Source[S#Tx, Reduce[S]])
    extends ReductionView[S] /* with ComponentHolder[Component] */ {

    def component = peer.component

    def reduction(implicit tx: S#Tx): Reduce[S] = redH()

    def dispose()(implicit tx: S#Tx) = peer.dispose()
  }
}
trait ReductionView[S <: Sys[S]] extends View[S] {
  def reduction(implicit tx: S#Tx): Reduce[S]
}