/*
 *  ReductionView.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 by Hanns Holger Rutz.
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

import java.io.FileNotFoundException
import javax.swing.event.{ChangeEvent, ChangeListener}

import de.sciss.audiowidgets.DualRangeModel
import de.sciss.desktop.UndoManager
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{IntSpinnerView, IntRangeSliderView, View}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.swing.{TextField, Swing, Alignment, Component, Label, Orientation, BoxPanel}
import scala.util.{Failure, Success, Try}

object ReductionView {
  def apply[S <: Sys[S]](dimVal: Dimension.Value, /* editable: Option[Matrix.Var[S]], */ red: Reduce[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], resolver: DataSource.Resolver[S],
                         exec: ExecutionContext,
                         undo: UndoManager): ReductionView[S] = {
    val dims        = red.dimensions
    val dimIdx      = dims.indexWhere(_.name == dimVal.name)
    val dimIdxView  = DimensionIndex(dims(dimIdx))
    // important: we need the range of the reduction's input!
    val dimRange    = red.in.ranges.apply(dimIdx)    // XXX TODO - should be dynamic

    // println(s"DIM NAME = ${dimVal.name}; SIZE = ${dimVal.size}; IDX = $dimIdx, RANGE = $dimRange")

    def mkUnit(valIdx: Int): Option[Try[String]] =
      if (valIdx < 0 || valIdx >= dimRange.size) None else {
        val idxInDim = dimRange(valIdx)
        dimIdxView.tryFormat(idxInDim)
        //        dimIdxView.value(idxInDim).map { mag =>
        //          dimIdxView.format(mag)
        //        }
      }

    def mkUnit1(t: Try[String]): String = t match {
      case Success(x) => x
      case Failure(_: FileNotFoundException) => "&lt;offline&gt;" // HTML entities!
      case Failure(e) => e.getClass.getSimpleName
    }

    def mkTT(up: String, loUnit: Option[Try[String]], hiUnit: Option[Try[String]], lo: Int, hi: Int): String =
      loUnit.fold(up) { lu =>
        hiUnit.fold(up) { hu =>
          val down = if (hi == lo || lu.isFailure)
            mkUnit1(lu)
          else if (hu.isFailure)
            mkUnit1(hu)
          else
            s"${mkUnit1(lu)} to ${mkUnit1(hu)}"

          mkHTML(up, down)
        }
      }

    def mkHTML(up: String, down: String): String = s"<HTML><BODY>$up<BR><I>$down</I></BODY>"

    @tailrec def loopOp(op: Reduce.Op[S], vr: Option[Reduce.Op.Var[S]]):
    (ReduceOpEnum, View[S], Reduce.Op[S], Option[Reduce.Op.Var[S]]) = op match {

      case oi: Reduce.Op.Apply[S] =>
        // val viewIdx = IntSpinnerView(oi.index, s"Index in $dimName")
        val rm        = DualRangeModel(minimum = 0, maximum = dimVal.size - 1)
        val exprIdx   = oi.index
        val nameIdx   = s"Index in ${dimVal.name}"
        val slidIdx   = IntRangeSliderView(rm, nameIdx)
        val spinIdx   = IntSpinnerView(exprIdx, nameIdx, 80)
        val dimIdxView  = DimensionIndex(dims(dimIdx))
        val unitIdx   = new UnitLabelImpl(dimIdxView, dimRange).init(exprIdx)
        slidIdx.value = Some(exprIdx)
        val view: View[S] = new View[S] {
          lazy val component: Component = new BoxPanel(Orientation.Horizontal) {
            contents += new Label("Index:")
            contents += slidIdx.component
            contents += spinIdx.component
            contents += unitIdx.component
            contents += Swing.HStrut(4)
          }

          def dispose()(implicit tx: S#Tx): Unit = {
            slidIdx.dispose()
            spinIdx.dispose()
            unitIdx.dispose()
            dimIdxView.dispose()
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
              val lo      = rm.rangeLo
              val loUnit  = mkUnit(lo)
              val hi      = rm.rangeHi
              val hiUnit  = mkUnit(hi)
              val up      = if (hi == lo) lo.toString else s"$lo to $hi"
              val tt      = mkTT(up, loUnit, hiUnit, lo, hi)
              viewSlice.component.tooltip = tt
            }
          }
          rm.addChangeListener(cl)
          cl.stateChanged(null)

          new BoxPanel(Orientation.Horizontal) {
            contents += new Label("Slice:")
            contents += viewSlice.component
          }
        }
        (ReduceOpEnum.Slice, view, os, vr)

      case os: Reduce.Op.Stride[S] =>
        // val rm          = DualRangeModel(minimum = 0, maximum = dimVal.size - 1)
        val exprStride  = os.step
        val nameStride  = s"Stride in ${dimVal.name}"
        // val viewStride  = IntRangeSliderView(rm, nameStride)
        val spinStride  = IntSpinnerView(exprStride, nameStride, 80)
        // viewSlice.rangeLo = Some(os.from)
        // viewSlice.rangeHi = Some(os.to  )
        // viewStride.value   = Some(os.step)
        val view: View[S] = new View[S] {
          lazy val component: Component = new BoxPanel(Orientation.Horizontal) {
            contents += new Label("Stride:")
            contents += spinStride.component
          }

          def dispose()(implicit tx: S#Tx): Unit = {
            spinStride.dispose()
          }
        }
        (ReduceOpEnum.Stride, view, os, vr)

      case dv: Reduce.Op.Var[S] =>
        loopOp(dv(), Some(dv))
    }

    val (opEnum, opView, _, _) = loopOp(red.op, None)
    val res = new Impl(opView, tx.newHandle(red), isLeaf = opEnum != ReduceOpEnum.Slice)
    res
  }

  private final class Impl[S <: Sys[S]](peer: View[S], redH: stm.Source[S#Tx, Reduce[S]], val isLeaf: Boolean)
    extends ReductionView[S] /* with ComponentHolder[Component] */ {

    def component = peer.component

    def reduction(implicit tx: S#Tx): Reduce[S] = redH()

    def dispose()(implicit tx: S#Tx) = peer.dispose()
  }

  private final class UnitLabelImpl[S <: Sys[S]](dimIdxView: DimensionIndex[S], dimRange: Range)
    extends View[S] with ComponentHolder[TextField] {

    private var observer: Disposable[S#Tx] = _

    def init(ex: Expr[S, Int])(implicit tx: S#Tx): this.type = {
      val v0      = ex.value
      val text0   = mkUnit(v0)
      // val textMin = if (v0 == 0) text0 else mkUnit(0)
      // val szm     = dimRange.size - 1
      // val textMax = if (v0 == szm) text0 else mkUnit(szm)
      // val width   = math.max(textMin.length, textMax.length)
      deferTx(guiInit(text0 /*, textMin, textMax */))
      observer = ex.changed.react { implicit tx => ch =>
        val text1 = mkUnit(ch.now)
        deferTx(component.text = text1)
      }
      this
    }

    private def mkUnit(idx: Int)(implicit tx: S#Tx): String = {
      val opt = if (idx < 0 || idx >= dimRange.size) None else {
        val idxInDim = dimRange(idx)
        dimIdxView.tryFormat(idxInDim)
      }

      val text = opt.fold("") {
        case Success(x) => x
        case Failure(_: FileNotFoundException) => "&lt;offline&gt;" // HTML entities!
        case Failure(e) => e.getClass.getSimpleName
      }

      text
    }

    private def guiInit(text0: String): Unit = {
      val gg = new TextField(13)
      gg.text = text0
      gg.editable = false
      gg.horizontalAlignment = Alignment.Right
      component = gg
    }

    def dispose()(implicit tx: S#Tx): Unit = observer.dispose()
  }
}
trait ReductionView[S <: Sys[S]] extends View[S] {
  def reduction(implicit tx: S#Tx): Reduce[S]

  /** Constant, may be called on any thread. */
  def isLeaf: Boolean
}