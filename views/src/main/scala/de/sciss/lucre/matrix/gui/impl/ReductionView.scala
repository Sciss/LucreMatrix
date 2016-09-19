/*
 *  ReductionView.scala
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

import java.awt
import java.awt.datatransfer.Transferable
import java.awt.event.{InputEvent, MouseAdapter, MouseEvent}
import java.awt.geom.{AffineTransform, GeneralPath, Path2D}
import java.awt.{Graphics, RenderingHints}
import java.io.FileNotFoundException
import javax.swing.TransferHandler.TransferSupport
import javax.swing.{Icon, JComponent, TransferHandler}

import de.sciss.audiowidgets.DualRangeModel
import de.sciss.desktop.UndoManager
import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{IntRangeSliderView, IntSpinnerView, View, deferTx}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.swing.{Alignment, BoxPanel, Component, Graphics2D, Label, Orientation, Swing, TextField}
import scala.util.{Failure, Success}

object ReductionView {
  def apply[S <: Sys[S]](dimVal: Dimension.Value, red: Reduce[S], transferHandler: Option[MatrixView.TransferHandler[S]])
                        (implicit tx: S#Tx, csr: stm.Cursor[S], resolver: DataSource.Resolver[S],
                         exec: ExecutionContext,
                         undo: UndoManager): ReductionView[S] = {
    val dims        = red.dimensions
    val dimIdx      = dims.indexWhere(_.name == dimVal.name)
    // val dimIdxView  = DimensionIndex(dims(dimIdx))
    // important: we need the range of the reduction's input!
    val dimRange    = red.in.ranges.apply(dimIdx)    // XXX TODO - should be dynamic

    // println(s"DIM NAME = ${dimVal.name}; SIZE = ${dimVal.size}; IDX = $dimIdx, RANGE = $dimRange")

    //    def mkUnit(valIdx: Int): Option[Try[String]] =
    //      if (valIdx < 0 || valIdx >= dimRange.size) None else {
    //        val idxInDim = dimRange(valIdx)
    //        dimIdxView.tryFormat(idxInDim)
    //        //        dimIdxView.value(idxInDim).map { mag =>
    //        //          dimIdxView.format(mag)
    //        //        }
    //      }

    //    def mkUnit1(t: Try[String]): String = t match {
    //      case Success(x) => x
    //      case Failure(_: FileNotFoundException) => "&lt;offline&gt;" // HTML entities!
    //      case Failure(e) => e.getClass.getSimpleName
    //    }

    //    def mkTT(up: String, loUnit: Option[Try[String]], hiUnit: Option[Try[String]], lo: Int, hi: Int): String =
    //      loUnit.fold(up) { lu =>
    //        hiUnit.fold(up) { hu =>
    //          val down = if (hi == lo || lu.isFailure)
    //            mkUnit1(lu)
    //          else if (hu.isFailure)
    //            mkUnit1(hu)
    //          else
    //            s"${mkUnit1(lu)} to ${mkUnit1(hu)}"
    //
    //          mkHTML(up, down)
    //        }
    //      }

    // def mkHTML(up: String, down: String): String = s"<HTML><BODY>$up<BR><I>$down</I></BODY>"

    @tailrec def loopOp(op: Reduce.Op[S], vr: Option[Reduce.Op.Var[S]]):
    (ReduceOpEnum, View[S], Reduce.Op[S], Option[Reduce.Op.Var[S]]) = op match {

      case oi: Reduce.Op.Apply[S] =>
        // val viewIdx = IntSpinnerView(oi.index, s"Index in $dimName")
        val rm        = DualRangeModel(minimum = 0, maximum = dimVal.size - 1)
        val exprIdx   = oi.index
        val nameIdx   = s"Index in ${dimVal.name}"
        val slidIdx   = IntRangeSliderView(rm, nameIdx)
        val spinIdx   = IntSpinnerView(exprIdx, nameIdx, 80)
        val dimIdxView= DimensionIndex(dims(dimIdx))
        val unitIdx   = new UnitLabelImpl(dimIdxView, dimRange).init(exprIdx)
        slidIdx.value = Some(exprIdx)
        val dndOpt    = transferHandler.map(DnDButton(_, exprIdx))
        val view: View[S] = new View[S] {
          lazy val component: Component = new BoxPanel(Orientation.Horizontal) {
            contents += new Label("Index:")
            contents += slidIdx.component
            dndOpt.foreach(contents += _.component)
            contents += spinIdx.component
            contents += unitIdx.component
            // contents += Swing.HStrut(4)
          }

          def dispose()(implicit tx: S#Tx): Unit = {
            slidIdx   .dispose()
            spinIdx   .dispose()
            unitIdx   .dispose()
            dimIdxView.dispose()
          }
        }
        (ReduceOpEnum.Apply, view, oi, vr)

      case os: Reduce.Op.Slice[S] =>
        val rm        = DualRangeModel(minimum = 0, maximum = dimVal.size - 1)
        val nameSlice = s"Slice in ${dimVal.name}"
        val viewSlice = IntRangeSliderView(rm, nameSlice)
        val exprLo    = os.from
        val exprHi    = os.to
        viewSlice.rangeLo = Some(exprLo)
        viewSlice.rangeHi = Some(exprHi)
        val spinLo    = IntSpinnerView(exprLo, nameSlice, 80)
        val spinHi    = IntSpinnerView(exprHi, nameSlice, 80)
        val dimIdxView = DimensionIndex(dims(dimIdx))
        val unitLo    = new UnitLabelImpl(dimIdxView, dimRange).init(exprLo)
        val unitHi    = new UnitLabelImpl(dimIdxView, dimRange).init(exprHi)
        val dndLoOpt  = transferHandler.map(DnDButton(_, exprLo))
        val dndHiOpt  = transferHandler.map(DnDButton(_, exprHi))
        val view: View[S] = new View[S] {
          lazy val component: Component = new BoxPanel(Orientation.Horizontal) {
            contents += new Label("Slice:")
            contents += viewSlice.component
            contents += new BoxPanel(Orientation.Vertical) {
              //              override lazy val peer = {
              //                val p = new javax.swing.JPanel with SuperMixin {
              //                  override def getBaseline(width: Int, height: Int): Int = {
              //                    val j = spinLo.component.peer
              //                    j.getBaseline(width, height) + j.getY
              //                  }
              //                }
              //                val l = new javax.swing.BoxLayout(p, Orientation.Vertical.id)
              //                p.setLayout(l)
              //                p
              //              }

              contents += new BoxPanel(Orientation.Horizontal) {
                dndLoOpt.foreach(contents += _.component)
                contents += spinLo.component
                contents += unitLo.component
              }
              contents += new BoxPanel(Orientation.Horizontal) {
                dndHiOpt.foreach(contents += _.component)
                contents += spinHi.component
                contents += unitHi.component
              }
            }
            contents += Swing.HStrut(4)
          }

          def dispose()(implicit tx: S#Tx): Unit = {
            viewSlice .dispose()
            spinLo    .dispose()
            spinHi    .dispose()
            unitLo    .dispose()
            unitHi    .dispose()
            dimIdxView.dispose()
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
        val dndOpt = transferHandler.map(DnDButton(_, exprStride))
        val view: View[S] = new View[S] {
          lazy val component: Component = new BoxPanel(Orientation.Horizontal) {
            contents += new Label("Stride:")
            dndOpt.foreach(contents += _.component)
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
    private var value: Int = _

    def init(ex: IntObj[S])(implicit tx: S#Tx): this.type = {
      val v0      = ex.value
      // val textMin = if (v0 == 0) text0 else mkUnit(0)
      // val szm     = dimRange.size - 1
      // val textMax = if (v0 == szm) text0 else mkUnit(szm)
      // val width   = math.max(textMin.length, textMax.length)
      deferTx(guiInit(v0))
      observer = ex.changed.react { implicit tx => ch =>
        deferTx {
          value = ch.now
          component.text = mkUnit(value)
        }
      }
      this
    }

    private def mkUnit(idx: Int): String = {
      val opt = if (idx < 0 || idx >= dimRange.size) None else {
        val idxInDim = dimRange(idx)
        dimIdxView.tryFormat(idxInDim)
      }

      val text = opt.fold("") {
        case Success(x) => x
        case Failure(_: FileNotFoundException) => "<offline>" // HTML entities! NOT
        case Failure(e) => e.getClass.getSimpleName
      }

      text
    }

    private def guiInit(v0: Int): Unit = {
      value   = v0
      val gg  = new TextField(13)
      gg.text = mkUnit(v0)
      gg.editable = false
      gg.horizontalAlignment = Alignment.Right
      component = gg
      dimIdxView.addListener {
        case DimensionIndex.Ready => gg.text = mkUnit(value)
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = observer.dispose()
  }

  private def mkDnDShape(p: Path2D): Unit = {
    // credits: Raphael Icons (http://raphaeljs.com/icons/), released under MIT license
    p.moveTo(23.898000717163086, 6.135000228881836)
    p.curveTo(22.327001571655273, 5.010000228881836, 20.14000129699707, 5.371000289916992, 19.013999938964844, 6.943000316619873)
    p.lineTo(10.182000160217285, 19.27400016784668)
    p.curveTo(9.378000259399414, 20.395999908447266, 9.63599967956543, 21.95800018310547, 10.758999824523926, 22.762001037597656)
    p.curveTo(11.881999969482422, 23.565000534057617, 13.442999839782715, 23.3070011138916, 14.246999740600586, 22.184001922607422)
    p.lineTo(20.482999801635742, 13.478001594543457)
    p.lineTo(19.670000076293945, 12.895001411437988)
    p.lineTo(13.434999465942383, 21.602001190185547)
    p.lineTo(13.434999465942383 , 21.602001190185547)
    p.curveTo(12.95199966430664, 22.274002075195312, 12.014999389648438, 22.43000030517578, 11.342999458312988, 21.94900131225586)
    p.curveTo(10.669999122619629, 21.468000411987305, 10.515999794006348, 20.530000686645508, 10.99799919128418, 19.856000900268555)
    p.lineTo(10.99799919128418, 19.856000900268555)
    p.lineTo(19.828998565673828, 7.5260009765625)
    p.lineTo(19.829998016357422, 7.525001049041748)
    p.lineTo(19.8279972076416, 7.524001121520996)
    p.curveTo(20.630996704101562, 6.405001163482666, 22.196996688842773, 6.146000862121582, 23.316997528076172, 6.948000907897949)
    p.curveTo(24.43699836730957, 7.751000881195068, 24.69599723815918, 9.317001342773438, 23.893997192382812, 10.43700122833252)
    p.lineTo(23.893997192382812, 10.43600082397461)
    p.lineTo(14.213996887207031, 23.95199966430664)
    p.lineTo(14.214997291564941, 23.952999114990234)
    p.curveTo(13.090997695922852, 25.52199935913086, 10.89899730682373, 25.88399887084961, 9.329997062683105, 24.76099967956543)
    p.curveTo(7.7609968185424805, 23.63599967956543, 7.399997234344482, 21.445999145507812, 8.52299690246582, 19.875999450683594)
    p.lineTo(15.55799674987793, 10.053999900817871)
    p.lineTo(14.744997024536133, 9.472000122070312)
    p.lineTo(7.709997177124023, 19.29399871826172)
    p.curveTo(6.262997150421143, 21.31399917602539, 6.727997303009033, 24.123998641967773, 8.74899673461914, 25.570999145507812)
    p.curveTo(10.769996643066406, 27.018999099731445, 13.579996109008789, 26.55299949645996, 15.026996612548828, 24.533998489379883)
    p.lineTo(24.70699691772461, 11.017998695373535)
    p.curveTo(25.829999923706055, 9.446999549865723, 25.469999313354492, 7.261000156402588, 23.898000717163086, 6.135000228881836)
    p.lineTo(23.898000717163086, 6.135000228881836)
  }

  private def DnDButton[S <: Sys[S]](th: MatrixView.TransferHandler[S], expr: IntObj[S])
                                    (implicit tx: S#Tx, cursor: stm.Cursor[S], undoManager: UndoManager): View[S] = {
    val isVar   = IntObj.Var.unapply(expr).isDefined
    import IntObj.serializer
    val source  = tx.newHandle(expr)
    View.wrap[S](new DnDButton(th, source = source, isVar = isVar))
  }

  private final class DnDButton[S <: Sys[S]](th: MatrixView.TransferHandler[S], source: stm.Source[S#Tx, IntObj[S]],
                                             isVar: Boolean)
                                            (implicit csr: stm.Cursor[S], undoManager: UndoManager)
    extends Label(null, null, Alignment.Center) {

    icon = new Icon {
      private val extent  = 28

      private val shape = {
        val p = new GeneralPath(Path2D.WIND_EVEN_ODD)
        mkDnDShape(p)
        p.closePath()
        val scale   = extent/32f
        AffineTransform.getScaleInstance(scale, scale).createTransformedShape(p)
      }

      def getIconWidth : Int = extent
      def getIconHeight: Int = extent

      def paintIcon(c: awt.Component, g: Graphics, x: Int, y: Int): Unit = {
        val g2 = g.asInstanceOf[Graphics2D]
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON)
        g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE )
        val atOrig = g2.getTransform
        g2.translate(x, y)
        g2.fill(shape)
        g2.setTransform(atOrig)
      }
    }

    private object Transfer extends TransferHandler {
      override def exportAsDrag(jComponent: JComponent, inputEvent: InputEvent, i: Int): Unit = {
        // println(s"exportAsDrag - ${i.toHexString}")
        super.exportAsDrag(jComponent, inputEvent, i)
      }

      override def getSourceActions(c: JComponent): Int = {
        // println("getSourceActions")
        // N.B.: Drag-and-Drop only works on OS X if `MOVE` is included,
        // even if we don't support that action. Thanks Apple for being special!!
        TransferHandler.LINK | TransferHandler.COPY | TransferHandler.MOVE
      }

      override def createTransferable(c: JComponent): Transferable = {
        val opt = csr.step { implicit tx =>
          val ex = source()
          val vr = ex // Expr.Var.unapply(ex).fold(ex)(_.apply()) // don't move variables!
          th.exportInt(vr)
        }
        opt.orNull
      }

      // how to enforce a drop action: https://weblogs.java.net/blog/shan_man/archive/2006/02/choosing_the_dr.html
      override def canImport(support: TransferSupport): Boolean = isVar && /* component.enabled && */ {
        // println(support.getDataFlavors.mkString("---supported flavours:---\n ", "\n ", ""))
        // println(s"drop actions: ${support.getSourceDropActions.toHexString} / ${support.getDropAction.toHexString}")

        if (th.canImportInt(support) &&
          ((support.getSourceDropActions & (TransferHandler.LINK | TransferHandler.COPY)) != 0)) {
          if (support.getDropAction != TransferHandler.COPY) {
            // println("SET LINK")
            support.setDropAction(TransferHandler.LINK)
          }
          true
        }
        else false
      }

      override def importData(support: TransferSupport): Boolean = {
        val isCopy  = support.getDropAction == TransferHandler.COPY
        val editOpt = csr.step { implicit tx =>
          IntObj.Var.unapply(source()).flatMap { vr =>
            th.importInt(support).flatMap { value0 =>
              val valueOpt = if (isCopy) Some(IntObj.newConst[S](value0.value)) else {
                @tailrec def isRecursive(x: IntObj[S]): Boolean =
                  x match {
                    case IntObj.Var(vr1) => if (vr1 == vr) true else isRecursive(vr1())
                    case _ => false
                  }

                if (isRecursive(value0)) None else Some(value0)
              }

              valueOpt.map { value =>
                // println(s"COPY? $isCopy | DROP $value0 | ONTO $vr | YIELDS $value")
                implicit val intTpe = IntObj
                EditVar.Expr[S, Int, IntObj](name = "Drop Number", expr = vr, value = value)
              }
            }
          }
        }
        editOpt.foreach(undoManager.add)
        editOpt.isDefined
      }
    }

    peer.setTransferHandler(Transfer)

    private var dndInitX    = 0
    private var dndInitY    = 0
    private var dndPressed  = false
    private var dndStarted  = false
    private object Mouse extends MouseAdapter {
      override def mousePressed(e: MouseEvent): Unit = {
        dndInitX	  = e.getX
        dndInitY    = e.getY
        dndPressed  = true
        dndStarted	= false
      }

      override def mouseReleased(e: MouseEvent): Unit = {
        dndPressed  = false
        dndStarted	= false
      }

      override def mouseDragged(e: MouseEvent): Unit =
        if (dndPressed && !dndStarted && ((math.abs(e.getX - dndInitX) > 5) || (math.abs(e.getY - dndInitY) > 5))) {
          Transfer.exportAsDrag(peer, e, TransferHandler.COPY /* .LINK */ /* sourceAction(e.getModifiers) */)
          dndStarted = true
        }
    }

    peer.addMouseListener      (Mouse)
    peer.addMouseMotionListener(Mouse)
  }
}
trait ReductionView[S <: Sys[S]] extends View[S] {
  def reduction(implicit tx: S#Tx): Reduce[S]

  /** Constant, may be called on any thread. */
  def isLeaf: Boolean
}