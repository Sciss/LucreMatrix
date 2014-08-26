/*
 *  MatrixViewImpl.scala
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

import de.sciss.lucre.{expr, stm}
import de.sciss.model.Model
import scala.swing.{Insets, GridBagPanel, ScrollPane, MenuItem, Action, Orientation, BoxPanel, Button, Swing, Label, Component}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing._
import scala.concurrent.stm.Ref
import de.sciss.desktop.UndoManager
import de.sciss.swingplus.{PopupMenu, Separator}
import scala.annotation.tailrec
import javax.swing.{UIManager, Icon}
import java.awt
import java.awt.Graphics
import de.sciss.lucre.stm.Disposable
import scala.swing.GridBagPanel.{Fill, Anchor}
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.audiowidgets.DualRangeModel
import de.sciss.model.impl.ModelImpl
import javax.swing.event.{ChangeEvent, ChangeListener}

object MatrixViewImpl {
  var DEBUG = false

  def apply[S <: Sys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S], undoManager: UndoManager): MatrixView[S] = {
    val res = new Impl[S]
    deferTx(res.guiInit())
    res
  }

  private sealed trait PlusMinus extends Icon {
    final def getIconHeight = 12
    final def getIconWidth  = 12

    final def paintIcon(c: awt.Component, g: Graphics, x: Int, y: Int): Unit = {
      g.setColor(UIManager.getColor(if (c.isEnabled) "Label.foreground" else "Label.disabledForeground"))
      paintImpl(g, x, y)
    }

    protected def paintImpl(g: Graphics, x: Int, y: Int): Unit
  }

  private object MinusIcon extends PlusMinus {
    protected def paintImpl(g: Graphics, x: Int, y: Int): Unit =
      g.fillRect(x, y + 6 - 2, 12, 4)
  }

  private object PlusIcon extends PlusMinus {
    protected def paintImpl(g: Graphics, x: Int, y: Int): Unit = {
      g.fillRect(x, y + 6 - 2, 12, 4)
      g.fillRect(x + 6 - 2, y, 4, 12)
    }
  }

  // ---- impl ----

  private object DimensionView {
    def apply[S <: Sys[S]](varOpt: Option[Matrix.Var[S]], name: String)
                          (implicit tx: S#Tx, cursor: stm.Cursor[S], undo: UndoManager): DimensionView[S] = {
      val red         = ReductionsView[S](name)
      val varOptH     = varOpt.map(tx.newHandle(_))
      val res         = new Impl[S](varOptH, name, red)
      val redIsEmpty  = red.isEmpty
      deferTx(res.guiInit(redIsEmpty = redIsEmpty))
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
          val dim   = Dimension.Selection.Name[S](expr.String.newConst(name))
          val op    = opV match {
            case ReduceOpEnum.Apply =>
              Reduce.Op.Apply[S](expr.Int.newVar(expr.Int.newConst(0)))
            case ReduceOpEnum.Slice =>
              Reduce.Op.Slice[S](
                from = expr.Int.newVar(expr.Int.newConst(0)),
                to   = expr.Int.newVar(expr.Int.newConst(Int.MaxValue - 1))
              )
            case ReduceOpEnum.Stride =>
              Reduce.Op.Stride[S](
                from = expr.Int.newVar(expr.Int.newConst(0)),
                to   = expr.Int.newVar(expr.Int.newConst(Int.MaxValue - 1)),
                step = expr.Int.newVar(expr.Int.newConst(1))
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
      private lazy val ggAdd     = new Button(addAction)

      def guiInit(redIsEmpty: Boolean): Unit = {
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
          ggRemove.visible = !redIsEmpty
          reductions.addListener {
            case u: ReductionsView.Update =>
              val vis = !u.isEmpty
              if (vis != ggRemove.visible) {
                ggRemove.visible = vis
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
  private trait DimensionView[S <: Sys[S]] extends View[S] {
    def reductions: ReductionsView[S]
  }

  private object ReductionsView {
    def apply[S <: Sys[S]](dimName: String)(implicit tx: S#Tx): ReductionsView[S] = {
      val res = new Impl[S](dimName)
      deferTx(res.guiInit())
      res
    }

    case class Update(size: Int) {
      def isEmpty = size == 0
    }

    private final class Impl[S <: Sys[S]](val dimName: String)
      extends ReductionsView[S] with ComponentHolder[BoxPanel] with ModelImpl[ReductionsView.Update] {

      private val children = Ref(Vec.empty[ReductionView[S]])

      def guiInit(): Unit = {
        component = new BoxPanel(Orientation.Horizontal)
      }

      def dispose()(implicit tx: S#Tx): Unit = {
        val views = children.swap(Vec.empty)(tx.peer)
        deferTx(component.contents.clear())
        views.foreach(_.dispose())
      }

      def insert(idx: Int, view: ReductionView[S])(implicit tx: S#Tx): Unit = {
        val vec1 = children.transformAndGet(_.patch(idx, view :: Nil, 0))(tx.peer)
        deferTx {
          component.contents.insert(idx, view.component)
          dispatch(ReductionsView.Update(size = vec1.size))
        }
      }

      def remove(idx: Int)(implicit tx: S#Tx): Unit = {
        val vec0  = children.get(tx.peer)
        val view  = vec0(idx)
        val vec1  = vec0.patch(idx, Nil, 1)
        children.set(vec1)(tx.peer)

        deferTx {
          component.contents.remove(idx)
          dispatch(ReductionsView.Update(size = vec1.size))
        }

        view.dispose()
      }

      def apply(idx: Int)(implicit tx: S#Tx): ReductionView[S] = children.get(tx.peer).apply(idx)

      def size(implicit tx: S#Tx): Int = children.get(tx.peer).size

      def isEmpty(implicit tx: S#Tx): Boolean = children.get(tx.peer).isEmpty
    }
  }
  private trait ReductionsView[S <: Sys[S]] extends View[S] with Model[ReductionsView.Update] {
    def dimName: String

    def size(implicit tx: S#Tx): Int

    def isEmpty(implicit tx: S#Tx): Boolean

    def apply(idx: Int)(implicit tx: S#Tx): ReductionView[S]

    def remove(idx: Int)(implicit tx: S#Tx): Unit

    // def clear(): Unit

    def insert(idx: Int, view: ReductionView[S])(implicit tx: S#Tx): Unit
  }

  private object ReduceOpEnum {
    case object Apply  extends ReduceOpEnum { val id = 0; val name = "Index"  }
    case object Slice  extends ReduceOpEnum { val id = 1; val name = "Slice"  }
    case object Stride extends ReduceOpEnum { val id = 2; val name = "Stride" }

    val seq = Vec[ReduceOpEnum](Apply, Slice, Stride)
  }
  private sealed trait ReduceOpEnum {
    def id  : Int
    def name: String

    override def toString = name
  }

  private object ReductionView {
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
  private trait ReductionView[S <: Sys[S]] extends View[S] {
    def reduction(implicit tx: S#Tx): Reduce[S]
  }

  private final class Impl[S <: Sys[S]](implicit cursor: stm.Cursor[S], undo: UndoManager)
    extends MatrixView[S] with ComponentHolder[Component] with ModelImpl[MatrixView.Update] {

    private val _matrixObs  = Ref(Option.empty[stm.Disposable[S#Tx]])
    private val _matrix     = Ref(Option.empty[stm.Source[S#Tx, Matrix    [S]]])
    private val _matrixVar  = Ref(Option.empty[stm.Source[S#Tx, Matrix.Var[S]]])
    private val dimViews    = Ref(Vec.empty[DimensionView[S]])
    private var editable    = false

    // private lazy val p = new BoxPanel(Orientation.Vertical)
    private lazy val p = new GridBagPanel

    def matrix(implicit tx: S#Tx): Option[Matrix[S]] = _matrix.get(tx.peer).map(_.apply())

    private def disposeViews()(implicit tx: S#Tx): Unit = {
      val oldViews = dimViews.swap(Vec.empty)(tx.peer)
      if (oldViews.nonEmpty) {
        deferTx {
          p.layout --= oldViews.map(_.component)
        }
        oldViews.foreach(_.dispose())
      }
    }

    private def removeMatrix()(implicit tx: S#Tx): Unit = {
      _matrix   .set(None)(tx.peer)
      _matrixVar.set(None)(tx.peer)
      val obsOpt = _matrixObs.swap(None)(tx.peer)
      obsOpt.foreach { obs =>
        if (DEBUG) println(s"MatrixView.removeMatrix, obs = $obs")
        obs.dispose()
      }
    }

    private def matrixUpdate(tx0: S#Tx)(upd: Matrix.Update[S]): Unit = upd match {
      case Matrix.Var.Update.Changed(_, _) =>
        implicit val tx = tx0
        // println("matrixUpdate")
        matrix = matrix // XXX TODO: smart update instead of rebuilding all
      case _ =>
    }

    def matrix_=(value: Option[Matrix[S]])(implicit tx: S#Tx): Unit = {
      disposeViews()
      removeMatrix()

      val (_matrixName, mEdit, _dimViews, obsOpt) =
        value.fold[(String, Option[Matrix.Var[S]], Vec[DimensionView[S]], Option[Disposable[S#Tx]])] {
         ("<none>", None, Vec.empty, None)

      } { m0 =>
        val dims        = m0.dimensions
        val numDims     = dims.size
        // val __dimNames  = dims.map(_.name)

        @tailrec def loopMatrix(m: Matrix[S], vr: Option[Matrix.Var[S]],
            dimViews0: Vec[List[ReductionView[S]]]): (Matrix[S], Option[Matrix.Var[S]], Vec[List[ReductionView[S]]]) =
          m match {
            case red: Reduce[S] =>
              // red.dim - Dimension.Selection
              //  - Index, Name, Var
              // red.op - Reduce.Op
              //  - Apply, Slice, Var

              def loopDim(dim: Dimension.Selection[S]): Int = dim match {
                case di: Dimension.Selection.Index[S] =>
                  val index = di.expr
                  index.value

                case dn: Dimension.Selection.Name [S] =>
                  val name    = dn.expr
                  val nameV   = name.value
                  val indexV  = dims.indexWhere(_.name == nameV)
                  indexV

                case dv: Dimension.Selection.Var  [S] =>
                  loopDim(dv())
              }
              val dIdx = loopDim(red.dim)

              val dimViews1: Vec[List[ReductionView[S]]] = if (dIdx < 0 || dIdx >= numDims) dimViews0 else {
                val dim     = dims(dIdx)
                val dimVal  = Dimension.Value(dim.name, dim.size.toInt)
                val redView = ReductionView(dimVal /* dims(dIdx) */, red)
                val before  = dimViews0(dIdx)
                val now     = redView :: before
                dimViews0.updated(dIdx, now)
              }

              loopMatrix(red.in, vr, dimViews1)

            case vr1: Matrix.Var[S] =>
              loopMatrix(vr1(), Some(vr1), dimViews0)

            case _ =>
              (m, vr, dimViews0)
          }

        val (_, _mEdit, _dimViews0) = loopMatrix(m0, None, Vec.fill(numDims)(Nil))

        val __matrixName  = m0.name
        val _dimViews1    = (_dimViews0 zip dims).map { case (list, dimVal) =>
          val v   = DimensionView(_mEdit, dimVal.name)
          val rs  = v.reductions
          list.foreach { r => rs.insert(rs.size, r) }
          v
        }

        val _obs = m0.changed.react(matrixUpdate)
        if (DEBUG) println(s"MatrixView.matrix_=($m0, ${m0.changed}), new observer = ${_obs}")

        (__matrixName, _mEdit, _dimViews1, Some(_obs))
      }

      _matrix   .set(value.map(tx.newHandle(_)))(tx.peer)
      _matrixVar.set(mEdit.map(tx.newHandle(_)))(tx.peer)
      _matrixObs.set(obsOpt)                    (tx.peer)
      dimViews  .set(_dimViews)                 (tx.peer)

      val _editable = mEdit.isDefined

      deferTx {
        if (_dimViews.nonEmpty) {
          val cons = new p.Constraints(gridx = 1, gridwidth = 1, gridy = 0, gridheight = 1, weightx = 1.0,
            weighty = 0.0, anchor = Anchor.PageStart.id, fill = Fill.Horizontal.id, insets = new Insets(2, 2, 2, 2),
            ipadx = 2, ipady = 2)
          // p.contents ++= _dimViews.map(_.component)
          val lay = p.layout
          _dimViews.zipWithIndex.foreach { case (dv, idx) =>
            cons.gridy = idx
            lay(dv.component) = cons
          }
        }

        if (_editable != editable) {
          editable = _editable
        }

        ggName.text = _matrixName

        p.revalidate()
        p.repaint()
        dispatch(MatrixView.Resized)
      }
    }

    private lazy val ggName = new Label("<none>")

    private lazy val topPane: Component = {
      val p       = new BoxPanel(Orientation.Vertical)
      p.contents += ggName
      val sep     = Separator()
      p.contents += sep
      p.visible   = _nameVisible.single()
      p
    }

    private val _nameVisible = Ref(initialValue = true)
    def nameVisible(implicit tx: S#Tx): Boolean = _nameVisible.get(tx.peer)
    def nameVisible_=(value: Boolean)(implicit tx: S#Tx): Unit = {
      val old = _nameVisible.swap(value)(tx.peer)
      if (old != value) deferTx {
        topPane.visible = value
      }
    }

    private val _rowHeaders = Ref(Vec.empty[View[S]])
    def rowHeaders(implicit tx: S#Tx): Vec[View[S]] = _rowHeaders.get(tx.peer)

    def rowHeaders_=(views: Vec[View[S]])(implicit tx: S#Tx): Unit = {
      val old = _rowHeaders.swap(views)(tx.peer)
      deferTx {
        val cons = new p.Constraints(gridx = 0, gridwidth = 1, gridy = 0, gridheight = 1, weightx = 0.0,
          weighty = 0.0, anchor = Anchor.PageStart.id, fill = Fill.Horizontal.id, insets = new Insets(2, 2, 2, 2),
          ipadx = 2, ipady = 2)
        val lay = p.layout
        if (old.nonEmpty) lay --= old.map(_.component)
        if (views.nonEmpty) views.zipWithIndex.foreach { case (rv, idx) =>
          cons.gridy = idx
          lay(rv.component) = cons
        }
      }
    }

    def guiInit(): Unit = {
      val p1 = new BoxPanel(Orientation.Vertical)
      p1.contents += topPane
      val lbDims   = new Label("<html><b>Dimensions</b></html>")
      p1.contents += lbDims
      p1.border = Swing.EmptyBorder(4)

      //      val p2 = new BorderPanel {
      //        add(p, BorderPanel.Position.West)
      //        add(Swing.HGlue, BorderPanel.Position.East)
      //      }
      val scroll    = new ScrollPane(p)
      // scroll.border = Swing.EmptyBorder
      p1.contents += scroll

      component = p1
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      disposeViews()
      removeMatrix()
    }
  }
}