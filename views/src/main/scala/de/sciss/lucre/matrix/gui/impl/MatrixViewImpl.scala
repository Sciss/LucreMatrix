/*
 *  MatrixViewImpl.scala
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

package de.sciss.lucre.matrix
package gui
package impl

import de.sciss.lucre.{expr, stm}
import scala.swing.{ScrollPane, MenuItem, Action, Orientation, BoxPanel, Button, Swing, Label, Component}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing._
import scala.concurrent.stm.Ref
import de.sciss.desktop.UndoManager
import de.sciss.swingplus.Separator
import scala.annotation.tailrec
import javax.swing.Icon
import java.awt
import java.awt.Graphics
import scalaswingcontrib.PopupMenu
import de.sciss.lucre.stm.Disposable

object MatrixViewImpl {
  def apply[S <: Sys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S], undoManager: UndoManager): MatrixView[S] = {
    val res = new Impl[S]
    deferTx(res.guiInit())
    res
  }

  private object MinusIcon extends Icon {
    def getIconHeight = 12
    def getIconWidth  = 12

    def paintIcon(c: awt.Component, g: Graphics, x: Int, y: Int): Unit = {
      g.fillRect(x, y + 6 - 2, 12, 4)
    }
  }

  private object PlusIcon extends Icon {
    def getIconHeight = 12
    def getIconWidth  = 12

    def paintIcon(c: awt.Component, g: Graphics, x: Int, y: Int): Unit = {
      g.fillRect(x, y + 6 - 2, 12, 4)
      g.fillRect(x + 6 - 2, y, 4, 12)
    }
  }

  // ---- impl ----

  private object DimensionView {
    def apply[S <: Sys[S]](varOpt: Option[Matrix.Var[S]], name: String)
                          (implicit tx: S#Tx, cursor: stm.Cursor[S]): DimensionView[S] = {
      val red     = ReductionsView[S](name)
      val varOptH = varOpt.map(tx.newHandle(_))
      val res     = new Impl(varOptH, name, red)
      deferTx(res.guiInit())
      res
    }

    private final class Impl[S <: Sys[S]](varOptH: Option[stm.Source[S#Tx, Matrix.Var[S]]], name: String,
                                          val reductions: ReductionsView[S])(implicit cursor: stm.Cursor[S])
      extends DimensionView[S] with ComponentHolder[Component] {

      // XXX TODO: use undo manager
      private def performAdd(opV: ReduceOpEnum): Unit = varOptH.foreach { varH =>
        cursor.step { implicit tx =>
          val vr    = varH()
          val prev  = vr()
          val dim   = Dimension.Selection.Name[S](expr.String.newConst(name))
          val op    = opV match {
            case ReduceOpEnum.Apply =>
              Reduce.Op.Apply[S](expr.Int.newVar(expr.Int.newConst(0)))
            case ReduceOpEnum.Slice =>
              Reduce.Op.Slice[S](
                expr.Int.newVar(expr.Int.newConst(0)),
                expr.Int.newVar(expr.Int.newConst(Int.MaxValue))
              )
            }
          val comp  = Reduce(prev, dim, op)
          vr()      = comp
        }
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

      private def performRemove(): Unit = {

      }

      private lazy val addAction = new Action(null) {
        icon = PlusIcon
        def apply(): Unit = performAdd()
      }
      private lazy val ggAdd     = new Button(addAction)

      def guiInit(): Unit = {
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

    private final class Impl[S <: Sys[S]](val dimName: String)
      extends ReductionsView[S] with ComponentHolder[BoxPanel] {

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
        children.transform(_.patch(idx, view :: Nil, 0))(tx.peer)
        deferTx {
          component.contents.insert(idx, view.component)
        }
      }

      def remove(idx: Int)(implicit tx: S#Tx): Unit = {
        val vec0  = children.get(tx.peer)
        val view  = vec0(idx)
        val vec1  = vec0.patch(idx, Nil, 1)
        children.set(vec1)(tx.peer)

        deferTx {
          component.contents.remove(idx)
        }

        view.dispose()
      }

      def apply(idx: Int)(implicit tx: S#Tx): ReductionView[S] = children.get(tx.peer).apply(idx)

      def size(implicit tx: S#Tx): Int = children.get(tx.peer).size
    }
  }
  private trait ReductionsView[S <: Sys[S]] extends View[S] {
    def dimName: String

    def size(implicit tx: S#Tx): Int

    def apply(idx: Int)(implicit tx: S#Tx): ReductionView[S]

    def remove(idx: Int)(implicit tx: S#Tx): Unit

    // def clear(): Unit

    def insert(idx: Int, view: ReductionView[S])(implicit tx: S#Tx): Unit
  }

  private object ReduceOpEnum {
    case object Apply extends ReduceOpEnum { val id = 0; val name = "Index" }
    case object Slice extends ReduceOpEnum { val id = 1; val name = "Slice" }

    val seq = Vec[ReduceOpEnum](Apply, Slice)
  }
  private sealed trait ReduceOpEnum {
    def id  : Int
    def name: String

    override def toString = name
  }

  private object ReductionView {
    def apply[S <: Sys[S]](dimName: String, /* editable: Option[Matrix.Var[S]], */ red: Reduce[S])
                          (implicit tx: S#Tx, cursor: stm.Cursor[S], undo: UndoManager): ReductionView[S] = {
      @tailrec def loopOp(op: Reduce.Op[S], vr: Option[Reduce.Op.Var[S]]):
          (ReduceOpEnum, View[S], Reduce.Op[S], Option[Reduce.Op.Var[S]]) = op match {

        case oi: Reduce.Op.Apply[S] =>
          // val view = IntSpinnerView(di.expr, "Dimension Index", width = 96)
          val viewIdx = IntSpinnerView(oi.index, s"Index in $dimName")
          val view    = View.wrap[S] {
            new BoxPanel(Orientation.Horizontal) {
              contents += new Label("Index")
              contents += viewIdx.component
            }
          }
          (ReduceOpEnum.Apply, view, oi, vr)

        case os: Reduce.Op.Slice[S] =>
          // val view = StringFieldView(dn.expr, "Dimension Name", columns = 6)
          val viewLo = IntSpinnerView(os.from , s"Slice in $dimName")
          val viewHi = IntSpinnerView(os.until, s"Slice in $dimName")
          val view   = View.wrap[S] {
            new BoxPanel(Orientation.Horizontal) {
              contents += new Label("Slice")
              contents += viewLo.component
              contents += viewHi.component
            }
          }
          (ReduceOpEnum.Slice, view, os, vr)

        case dv: Reduce.Op.Var[S] =>
          loopOp(dv(), Some(dv))
      }

      val (opComboItem, opView, _, opVarOpt) = loopOp(red.op, None)
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
    extends MatrixView[S] with ComponentHolder[Component] {

    private val _matrixObs  = Ref(Option.empty[stm.Disposable[S#Tx]])
    private val _matrix     = Ref(Option.empty[stm.Source[S#Tx, Matrix    [S]]])
    private val _matrixVar  = Ref(Option.empty[stm.Source[S#Tx, Matrix.Var[S]]])
    private val dimViews    = Ref(Vec.empty[DimensionView[S]])
    private var editable    = false

    private lazy val p = new BoxPanel(Orientation.Vertical)

    def matrix(implicit tx: S#Tx): Option[Matrix[S]] = _matrix.get(tx.peer).map(_.apply())

    private def disposeViews()(implicit tx: S#Tx): Unit = {
      val oldViews = dimViews.swap(Vec.empty)(tx.peer)
      if (oldViews.nonEmpty) {
        deferTx {
          p.contents --= oldViews.map(_.component)
        }
        oldViews.foreach(_.dispose())
      }
    }

    private def removeMatrix()(implicit tx: S#Tx): Unit = {
      _matrix   .set(None)(tx.peer)
      _matrixVar.set(None)(tx.peer)
      val obs = _matrixObs.swap(None)(tx.peer)
      obs.foreach(_.dispose())
    }

    private def matrixUpdate(tx0: S#Tx)(upd: Matrix.Update[S]): Unit = upd match {
      case Matrix.Var.Update.Changed(_, _) =>
        implicit val tx = tx0
        // println("matrixUpdate")
        matrix = matrix // XXX TODO: smart update
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
        val __dimNames  = dims.map(_.name)

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
                  val indexV  = __dimNames.indexOf(nameV)
                  indexV

                case dv: Dimension.Selection.Var  [S] =>
                  loopDim(dv())
              }
              val dIdx = loopDim(red.dim)

              val dimViews1: Vec[List[ReductionView[S]]] = if (dIdx < 0 || dIdx >= numDims) dimViews0 else {
                val redView = ReductionView(__dimNames(dIdx), red)
                val before  = dimViews0(dIdx)
                val now     = redView :: before
                dimViews0.patch(dIdx, now :: Nil, 0) // (collection.breakOut)
              }

              loopMatrix(red.in, vr, dimViews1)

            case vr1: Matrix.Var[S] =>
              loopMatrix(vr1(), Some(vr1), dimViews0)

            case _ =>
              (m, vr, dimViews0)
          }

        val (mBase, _mEdit, _dimViews0) = loopMatrix(m0, None, Vec.fill(numDims)(Nil))

        val __matrixName  = m0.name
        val _dimViews1    = (_dimViews0 zip __dimNames).map { case (list, name) =>
          val v   = DimensionView(_mEdit, name)
          val rs  = v.reductions
          list.foreach { r => rs.insert(0, r) }
          v
        }

        val _obs = m0.changed.react(matrixUpdate)

        (__matrixName, _mEdit, _dimViews1, Some(_obs))
      }

      _matrix   .set(value.map(tx.newHandle(_)))(tx.peer)
      _matrixVar.set(mEdit.map(tx.newHandle(_)))(tx.peer)
      _matrixObs.set(obsOpt)                    (tx.peer)
      dimViews  .set(_dimViews)                 (tx.peer)

      val _editable = mEdit.isDefined

      deferTx {
        if (_dimViews.nonEmpty) {
          p.contents ++= _dimViews.map(_.component)
        }

        if (_editable != editable) {
          editable = _editable
        }

        ggName.text = _matrixName

        p.revalidate()
      }
    }

    private lazy val ggName = new Label("<none>")

    def guiInit(): Unit = {
      val p1 = new BoxPanel(Orientation.Vertical)
      p1.contents += ggName

      val sep = Separator()
      p1.contents += sep

      val lbDims = new Label("<html><b>Dimensions</b></html>")
      p1.contents += lbDims
      p1.border = Swing.EmptyBorder(4)

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
