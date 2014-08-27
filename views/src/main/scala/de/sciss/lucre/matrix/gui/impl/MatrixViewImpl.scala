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

import de.sciss.lucre.stm
import scala.swing.{Insets, GridBagPanel, ScrollPane, Orientation, BoxPanel, Swing, Label, Component}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{View, deferTx}
import scala.concurrent.stm.Ref
import de.sciss.desktop.UndoManager
import de.sciss.swingplus.Separator
import scala.annotation.tailrec
import de.sciss.lucre.stm.Disposable
import scala.swing.GridBagPanel.{Fill, Anchor}
import de.sciss.model.impl.ModelImpl

object MatrixViewImpl {
  var DEBUG = false

  def apply[S <: Sys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S], undoManager: UndoManager): MatrixView[S] = {
    val res = new Impl[S]
    deferTx(res.guiInit())
    res
  }

  // ---- impl ----

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