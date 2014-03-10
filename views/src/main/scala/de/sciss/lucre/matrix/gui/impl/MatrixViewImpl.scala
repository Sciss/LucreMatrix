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

import de.sciss.lucre.stm
import scala.swing.{FlowPanel, Swing, GridBagPanel, Label, Component}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.swing._
import scala.concurrent.stm.Ref
import de.sciss.desktop.UndoManager
import de.sciss.swingplus.Separator
import GridBagPanel.{Anchor, Fill}
import de.sciss.lucre.stm.Disposable
import scala.annotation.tailrec

object MatrixViewImpl {
  def apply[S <: Sys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S], undoManager: UndoManager): MatrixView[S] = {
    val res = new Impl[S]
    deferTx(res.guiInit())
    res
  }

  // ---- view data ----

  //  private sealed trait SelState[S <: Sys[S]] {
  //    // def h: stm.Source[S#Tx, Dimension.Selection[S]]
  //    // def isVar: Boolean
  //  }
  //
  //  private final class SelIndexState[S <: Sys[S]](val indexVar: Option[stm.Source[S#Tx, Expr.Var[S, Int]]],
  //                                                 var index: Int)
  //    extends SelState[S]
  //
  //  private final class SetNameState [S <: Sys[S]](val nameVar: Option[stm.Source[S#Tx, Expr.Var[S, String]]],
  //                                                 var name : String)
  //    extends SelState[S]
  //
  //  private sealed trait RedOpState[S <: Sys[S]]
  //
  //  private final class RedApplyState[S <: Sys[S]](val h: stm.Source[S#Tx, Reduce.Op.Apply[S]], var index: Int)
  //    extends RedOpState[S]

  // ---- impl ----

  private final class DimensionView[S <: Sys[S]](name: String, val reductions: ReductionsView[S]) {
    private lazy val lbName = new Label(name)
    private lazy val _p     = new FlowPanel(lbName, reductions.p)

    def p: Component = {
      requireEDT()
      _p
    }
  }

  private final class ReductionsView[S <: Sys[S]](editable: Boolean)
    extends Disposable[S#Tx] {

    def clear(): Unit = {
      ???
    }

    def insert(idx: Int): Unit = {
      ???
    }

    def p: Component = {
      requireEDT()
      ???
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      ???
    }
  }

  private object ReduceOpEnum extends Enumeration {
    val Apply, Slice = Value
  }

  private object ReductionView {
    def apply[S <: Sys[S]](dimName: String, editable: Option[Matrix.Var[S]], red: Reduce[S])
                          (implicit tx: S#Tx, cursor: stm.Cursor[S], undo: UndoManager): ReductionView[S] = {
      @tailrec def loopOp(op: Reduce.Op[S], vr: Option[Reduce.Op.Var[S]]):
          (ReduceOpEnum.Value, View[S], Reduce.Op[S], Option[Reduce.Op.Var[S]]) = op match {

        case oi: Reduce.Op.Apply[S] =>
          // val view = IntSpinnerView(di.expr, "Dimension Index", width = 96)
          val view = IntSpinnerView(oi.index, s"Index in $dimName")
          (ReduceOpEnum.Apply, view, oi, vr)

        case os: Reduce.Op.Slice[S] =>
          // val view = StringFieldView(dn.expr, "Dimension Name", columns = 6)
          val viewLo = IntSpinnerView(os.from , s"Slice in $dimName")
          val viewHi = IntSpinnerView(os.until, s"Slice in $dimName")
          val view   = View.wrap[S] {
            new FlowPanel(viewLo.component, viewHi.component)
          }
          (ReduceOpEnum.Slice, view, os, vr)

        case dv: Reduce.Op.Var[S] =>
          loopOp(dv(), Some(dv))
      }
      val (opComboItem, opView, _, opVarOpt) = loopOp(red.op, None)

      opVarOpt.foreach { opVar =>
        ???
      }

      // val obs = red.op.changed.react { implicit tx => upd =>
        // upd.op
      // }

      ???
    }
  }
  private final class ReductionView[S <: Sys[S]] {

  }

  private final class Impl[S <: Sys[S]](implicit cursor: stm.Cursor[S], undo: UndoManager)
    extends MatrixView[S] with ComponentHolder[Component] {

    private val _matrix     = Ref(Option.empty[stm.Source[S#Tx, Matrix    [S]]])
    private val _matrixVar  = Ref(Option.empty[stm.Source[S#Tx, Matrix.Var[S]]])
    private var dimNames    = Vec.empty[String]
    private var editable    = false
    // private var matrixName  = ""

    private lazy val p     = new GridBagPanel
    private lazy val cons  = new p.Constraints()

    private var redComp = Vec.empty[Component]

    def matrix(implicit tx: S#Tx): Option[Matrix[S]] = _matrix.get(tx.peer).map(_.apply())

    // what we've got:
    // - opaque input matrix
    // - var
    // - reduce

    private def matrixChanged(): Unit = {
      val lay = p.layout
      if (redComp.nonEmpty) {
        redComp.foreach(lay -= _)
        redComp = Vec.empty
      }

      if (dimNames.nonEmpty) {
        redComp = dimNames.map { name =>
          new Label(name)
        }
        redComp.zipWithIndex.foreach { case (comp, i) =>
          cons.gridx  = 0
          cons.gridy  = 3 + i
          cons.fill   = Fill.None // Horizontal
          cons.anchor = Anchor.LineStart // .LineEnd
          lay(comp)   = cons
        }
      }
    }

    def matrix_=(value: Option[Matrix[S]])(implicit tx: S#Tx): Unit = {
      val (_dimNames, _matrixName, mEdit) = value.fold[(Vec[String], String, Option[Matrix.Var[S]])] {
        (Vec.empty, "<none>", None)

      } { m0 =>
        val dims    = m0.dimensions
        var dimMap  = Map.empty[String, List[View[S]]]

        /* @tailrec */ def loopMatrix(m: Matrix[S]): Matrix[S] = m match {
          case red: Reduce[S] =>
            // red.dim - Dimension.Selection
            //  - Index, Name, Var
            // red.op - Reduce.Op
            //  - Apply, Slice, Var

            def loopDim(dim: Dimension.Selection[S]): Dimension.Selection[S] = dim match {
              case di: Dimension.Selection.Index[S] =>
                val view = IntSpinnerView(di.expr, "Dimension Index", width = 96)
                di

              case dn: Dimension.Selection.Name [S] =>
                val view = StringFieldView(dn.expr, "Dimension Name", columns = 6)
                dn

              case dv: Dimension.Selection.Var  [S] =>
                loopDim(dv())
                dv

            }
            val dBase = loopDim(red.dim)

            loopMatrix(red.in)

          case vr: Matrix.Var[S] =>
            loopMatrix(vr())
            vr

          case _ =>
            m
        }

        val mBase   = loopMatrix(m0)
        val _mEdit  = mBase match {
          case vr: Matrix.Var[S]  => Some(vr) // Some(tx.newHandle(vr))
          case _                  => None
        }

        val __dimNames    = dims.map(_.name)
        val __matrixName  = m0.name
        (__dimNames, __matrixName, _mEdit)
      }

      _matrix   .set(value.map(tx.newHandle(_)))(tx.peer)
      _matrixVar.set(mEdit.map(tx.newHandle(_)))(tx.peer)
      val _editable = mEdit.isDefined

      deferTx {
        if (_dimNames != dimNames) {
          dimNames = _dimNames
          matrixChanged()
        }
        if (_editable != editable) {
          editable = _editable

        }

        ggName.text = _matrixName
      }
    }

    private lazy val ggName = new Label("<none>")

    def guiInit(): Unit = {
      cons.gridx  = 0
      cons.gridy  = 0
      // cons.insets = insLabel
      cons.fill   = Fill.Horizontal
      cons.anchor = Anchor.LineEnd
      p.layout(ggName) = cons

      cons.gridy += 1
      cons.gridx  = 0
      cons.anchor = Anchor.LineStart
      p.layout(Separator()) = cons

      cons.gridy += 1
      cons.gridx  = 0
      cons.anchor = Anchor.LineStart
      p.layout(new Label("<html><b>Dimensions</b></html>")) = cons

      //      cons.gridx  = 1
      //      cons.insets = insView
      //      cons.fill   = avComp match {
      //        case _: ComboBox[_] => Fill.None
      //        case _: Spinner     => Fill.Horizontal
      //        case _: CheckBox    => Fill.Horizontal
      //        case _: TextField   => Fill.Horizontal
      //        case _              => Fill.Both
      //      }
      //      cons.anchor = Anchor.West
      //      comp.layout(gg) = cons

      p.border = Swing.EmptyBorder(4)

      component = p
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      // XXX TODO: what to dispose? the child views of course
    }
  }
}
