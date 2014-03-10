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
import scala.swing.{Swing, GridBagPanel, Label, Component, TextField}
import de.sciss.lucre.swing.impl.ComponentHolder
import scala.annotation.tailrec
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.swing._
import scala.concurrent.stm.Ref
import de.sciss.desktop.UndoManager
import org.scalatest.selenium.WebBrowser.TextField
import de.sciss.swingplus.Separator

object MatrixViewImpl {
  def apply[S <: Sys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S], undoManager: UndoManager): MatrixView[S] = {
    val res = new Impl[S]
    deferTx(res.guiInit())
    res
  }

  // ---- view data ----

  private sealed trait SelState[S <: Sys[S]] {
    // def h: stm.Source[S#Tx, Dimension.Selection[S]]
    // def isVar: Boolean
  }

  private final class SelIndexState[S <: Sys[S]](val indexVar: Option[stm.Source[S#Tx, Expr.Var[S, Int]]],
                                                 var index: Int)
    extends SelState[S]

  private final class SetNameState [S <: Sys[S]](val nameVar: Option[stm.Source[S#Tx, Expr.Var[S, String]]],
                                                 var name : String)
    extends SelState[S]

  private sealed trait RedOpState[S <: Sys[S]]

  private final class RedApplyState[S <: Sys[S]](val h: stm.Source[S#Tx, Reduce.Op.Apply[S]], var index: Int)
    extends RedOpState[S]

  // ---- impl ----

  private final class Impl[S <: Sys[S]](implicit cursor: stm.Cursor[S], undo: UndoManager)
    extends MatrixView[S] with ComponentHolder[Component] {

    private val _matrix     = Ref(Option.empty[stm.Source[S#Tx, Matrix[S]]])
    private var dimNames    = Vec.empty[String]
    // private var matrixName  = ""

    def matrix(implicit tx: S#Tx): Option[Matrix[S]] = _matrix.get(tx.peer).map(_.apply())

    // what we've got:
    // - opaque input matrix
    // - var
    // - reduce

    private def matrixChanged(): Unit = {

    }

    def matrix_=(value: Option[Matrix[S]])(implicit tx: S#Tx): Unit = {
      val (_dimNames, _matrixName) = value.fold[(Vec[String], String)] {
        (Vec.empty, "<none>")

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

        val mBase = loopMatrix(m0)
        val mEdit = mBase match {
          case vr: Matrix.Var[S] => Some(tx.newHandle(vr))
          case _ => None
        }

        val __dimNames    = dims.map(_.name)
        val __matrixName  = m0.name
        (__dimNames, __matrixName)
      }

      _matrix.set(value.map(tx.newHandle(_)))(tx.peer)

      deferTx {
        if (_dimNames != dimNames) {
          dimNames = _dimNames
          matrixChanged()
        }
        ggName.text = _matrixName
      }
    }

    private lazy val ggName = new Label("<none>")

    def guiInit(): Unit = {
      import GridBagPanel.{Anchor, Fill}
      val p     = new GridBagPanel
      val cons  = new p.Constraints()

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
