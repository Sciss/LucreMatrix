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
import scala.swing.{Component, Swing}
import de.sciss.lucre.swing._
import de.sciss.lucre.swing.impl.ComponentHolder
import scala.annotation.tailrec

object MatrixViewImpl {
  def apply[S <: Sys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S]): MatrixView[S] = {
    val res = new Impl[S](cursor)
    deferTx(res.guiInit())
    res
  }

  // ---- view data ----

  private sealed trait SelState[S <: Sys[S]]{
    def h: stm.Source[S#Tx, Dimension.Selection[S]]
    // def isVar: Boolean
  }

  private final class SelIndexState[S <: Sys[S]](val h: stm.Source[S#Tx, Dimension.Selection.Index[S]],
                                                 var index: Int, val isVar: Boolean)
    extends SelState[S]

  private final class SetNameState [S <: Sys[S]](val h: stm.Source[S#Tx, Dimension.Selection.Name [S]],
                                                 var name : String)
    extends SelState[S]

  private sealed trait RedOpState[S <: Sys[S]]

  private final class RedApplyState[S <: Sys[S]](val h: stm.Source[S#Tx, Reduce.Op.Apply[S]], var index: Int)
    extends RedOpState[S]

  // ---- impl ----

  private final class Impl[S <: Sys[S]](cursor: stm.Cursor[S])
    extends MatrixView[S] with ComponentHolder[Component] {

    private var _matrix = Option.empty[stm.Source[S#Tx, Matrix[S]]]

    def matrix(implicit tx: S#Tx): Option[Matrix[S]] = _matrix.map(_.apply())

    // what we've got:
    // - opaque input matrix
    // - var
    // - reduce

    def matrix_=(value: Option[Matrix[S]])(implicit tx: S#Tx): Unit = {
      value.foreach { m0 =>
        val dim     = m0.dimensions
        var dimMap  = Map.empty[String, List[Any]]

        @tailrec def loop(m: Matrix[S]): Matrix[S] = m match {
          case red: Reduce[S] =>
            // red.dim - Dimension.Selection
            //  - Index, Name, Var
            // red.op - Reduce.Op
            //  - Apply, Slice, Var

            loop(red.in)
          case vr: Matrix.Var[S] =>
            vr
          case _ =>
            m
        }

        val mBase = loop(m0)
        val mEdit = mBase match {
          case vr: Matrix.Var[S] => Some(tx.newHandle(vr))
          case _ => None
        }
      }

      ???
    }

    def guiInit(): Unit = {

      ???
    }

    def dispose()(implicit tx: S#Tx): Unit = ???
  }
}
