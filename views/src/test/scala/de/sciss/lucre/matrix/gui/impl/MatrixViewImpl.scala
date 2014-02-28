package de.sciss.lucre.matrix
package gui
package impl

import de.sciss.lucre.stm
import scala.swing.{Component, Swing}
import de.sciss.lucre.swing._

object MatrixViewImpl {
  def apply[S <: Sys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S]): MatrixView[S] = {
    val res = new Impl[S](cursor)
    deferTx(res.guiInit())
    res
  }

  private final class Impl[S <: Sys[S]](cursor: stm.Cursor[S]) extends MatrixView[S] {
    private var _component: Component = null
    private var _matrix = Option.empty[stm.Source[S#Tx, Matrix[S]]]

    def component: Component = _component

    def matrix(implicit tx: S#Tx): Option[Matrix[S]] = _matrix.map(_.apply())

    def matrix_=(value: Option[Matrix[S]])(implicit tx: S#Tx): Unit = {
      ???
    }

    def guiInit(): Unit = {
      ???
    }

    def dispose()(implicit tx: S#Tx): Unit = ???
  }
}
