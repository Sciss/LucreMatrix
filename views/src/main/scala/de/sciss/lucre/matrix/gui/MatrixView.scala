/*
 *  MatrixView.scala
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

import scala.swing.Component
import de.sciss.lucre.stm
import de.sciss.lucre.swing.View
import impl.{MatrixViewImpl => Impl}
import de.sciss.desktop.UndoManager

object MatrixView {
  def apply[S <: Sys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S], undoManager: UndoManager): MatrixView[S] = Impl[S]
}
trait MatrixView[S <: Sys[S]] extends View[S] {
  def matrix(implicit tx: S#Tx): Option[Matrix[S]]
  def matrix_=(value: Option[Matrix[S]])(implicit tx: S#Tx): Unit

  def nameVisible(implicit tx: S#Tx): Boolean
  def nameVisible_=(value: Boolean)(implicit tx: S#Tx): Unit

  def rowHeaders(implicit tx: S#Tx): Vec[View[S]]
  def rowHeaders_=(views: Vec[View[S]])(implicit tx: S#Tx): Unit

  def component: Component
}
