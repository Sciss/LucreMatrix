/*
 *  MatrixView.scala
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

import scala.concurrent.ExecutionContext
import scala.swing.Component
import de.sciss.lucre.stm
import de.sciss.lucre.swing.View
import impl.{MatrixViewImpl => Impl}
import de.sciss.desktop.UndoManager
import de.sciss.model.Model

object MatrixView {
  def apply[S <: Sys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S], resolver: DataSource.Resolver[S],
                         exec: ExecutionContext, undoManager: UndoManager): MatrixView[S] = Impl[S]

  sealed trait Update
  case object Resized extends Update
}
trait MatrixView[S <: Sys[S]] extends View[S] with Model[MatrixView.Update] {
  def matrix(implicit tx: S#Tx): Option[Matrix[S]]
  def matrix_=(value: Option[Matrix[S]])(implicit tx: S#Tx): Unit

  def nameVisible(implicit tx: S#Tx): Boolean
  def nameVisible_=(value: Boolean)(implicit tx: S#Tx): Unit

  def rowHeaders(implicit tx: S#Tx): Vec[View[S]]
  def rowHeaders_=(views: Vec[View[S]])(implicit tx: S#Tx): Unit

  def component: Component
}
