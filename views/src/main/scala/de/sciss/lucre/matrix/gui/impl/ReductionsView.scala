/*
 *  ReductionsView.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2017 by Hanns Holger Rutz.
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

import de.sciss.lucre.swing.{View, deferTx}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.model.Model
import de.sciss.model.impl.ModelImpl

import scala.concurrent.stm.Ref
import scala.swing.{Orientation, BoxPanel}

object ReductionsView {
  def apply[S <: Sys[S]](dimName: String)(implicit tx: S#Tx): ReductionsView[S] = {
    val res = new Impl[S](dimName)
    deferTx(res.guiInit())
    res
  }

  case class Update(size: Int, isLeaf: Boolean) {
    def isEmpty: Boolean = size == 0
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
      val vec1    = children.transformAndGet(_.patch(idx, view :: Nil, 0))(tx.peer)
      val isLeaf  = vec1.nonEmpty && vec1.last.isLeaf
      deferTx {
        component.contents.insert(idx, view.component)
        dispatch(ReductionsView.Update(size = vec1.size, isLeaf = isLeaf))
      }
    }

    def remove(idx: Int)(implicit tx: S#Tx): Unit = {
      val vec0  = children.get(tx.peer)
      val view  = vec0(idx)
      val vec1  = vec0.patch(idx, Nil, 1)
      children.set(vec1)(tx.peer)
      val isLeaf = vec1.nonEmpty && vec1.last.isLeaf

      deferTx {
        component.contents.remove(idx)
        dispatch(ReductionsView.Update(size = vec1.size, isLeaf = isLeaf))
      }

      view.dispose()
    }

    def apply(idx: Int)(implicit tx: S#Tx): ReductionView[S] = children.get(tx.peer).apply(idx)

    def isLeaf(implicit tx: S#Tx): Boolean = {
      val vec = children.get(tx.peer)
      vec.nonEmpty && vec.last.isLeaf
    }

    def size(implicit tx: S#Tx): Int = children.get(tx.peer).size

    def isEmpty(implicit tx: S#Tx): Boolean = children.get(tx.peer).isEmpty
  }
}
trait ReductionsView[S <: Sys[S]] extends View[S] with Model[ReductionsView.Update] {
  def dimName: String

  def size(implicit tx: S#Tx): Int

  def isEmpty(implicit tx: S#Tx): Boolean

  def apply(idx: Int)(implicit tx: S#Tx): ReductionView[S]

  def remove(idx: Int)(implicit tx: S#Tx): Unit

  // def clear(): Unit

  def isLeaf(implicit tx: S#Tx): Boolean

  def insert(idx: Int, view: ReductionView[S])(implicit tx: S#Tx): Unit
}