/*
 *  DragAndDrop.scala
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

import java.awt.datatransfer.{UnsupportedFlavorException, Transferable, DataFlavor}
import javax.swing.TransferHandler.TransferSupport
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm

import collection.breakOut

object DragAndDrop {
  trait Flavor[+A] extends DataFlavor

  def internalFlavor[A](implicit ct: reflect.ClassTag[A]): Flavor[A] =
    new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType + ";class=\"" + ct.runtimeClass.getName + "\"") with Flavor[A]

  final case class IntExprDrag[S <: Sys[S]](source: stm.Source[S#Tx, Expr[S, Int]])

  val IntExprFlavor = DragAndDrop.internalFlavor[IntExprDrag[_]]

  def get[A](t: TransferSupport, flavor: Flavor[A]): Option[A] =
    if (!t.isDataFlavorSupported(flavor)) None
    else Some(t.getTransferable.getTransferData(flavor).asInstanceOf[A])

  object Transferable {
    /** Creates a transferable for one particular flavor. */
    def apply[A](flavor: Flavor[A])(data: A): Transferable = new Transferable {
      override def toString = s"Transferable($data)"

      // private val flavor = internalFlavor[A]
      // println(s"My flavor is $flavor")
      def getTransferDataFlavors: Array[DataFlavor] = Array(flavor) // flavors.toArray
      def isDataFlavorSupported(_flavor: DataFlavor): Boolean = {
        // println(s"is $flavor the same as ${this.flavor} ? ${flavor == this.flavor}")
        _flavor == flavor
        // flavors.contains(flavor)
      }
      def getTransferData(_flavor: DataFlavor): AnyRef  = {
        if (!isDataFlavorSupported(_flavor)) throw new UnsupportedFlavorException(flavor)
        data  /* .getOrElse(throw new IOException()) */ .asInstanceOf[AnyRef]
      }
    }

    /** Creates a transferable by wrapping a sequence of existing transferables. */
    def seq(xs: Transferable*): Transferable = new Transferable {
      def getTransferDataFlavors: Array[DataFlavor] = xs.flatMap(_.getTransferDataFlavors)(breakOut)
      def isDataFlavorSupported(_flavor: DataFlavor): Boolean = xs.exists(_.isDataFlavorSupported(_flavor))
      def getTransferData(_flavor: DataFlavor): AnyRef = {
        val peer = xs.find(_.isDataFlavorSupported(_flavor)).getOrElse(throw new UnsupportedFlavorException(_flavor))
        peer.getTransferData(_flavor)
      }
    }
  }
}