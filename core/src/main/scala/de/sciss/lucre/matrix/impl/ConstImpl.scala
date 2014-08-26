/*
 *  ConstImpl.scala
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
package impl

import de.sciss.lucre.{event => evt}
import evt.EventLike
import de.sciss.serial.DataOutput

trait ConstImpl[S <: Sys[S]] extends Matrix[S] {
  // ---- abstract ----

  protected def opID: Int

  protected def writeData(out: DataOutput): Unit

  protected def shapeConst: Vec[Int]

  // ---- impl ----

  final def shape     (implicit tx: S#Tx): Vec[Int]             = shapeConst
  final def ranges    (implicit tx: S#Tx): Vec[Range]           = shapeConst.map(0 until _)

  //  final def dimensions(implicit tx: S#Tx): Vec[Dimension.Value] =
  //    shapeConst.zipWithIndex.map { case (sz, idx) => Dimension.Value(s"dim$idx", sz) }

  final def dimensions(implicit tx: S#Tx): Vec[Matrix[S]] =
    shapeConst.zipWithIndex.map { case (sz, idx) =>
      val name = s"dim$idx"
      MatrixFactoryImpl.newConst1D(name, Range.Double(0.0, sz, 1.0))
    }

  final def write(out: DataOutput): Unit = {
    out.writeByte(3)    // 'constant'
    out.writeInt(opID)  // type
    writeData(out)
  }

  final def dispose()(implicit tx: S#Tx) = ()

  protected def nameConst: String

  final def name(implicit tx: S#Tx): String = nameConst

  final def changed: EventLike[S, Matrix.Update[S]] = evt.Dummy.apply
}