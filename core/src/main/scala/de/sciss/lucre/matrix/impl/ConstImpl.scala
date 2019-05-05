/*
 *  ConstImpl.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2019 by Hanns Holger Rutz.
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
import de.sciss.serial.DataOutput

trait ConstImpl[S <: Sys[S]] extends MatrixRoot[S] with evt.impl.ConstObjImpl[S, Matrix.Update[S]] {
  // ---- abstract ----

  protected def opId: Int

  protected def writeData1(out: DataOutput): Unit

  protected def shapeConst: Vec[Int]

  // ---- impl ----

  // final def mkCopy()(implicit tx: S#Tx): Matrix[S] = this

  final def shape     (implicit tx: S#Tx): Vec[Int]             = shapeConst
  final def ranges    (implicit tx: S#Tx): Vec[Option[Range]]   = shapeConst.map(sz => Some(0 until sz))

  //  final def dimensions(implicit tx: S#Tx): Vec[Dimension.Value] =
  //    shapeConst.zipWithIndex.map { case (sz, idx) => Dimension.Value(s"dim$idx", sz) }

  final def dimensions(implicit tx: S#Tx): Vec[Matrix[S]] =
    shapeConst.zipWithIndex.map { case (sz, idx) =>
      val name = s"dim$idx"
      MatrixFactoryImpl.newConst1D(name, Vec.tabulate(sz)(_.toDouble))
    }

  final protected def writeData(out: DataOutput): Unit = {
    out.writeInt(opId)  // type
    writeData1(out)
  }

  // final def dispose()(implicit tx: S#Tx) = ()

  protected def nameConst: String

  final def name(implicit tx: S#Tx): String = nameConst

  protected def unitsConst: String

  final def units(implicit tx: S#Tx): String = unitsConst

  // final def changed: EventLike[S, Matrix.Update[S]] = evt.Dummy.apply
}