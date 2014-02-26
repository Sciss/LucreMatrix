/*
 *  ZeroMatrixImpl.scala
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
package impl

import de.sciss.lucre.expr.Expr
import de.sciss.lucre.{event => evt}
import evt.EventLike
import de.sciss.lucre.matrix.Matrix.Update
import de.sciss.lucre.synth.expr.ExprImplicits
import de.sciss.serial.{DataInput, ImmutableSerializer, DataOutput}
import de.sciss.lucre.matrix.Dimension.Value

object ZeroMatrixImpl {
  final val opID = 0

  def apply[S <: Sys[S]](shape: Vec[Int])(implicit tx: S#Tx): Matrix[S] =
    new Impl[S](shape)

  private[matrix] def readIdentified[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Matrix[S] = {
    val shape = intVecSer.read(in)
    new Impl[S](shape)
  }

  private val intVecSer = ImmutableSerializer.indexedSeq[Int]

  private final class Impl[S <: Sys[S]](shapeConst: Vec[Int])
    extends Matrix[S] {

    def name(implicit tx: S#Tx): String = s"zeros${shape.mkString("[","][","]")}"

    def shape     (implicit tx: S#Tx): Vec[Int]             = shapeConst
    def ranges    (implicit tx: S#Tx): Vec[Range]           = shape.map(0 until _)
    def dimensions(implicit tx: S#Tx): Vec[Dimension.Value] =
      shape.zipWithIndex.map { case (sz, idx) => Dimension.Value(s"dim$idx", sz) }

    def changed: EventLike[S, Update[S]] = evt.Dummy.apply

    def flatten(implicit tx: S#Tx): Vec[Double] = {
      val sz = size
      require(sz <= 0x7FFFFFFF)
      val szI = sz.toInt
      Vec.fill(szI)(0.0)
    }

    def write(out: DataOutput): Unit = {
      out.writeByte(3)    // 'constant'
      out.writeInt(opID)  // type
      intVecSer.write(shapeConst, out)
    }

    def dispose()(implicit tx: S#Tx) = ()
  }
}