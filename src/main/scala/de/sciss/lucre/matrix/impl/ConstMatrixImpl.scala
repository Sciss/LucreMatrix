/*
 *  ConstMatrixImpl.scala
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

package de.sciss.lucre
package matrix
package impl

import de.sciss.lucre.{event => evt}
import evt.EventLike
import de.sciss.serial.{DataInput, ImmutableSerializer, DataOutput}

object ConstMatrixImpl {
  final val opID = 1

  def apply1D[S <: Sys[S]](v: Vec[Double])(implicit tx: S#Tx): Matrix[S] = {
    val shape = Vec(v.size)
    new Impl[S](shape, v)
  }

  def apply2D[S <: Sys[S]](v: Vec[Vec[Double]])(implicit tx: S#Tx): Matrix[S] = {
    val sz1   = v.headOption.fold(0)(_.size)
    val shape = Vec(v.size, sz1)
    require(v.forall(_.size == sz1), "In a 2D matrix, all row vectors must have equal length")
    val flat  = v.flatten
    new Impl[S](shape, flat)
  }

  def apply3D[S <: Sys[S]](v: Vec[Vec[Vec[Double]]])(implicit tx: S#Tx): Matrix[S] = {
    val h1    = v.headOption
    val sz1   = h1.fold(0)(_.size)
    val sz2   = h1.flatMap(_.headOption).fold(0)(_.size)
    val shape = Vec(v.size, sz1, sz2)
    require(v.forall { d1 =>
      d1.size == sz1 && d1.forall(_.size == sz2)
    }, "In a 3D matrix, all dimension slices must have equal length")
    val flat  = v.flatMap(_.flatten)
    new Impl[S](shape, flat)
  }

  private[matrix] def readIdentified[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Matrix[S] = {
    val shape = intVecSer   .read(in)
    val flat  = doubleVecSer.read(in)
    new Impl[S](shape, flat)
  }

  private val intVecSer     = ImmutableSerializer.indexedSeq[Int   ]
  private val doubleVecSer  = ImmutableSerializer.indexedSeq[Double]

  private final class Impl[S <: Sys[S]](shapeConst: Vec[Int], flatData: Vec[Double])
    extends Matrix[S] {

    def name(implicit tx: S#Tx): String = toString

    override def toString = s"Matrix@${hashCode.toHexString}${shapeConst.mkString("[","][","]")}"

    def shape     (implicit tx: S#Tx): Vec[Int]             = shapeConst
    def ranges    (implicit tx: S#Tx): Vec[Range]           = shape.map(0 until _)
    def dimensions(implicit tx: S#Tx): Vec[Dimension.Value] =
      shape.zipWithIndex.map { case (sz, idx) => Dimension.Value(s"dim$idx", sz) }

    def changed: EventLike[S, Matrix.Update[S]] = evt.Dummy.apply

    def flatten(implicit tx: S#Tx): Vec[Double] = flatData

    def write(out: DataOutput): Unit = {
      out.writeByte(3)    // 'constant'
      out.writeInt(opID)  // type
      intVecSer   .write(shapeConst, out)
      doubleVecSer.write(flatData  , out)
    }

    def dispose()(implicit tx: S#Tx) = ()
  }
}
