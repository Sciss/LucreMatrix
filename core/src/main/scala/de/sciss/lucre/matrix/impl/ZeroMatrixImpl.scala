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

import java.{util => ju}

import de.sciss.lucre.matrix.DataSource.Resolver
import de.sciss.lucre.matrix.Matrix.Reader
import de.sciss.lucre.{event => evt}
import evt.EventLike
import de.sciss.serial.{DataInput, ImmutableSerializer, DataOutput}

object ZeroMatrixImpl {
  final val opID = 0

  def apply[S <: Sys[S]](shape: Vec[Int])(implicit tx: S#Tx): Matrix[S] =
    new Impl[S](shape)

  private[matrix] def readIdentified[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Matrix[S] = {
    val shape = intVecSer.read(in)
    new Impl[S](shape)
  }

  private val intVecSer = ImmutableSerializer.indexedSeq[Int]

  private final class ReaderImpl(shape: Vec[Int], streamDim: Int) extends Matrix.Reader {
    val numFrames: Long = if (streamDim < 0) 1 else shape(streamDim)

    val numChannels: Int = {
      val sz = (1L /: shape)(_ * _)
      val n  = sz / numFrames
      if (n > 0x7FFFFFFF) throw new IndexOutOfBoundsException(s"numChannels $n exceeds 32-bit range")
      n.toInt
    }

    def read(buf: Array[Array[Float]], off: Int, len: Int): Unit = {
      var ch = 0; while (ch < numChannels) {
        ju.Arrays.fill(buf(ch), off, off + len, 0f)
        ch += 1
      }
    }
  }

  private final class Impl[S <: Sys[S]](shapeConst: Vec[Int])
    extends Matrix[S] {

    def name(implicit tx: S#Tx): String = toString

    override def toString = s"zeros${shapeConst.mkString("[","][","]")}"

    def shape     (implicit tx: S#Tx): Vec[Int]             = shapeConst
    def ranges    (implicit tx: S#Tx): Vec[Range]           = shape.map(0 until _)
    def dimensions(implicit tx: S#Tx): Vec[Dimension.Value] =
      shape.zipWithIndex.map { case (sz, idx) => Dimension.Value(s"dim$idx", sz) }

    def changed: EventLike[S, Matrix.Update[S]] = evt.Dummy.apply

    def reader(streamDim: Int)(implicit tx: S#Tx, resolver: Resolver[S]): Reader =
      new ReaderImpl(shapeConst, streamDim)

    def debugFlatten(implicit tx: S#Tx): Vec[Double] = {
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