/*
 *  ZeroMatrixImpl.scala
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

import java.{util => ju}

import de.sciss.lucre.matrix.DataSource.Resolver
import de.sciss.lucre.matrix.Matrix.Reader
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

  private final class KeyImpl(shapeConst: Vec[Int], streamDim: Int) extends impl.KeyImpl {
    protected def opID: Int = ZeroMatrixImpl.opID

    protected def writeData(out: DataOutput): Unit = ???

    def reader[S <: Sys[S]]()(implicit tx: S#Tx, resolver: Resolver[S]): Reader =
      new ReaderImpl(shapeConst, streamDim)
  }

  private final class Impl[S <: Sys[S]](protected val shapeConst: Vec[Int])
    extends ConstImpl[S] {

    override def toString = s"zeros${shapeConst.mkString("[","][","]")}"

    //    def reader(streamDim: Int)(implicit tx: S#Tx, resolver: Resolver[S]): Reader =
    //      new ReaderImpl(shapeConst, streamDim)

    protected def opID: Int = ZeroMatrixImpl.opID

    def getKey(streamDim: Int)(implicit tx: S#Tx): Matrix.Key = new KeyImpl(shapeConst, streamDim)

    def debugFlatten(implicit tx: S#Tx): Vec[Double] = {
      val sz = size
      require(sz <= 0x7FFFFFFF)
      val szI = sz.toInt
      Vec.fill(szI)(0.0)
    }

    protected def writeData(out: DataOutput): Unit = {
      // out.writeInt(streamDim)
      intVecSer.write(shapeConst, out)
    }
  }
}