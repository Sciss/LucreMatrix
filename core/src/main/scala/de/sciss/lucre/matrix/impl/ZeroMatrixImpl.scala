/*
 *  ZeroMatrixImpl.scala
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
package impl

import java.{util => ju}

import de.sciss.lucre.matrix.DataSource.Resolver
import de.sciss.lucre.matrix.Matrix.Reader
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Elem, Copy}
import de.sciss.serial.{DataInput, ImmutableSerializer, DataOutput}

object ZeroMatrixImpl {
  final val opID = 0

  def apply[S <: Sys[S]](shape: Vec[Int])(implicit tx: S#Tx): Matrix[S] =
    new Impl[S](tx.newID(), shape)

  private[matrix] def readIdentified[S <: Sys[S]](id: S#ID, in: DataInput)(implicit tx: S#Tx): Matrix[S] = {
    val shape = intVecSer.read(in)
    new Impl[S](id, shape)
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

  private[matrix] def readIdentifiedKey(in: DataInput): Matrix.Key = {
    val streamDim   = in.readShort()
    val shapeConst  = intVecSer.read(in)
    new KeyImpl(shapeConst, streamDim)
  }

  private final class KeyImpl(shapeConst: Vec[Int], val streamDim: Int) extends impl.KeyImpl {
    protected def opID: Int = ZeroMatrixImpl.opID

    override def toString = s"ZeroMatrix.Key(shape = ${shapeConst.mkString("[","][","]")}, streamDim = $streamDim)"

    protected def writeData(out: DataOutput): Unit = {
      out.writeShort(streamDim)
      intVecSer.write(shapeConst, out)
    }

    def reader[S <: Sys[S]]()(implicit tx: S#Tx, resolver: Resolver[S]): Reader =
      new ReaderImpl(shapeConst, streamDim)
  }

  private final class Impl[S <: Sys[S]](val id: S#ID, protected val shapeConst: Vec[Int])
    extends ConstImpl[S] {

    //    def reader(streamDim: Int)(implicit tx: S#Tx, resolver: Resolver[S]): Reader =
    //      new ReaderImpl(shapeConst, streamDim)

    def copy[Out <: stm.Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl(txOut.newID(), shapeConst)

    protected def nameConst = s"zeros${shapeConst.mkString("[","][","]")}"

    protected def unitsConst = ""

    protected def opID: Int = ZeroMatrixImpl.opID

    def getKey(streamDim: Int)(implicit tx: S#Tx): Matrix.Key = new KeyImpl(shapeConst, streamDim)

    def debugFlatten(implicit tx: S#Tx): Vec[Double] = {
      val sz = size
      require(sz <= 0x7FFFFFFF)
      val szI = sz.toInt
      Vec.fill(szI)(0.0)
    }

    protected def writeData1(out: DataOutput): Unit = {
      // out.writeInt(streamDim)
      intVecSer.write(shapeConst, out)
    }
  }
}