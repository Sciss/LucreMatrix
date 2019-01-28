/*
 *  ZeroMatrixImpl.scala
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

import java.{util => ju}

import de.sciss.lucre.matrix.DataSource.Resolver
import de.sciss.lucre.matrix.Matrix.Reader
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Copy, Elem}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.synth.proc.GenContext

import scala.concurrent.{ExecutionContext, Future}

object ZeroMatrixImpl {
  final val opId = 0

  def apply[S <: Sys[S]](shape: Vec[Int])(implicit tx: S#Tx): Matrix[S] =
    new Impl[S](tx.newId(), shape)

  private[matrix] def readIdentified[S <: Sys[S]](id: S#Id, in: DataInput): Matrix[S] = {
    val shape = intVecSer.read(in)
    new Impl[S](id, shape)
  }

  private val intVecSer = ImmutableSerializer.indexedSeq[Int]

  private final class ReaderImpl(shape: Vec[Int], streamDim: Int) extends Matrix.Reader {
    val numFrames: Long = if (streamDim < 0) 1 else shape(streamDim)

    val size: Long = if (shape.isEmpty) 0L else (1L /: shape)(_ * _)

    val numChannels: Int = {
      val sz = (1L /: shape)(_ * _)
      val n  = sz / numFrames
      if (n > 0x7FFFFFFF) throw new IndexOutOfBoundsException(s"numChannels $n exceeds 32-bit range")
      n.toInt
    }

    def readFloat2D(buf: Array[Array[Float]], off: Int, len: Int): Unit = {
      var ch = 0; while (ch < numChannels) {
        ju.Arrays.fill(buf(ch), off, off + len, 0f)
        ch += 1
      }
    }

    def readDouble1D(buf: Array[Double], off: Int, len: Int): Unit =
      ju.Arrays.fill(buf, off, off + len, 0.0)

    def readWindowDouble1D(dims: Array[Int], buf: Array[Double], off: Int): Unit = {
      if (dims.length == 0) return

      var i = 0
      var l = 1L
      while (i < dims.length) {
        val j = dims(i)
        l *= shape(j)
        i += 1
      }
      if (l > 0x7FFFFFFF) throw new IllegalStateException(s"Window size ($l) exceeds 32-bit")
      val len = l.toInt

      ju.Arrays.fill(buf, off, off + len, 0.0)
    }
  }

  private[matrix] def readIdentifiedKey(in: DataInput): Matrix.Key = {
    val streamDim   = in.readShort()
    val shapeConst  = intVecSer.read(in)
    KeyImpl(shapeConst, streamDim)
  }

  private final case class KeyImpl(shapeConst: Vec[Int], streamDim: Int)
    extends impl.KeyImpl {

    def shape : Vec[Int]  = shapeConst
    def rank  : Int       = shape.size
    def size  : Long      = (1L /: shape)(_ * _)

    protected def opId: Int = ZeroMatrixImpl.opId

    override def toString = s"ZeroMatrix.Key(shape = ${shapeConst.mkString("[","][","]")}, streamDim = $streamDim)"

    protected def writeData(out: DataOutput): Unit = {
      out.writeShort(streamDim)
      intVecSer.write(shapeConst, out)
    }
  }

  private final class ReaderFactoryImpl[S <: Sys[S]](val key: KeyImpl)
    extends Matrix.ReaderFactory[S] {

    def size: Long = key.size

    override def toString = s"ZeroMatrix.ReaderFactory($key)"

    def reader()(implicit tx: S#Tx, resolver: Resolver[S], exec: ExecutionContext,
                 context: GenContext[S]): Future[Reader] = {
      val r: Reader = new ReaderImpl(key.shapeConst, key.streamDim)
      Future.successful(r)
    }
  }

  private final class Impl[S <: Sys[S]](val id: S#Id, protected val shapeConst: Vec[Int])
    extends ConstImpl[S] {

    //    def reader(streamDim: Int)(implicit tx: S#Tx, resolver: Resolver[S]): Reader =
    //      new ReaderImpl(shapeConst, streamDim)

    def copy[Out <: stm.Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl(txOut.newId(), shapeConst)

    protected def nameConst = s"zeros${shapeConst.mkString("[","][","]")}"

    protected def unitsConst = ""

    protected def opId: Int = ZeroMatrixImpl.opId

    def prepareReader(streamDim: Int)(implicit tx: S#Tx): Matrix.ReaderFactory[S] =
      new ReaderFactoryImpl[S](KeyImpl(shapeConst, streamDim))

    def debugFlatten(implicit tx: S#Tx, resolver: DataSource.Resolver[S],
                     exec: ExecutionContext, context: GenContext[S]): Future[Vec[Double]] = {
      val sz = size
      require(sz <= 0x7FFFFFFF)
      val szI = sz.toInt
      val v = Vec.fill(szI)(0.0)
      Future.successful(v)
    }

    protected def writeData1(out: DataOutput): Unit = {
      // out.writeInt(streamDim)
      intVecSer.write(shapeConst, out)
    }
  }
}