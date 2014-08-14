/*
 *  ConstMatrixImpl.scala
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

package de.sciss.lucre
package matrix
package impl

import de.sciss.lucre.matrix.DataSource.Resolver
import de.sciss.lucre.matrix.Matrix.Reader
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

  /*

    ex. shape = [4, 3, 2]

    data =

       0:  000 001 | 010 011 | 020 021 ||
       6:  100 101 | 110 111 | 120 121 ||
      12:  200 201 | 210 211 | 222 221 ||
      18:  300 301 | 310 311 | 320 321 ||
      24:

    scans      = [4*3*2*1, 3*2*1, 2*1, 1] = [24, 6, 2, 1]
    streamScan = scans(streamDim + 1)
    streamSkip = if (streamDim < 0) 0 else scans(streamDim)

    numFrames   = if (streamDim < 0) 1 else shape(streamDim)
    numChannels = shape.product / numFrames = scans.head / numFrames

    read:
      flat-offset = pos * streamScan
      for each frame:
        flat-offset' = flat-offset + sc where sc = 0 until streamScan
        then increase flat-offset by streamSkip
        until numChannels values have been processed
   */

  private final class ReaderImpl(shape: Vec[Int], flatData: Vec[Double], streamDim: Int) extends Matrix.Reader {
    private val numFramesI = if (streamDim < 0) 1 else shape(streamDim)

    def numFrames = numFramesI.toLong

    //    val numChannels: Int = {
    //      val sz = (1L /: shape)(_ * _)
    //      val n  = if (streamDim < 0) sz else sz / shape(streamDim)
    //      if (n > 0x7FFFFFFF) throw new IndexOutOfBoundsException(s"numChannels $n exceeds 32-bit range")
    //      n.toInt
    //    }

    private val scans       = Vec.tabulate(shape.size + 1)(d => shape.drop(d).product)  // numFrames must be 32-bit
    val numChannels         = scans.head / numFramesI
    private val streamScan  = scans(streamDim + 1)
    private val streamSkip  = if (streamDim < 0) 0 else scans(streamDim)

    private var pos = 0

    def read(buf: Array[Array[Float]], off: Int, len: Int): Unit = {
      val stop = pos + len
      var off1 = off
      while (pos < stop) {
        var dOff = pos * streamScan
        var sc   = 0
        var ch = 0; while (ch < numChannels) {
          buf(ch)(off1) = flatData(dOff + sc).toFloat
          ch += 1
          sc += 1
          if (sc == streamScan) {
            sc    = 0
            dOff += streamSkip
          }
        }
        pos  += 1
        off1 += 1
      }
    }
  }

  private final class Impl[S <: Sys[S]](shapeConst: Vec[Int], flatData: Vec[Double])
    extends Matrix[S] {

    def name(implicit tx: S#Tx): String = toString

    override def toString = s"Matrix@${hashCode.toHexString}${shapeConst.mkString("[","][","]")}"

    def shape     (implicit tx: S#Tx): Vec[Int]             = shapeConst
    def ranges    (implicit tx: S#Tx): Vec[Range]           = shape.map(0 until _)
    def dimensions(implicit tx: S#Tx): Vec[Dimension.Value] =
      shape.zipWithIndex.map { case (sz, idx) => Dimension.Value(s"dim$idx", sz) }

    def reader(streamDim: Int)(implicit tx: S#Tx, resolver: Resolver[S]): Reader =
      new ReaderImpl(shapeConst, flatData, streamDim)

    def changed: EventLike[S, Matrix.Update[S]] = evt.Dummy.apply

    def debugFlatten(implicit tx: S#Tx): Vec[Double] = flatData

    def write(out: DataOutput): Unit = {
      out.writeByte(3)    // 'constant'
      out.writeInt(opID)  // type
      intVecSer   .write(shapeConst, out)
      doubleVecSer.write(flatData  , out)
    }

    def dispose()(implicit tx: S#Tx) = ()
  }
}
