/*
 *  ConstMatrixImpl.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2017 by Hanns Holger Rutz.
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
import de.sciss.lucre.stm.{Copy, Elem}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import ucar.ma2

object ConstMatrixImpl {
  final val opID = 1

  def apply1D[S <: Sys[S]](name: String, units: String, v: Vec[Double])(implicit tx: S#Tx): Matrix[S] = {
    val shape = Vec(v.size)
    val data  = Data(name, units, shape, v)
    new Impl[S](tx.newID(), data)
  }

  def apply2D[S <: Sys[S]](name: String, units: String, v: Vec[Vec[Double]])(implicit tx: S#Tx): Matrix[S] = {
    val sz1   = v.headOption.fold(0)(_.size)
    val shape = Vec(v.size, sz1)
    require(v.forall(_.size == sz1), "In a 2D matrix, all row vectors must have equal length")
    val flat  = v.flatten
    val data  = Data(name, units, shape, flat)
    new Impl[S](tx.newID(), data)
  }

  def apply3D[S <: Sys[S]](name: String, units: String, v: Vec[Vec[Vec[Double]]])(implicit tx: S#Tx): Matrix[S] = {
    val h1    = v.headOption
    val sz1   = h1.fold(0)(_.size)
    val sz2   = h1.flatMap(_.headOption).fold(0)(_.size)
    val shape = Vec(v.size, sz1, sz2)
    require(v.forall { d1 =>
      d1.size == sz1 && d1.forall(_.size == sz2)
    }, "In a 3D matrix, all dimension slices must have equal length")
    val flat  = v.flatMap(_.flatten)
    val data  = Data(name, units, shape, flat)
    new Impl[S](tx.newID(), data)
  }

  private[matrix] def readIdentified[S <: Sys[S]](id: S#ID, in: DataInput)(implicit tx: S#Tx): Matrix[S] = {
    val data = readData(in)
    new Impl[S](id, data)
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

  private final class ReaderImpl(key: KeyImpl) extends Matrix.Reader {
    import key.data.{shape => shapeConst, _}
    import key.streamDim

    private[this] val numFramesI = if (streamDim < 0) 1 else shapeConst(streamDim)

    def numFrames: Long = numFramesI.toLong

    val size: Long = if (shapeConst.isEmpty) 0L else (1L /: shapeConst)(_ * _)

    //    val numChannels: Int = {
    //      val sz = (1L /: shape)(_ * _)
    //      val n  = if (streamDim < 0) sz else sz / shape(streamDim)
    //      if (n > 0x7FFFFFFF) throw new IndexOutOfBoundsException(s"numChannels $n exceeds 32-bit range")
    //      n.toInt
    //    }

    private[this] val scans       = Vec.tabulate(shapeConst.size + 1)(d => shapeConst.drop(d).product)  // numFrames must be 32-bit
    val numChannels: Int          = scans.head / numFramesI
    private[this] val streamScan  = scans(streamDim + 1)
    private[this] val streamSkip  = if (streamDim < 0) 0 else scans(streamDim)

    private[this] var pos = 0L

    def readFloat2D(buf: Array[Array[Float]], off: Int, len: Int): Unit = {
      val stop = pos + len
      var off1 = off
      while (pos < stop) {
        val dOffL = pos * streamScan
        if (dOffL > 0x7FFFFFFF) throw new IndexOutOfBoundsException(dOffL.toString)
        var dOff  = dOffL.toInt
        var sc    = 0
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

    def readDouble1D(buf: Array[Double], off: Int, len: Int): Unit = {
      val stop = pos + len
      var off1 = off
      while (pos < stop) {
        val dOffL = pos * streamScan
        if (dOffL > 0x7FFFFFFF) throw new IndexOutOfBoundsException(dOffL.toString)
        var dOff  = dOffL.toInt
        var sc    = 0
        buf(off1) = flatData(dOff + sc)
        sc += 1
        if (sc == streamScan) {
          sc    = 0
          dOff += streamSkip
        }
        pos  += 1
        off1 += 1
      }
    }
  }

  private[matrix] def readIdentifiedKey(in: DataInput): Matrix.Key = {
    val streamDim = in.readShort()
    val data      = readData(in)
    KeyImpl(data, streamDim)
  }

  final case class KeyImpl(data: Data, streamDim: Int)
    extends impl.KeyImpl {

    override def productPrefix = "ConstMatrix.Key"

    override def toString = s"$productPrefix($data, streamDim = $streamDim)"

    def reader[S <: Sys[S]]()(implicit tx: S#Tx, resolver: Resolver[S]): Reader = new ReaderImpl(this)

    protected def opID: Int = ConstMatrixImpl.opID

    protected def writeData(out: DataOutput): Unit = {
      out.writeShort(streamDim)
      data.write(out)
    }
  }

  private def readData(in: DataInput): Data = {
    val name        = in.readUTF()
    val units       = in.readUTF()
    val shapeConst  = intVecSer   .read(in)
    val flatData    = doubleVecSer.read(in)
    Data(name, units, shapeConst, flatData)
  }

  final case class Data(name: String, units: String, shape: Vec[Int], flatData: Vec[Double]) {
    def write(out: DataOutput): Unit = {
      out.writeUTF(name)
      out.writeUTF(units)
      intVecSer   .write(shape   , out)
      doubleVecSer.write(flatData, out)
    }

    override def toString =
      s"$productPrefix@${hashCode().toHexString}($name, $units, shape = ${shape.mkString("[","][","]")})"
  }

  private final class Impl[S <: Sys[S]](val id: S#ID, data: Data)
    extends ConstImpl[S] {

    import data.flatData

    protected def shapeConst: Vec[Int]  = data.shape
    protected def nameConst : String    = data.name
    protected def unitsConst: String    = data.units

    def copy[Out <: stm.Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl(txOut.newID(), data)

    override def toString = s"$nameConst${shapeConst.mkString("[","][","]")}"

    //    def reader(streamDim: Int)(implicit tx: S#Tx, resolver: Resolver[S]): Reader =
    //      new ReaderImpl(shapeConst, flatData, streamDim)

    def getKey(streamDim: Int)(implicit tx: S#Tx): Matrix.Key = KeyImpl(data, streamDim)

    def debugFlatten(implicit tx: S#Tx): Vec[Double] = flatData

    protected def opID: Int = ConstMatrixImpl.opID

    protected def writeData1(out: DataOutput): Unit = data.write(out)
  }

  final class ReducedReaderImpl(d: Data, protected val streamDim: Int, protected val section: Vec[Range])
    extends impl.ReaderImpl {

    protected def indexMap: IndexMap = IndexMap.Double

    protected def mkArray(sect: ma2.Section): ma2.Array = {
      val arr = ma2.Array.factory(ma2.DataType.DOUBLE, d.shape.toArray)
      d.flatData.iterator.zipWithIndex.foreach { case (value, idx) =>
        arr.setDouble(idx, value)
      }
      arr.section(sect.getOrigin, sect.getShape, sect.getStride)
    }
  }
}