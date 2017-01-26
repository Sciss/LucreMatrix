/*
 *  ReaderImpl.scala
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

package de.sciss.lucre.matrix
package impl

import java.io.EOFException
import java.{util => ju}

import ucar.ma2

import scala.collection.breakOut

object ReaderImpl {
  private def zipToRange(a: Array[Int], b: Array[Int]): Vec[Range] = {
    val vb = Vector.newBuilder[Range]
    val n = a.length
    vb.sizeHint(n)
    var i = 0
    while (i < n) {
      vb += a(i) to b(i)
      i += 1
    }
    vb.result()
  }

  private def calcIndices(off: Long, mods: Array[Int], divs: Array[Int], out: Array[Int]): Unit = {
    var i = 0
    val n = out.length
    while (i < n) {
      val x = off / divs(i)
      val y = if (i == 0) x else x % mods(i)
      out(i) = y.toInt
      i += 1
    }
  }

  @inline
  private def calcPOI(a: Array[Int], b: Array[Int], min: Int): Int = {
    var i = min
    val n = a.length
    while (i < n && a(i) == b(i)) i += 1
    i
  }

  private def indexTrunc(a: Array[Int], poi: Int, inc: Boolean, divs: Array[Int]): Int = {
    var i = 0
    val n = a.length
    var res = 0
    while (i < n) {
      val ai = a(i)
      val x =
        if (i < poi) ai
        else if (i > poi) 0
        else if (inc) ai + 1
        else ai

      res += x * divs(i)
      i += 1
    }
    res
  }

  //  private def calcOff(a: Array[Int], divs: Array[Int]): Int = {
  //    var i = 0
  //    val n = a.length
  //    var res = 0
  //    while (i < n) {
  //      res += a(i) * divs(i)
  //      i += 1
  //    }
  //    res
  //  }

  def partition(shape: Array[Int], start0: Long, stop0: Long)(consume: Vec[Range] => Unit): Unit = {
    val rank  = shape.length
    val rankM = rank - 1

    val s0    = new Array[Int](rank)
    val s1    = new Array[Int](rank)
    val s1m   = new Array[Int](rank)
    val divs  = new Array[Int](rank)

    {
      var i   = rankM
      var div = 1
      while (i >= 0) {
        val sh  = shape(i)
        divs(i) = div
        div    *= sh
        i -= 1
      }
    }

    def loop(start: Long, stop: Long, poiMin: Int, dir: Boolean): Unit =
      if (start < stop) {
        val last = stop - 1

        calcIndices(start, shape, divs = divs, out = s0 )
        calcIndices(stop , shape, divs = divs, out = s1 )
        calcIndices(last , shape, divs = divs, out = s1m)

        val poi: Int = calcPOI(s0, s1m, poiMin)
        val trunc: Long = if (poi >= rankM) {
          if (dir) stop else start
        } else {
          indexTrunc(if (dir) s0 else s1, poi, inc = dir, divs = divs)
        }

        val split = trunc != (if (dir) stop else start)

        if (split) {
          if (dir) {
            loop(start, trunc, poiMin = poi + 1, dir = true )
            loop(trunc, stop , poiMin = 0      , dir = false)
          } else {
            calcIndices(trunc - 1, shape, divs = divs, out = s1m)
            val range = zipToRange(s0, s1m)
            consume(range)
            loop(trunc, stop , poiMin = poi + 1, dir = false)
          }
        } else {
          val range = zipToRange(s0, s1m)
          consume(range)
        }
      }

    loop(start0, stop0, poiMin = 0, dir = true)
  }
}

abstract class ReaderImpl extends Matrix.Reader {
  // ---- abstract ----

  protected def streamDim: Int

  protected def section: Vec[Range]

  protected def indexMap: IndexMap

  protected def mkArray(sect: ma2.Section): ma2.Array

  // ---- impl ----

  private[this] val numFramesI: Int = if (streamDim < 0) 1 else section(streamDim).size

  private[this] val shape: Array[Int] = section.map(_.size)(breakOut)

  final val size: Long = if (shape.length == 0) 0L else {
    var res = 1L
    var i = 0
    while (i < shape.length) {
      res *= shape(i)
      i += 1
    }
    res
  }

  private[this] val numChannelsL: Long = size / numFramesI

  final def numChannels: Int = {
    if (numChannelsL > 0xFFFF)
      throw new UnsupportedOperationException(s"The number of channels ($numChannelsL) is larger than supported")
    numChannelsL.toInt
  }

  private[this] var pos = 0L

  final def numFrames: Long = numFramesI.toLong

  final def readFloat2D(fBuf: Array[Array[Float]], off: Int, len: Int): Unit = {
    if (len < 0) throw new IllegalArgumentException(s"Illegal read length $len")
    val stop = pos + len
    if (stop > numFramesI) throw new EOFException(s"Reading past the end ($stop > $numFramesI)")
    val sect1 = if (pos == 0 && stop == numFramesI) section else {
      val newRange = sampleRange(section(streamDim), pos, stop)
      section.updated(streamDim, newRange)
    }
    val arr = mkArray(toUcarSection(sect1))
    // cf. Arrays.txt for (de-)interleaving scheme
    val t = if (streamDim <= 0) arr else arr.transpose(0, streamDim)
    val it = t.getIndexIterator

    var i = off
    val j = off + len
    val numCh = numChannels
    while (i < j) {
      var ch = 0
      while (ch < numCh) {
        fBuf(ch)(i) = indexMap.nextFloat(it)
        ch += 1
      }
      i += 1
    }

    pos = stop
  }

  final def readDouble1D(dBuf: Array[Double], off: Int, len: Int): Unit = {
    if (len < 0) throw new IllegalArgumentException(s"Illegal read length $len")
    val stop = pos + len
    if (stop > size) throw new EOFException(s"Reading past the end ($stop > $size)")

    var i = off

    def readSection(sect: Seq[Range]): Unit = {
      val uSect = toUcarSection(sect)
      val arr = mkArray(uSect)
      val it = arr.getIndexIterator
      val len0 = arr.getSize.toInt
      val stop0 = off + len0
      while (i < stop0) {
        dBuf(i) = indexMap.nextDouble(it)
        i += 1
      }
    }

    if (pos == 0 && stop == size) {
      readSection(section)
    } else {
      ReaderImpl.partition(shape, pos, stop)(readSection)
    }

    assert(i == off + len)
    pos = stop
  }

  final protected def toUcarSection(in: Seq[Range]): ma2.Section = {
    val sz    = in.size
    val list  = new ju.ArrayList[ma2.Range](sz)
    var i = 0
    while (i < sz) {
      list.add(toUcarRange(in(i)))
      i += 1
    }
    new ma2.Section(list)
  }

  // Note: will throw exception if range is empty or going backwards
  @inline
  private[this] def toUcarRange(in: Range): ma2.Range = new ma2.Range(in.start, in.last, in.step)
}