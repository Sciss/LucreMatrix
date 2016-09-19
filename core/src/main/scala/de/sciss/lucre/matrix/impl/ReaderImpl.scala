/*
 *  ReaderImpl.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 by Hanns Holger Rutz.
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

trait ReaderImpl extends Matrix.Reader {
  // ---- abstract ----

  protected def streamDim: Int

  protected def section: Vec[Range]

  protected def indexMap: IndexMap

  // ---- impl ----

  private[this] val numFramesI: Int = if (streamDim < 0) 1 else section(streamDim).size

  val numChannels: Int = {
    val size = (1L /: section) ((prod, r) => prod * r.size)
    val numChannelsL = size / numFramesI
    if (numChannelsL > 0xFFFF)
      throw new UnsupportedOperationException(s"The number of channels ($numChannelsL) is larger than supported")
    numChannelsL.toInt
  }

  private[this] var pos = 0

  def numFrames = numFramesI.toLong

  def read(fBuf: Array[Array[Float]], off: Int, len: Int): Unit = {
    if (len < 0) throw new IllegalArgumentException(s"Illegal read length $len")
    val stop = pos + len
    if (stop > numFramesI) throw new EOFException(s"Reading past the end ($stop > $numFramesI)")
    val sect1 = if (pos == 0 && stop == numFramesI) section else {
      val newRange = sampleRange(section(streamDim), pos until stop)
      section.updated(streamDim, newRange)
    }
    val arr = mkArray(toUcarSection(sect1))
    // cf. Arrays.txt for (de-)interleaving scheme
    val t   = if (streamDim <= 0) arr else arr.transpose(0, streamDim)
    val it  = t.getIndexIterator

    var i = off
    val j = off + len
    while (i < j) {
      var ch = 0
      while (ch < numChannels) {
        fBuf(ch)(i) = indexMap.next(it)
        ch += 1
      }
      i += 1
    }

    pos = stop
  }

  protected def mkArray(sect: ma2.Section): ma2.Array

  final protected def toUcarSection(in: Vec[Range]): ma2.Section = {
    val sz      = in.size
    val list    = new ju.ArrayList[ma2.Range](sz)
    var i = 0; while (i < sz) {
      list.add(toUcarRange(in(i)))
      i += 1
    }
    new ma2.Section(list)
  }

  // Note: will throw exception if range is empty or going backwards
  @inline
  private[this] def toUcarRange(in: Range): ma2.Range = new ma2.Range(in.start, in.last, in.step)
}