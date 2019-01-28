/*
 *  IndexMap.scala
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

import ucar.ma2

sealed trait IndexMap {
  def nextFloat (ma: ma2.IndexIterator): scala.Float
  def nextDouble(ma: ma2.IndexIterator): scala.Double
}
object IndexMap {
  object Byte extends IndexMap {
    def nextFloat (ma: ma2.IndexIterator): scala.Float  = ma.getByteNext().toFloat
    def nextDouble(ma: ma2.IndexIterator): scala.Double = ma.getByteNext().toDouble
  }

  object Short extends IndexMap {
    def nextFloat (ma: ma2.IndexIterator): scala.Float  = ma.getShortNext().toFloat
    def nextDouble(ma: ma2.IndexIterator): scala.Double = ma.getShortNext().toDouble
  }

  object Int extends IndexMap {
    def nextFloat (ma: ma2.IndexIterator): scala.Float  = ma.getIntNext().toFloat
    def nextDouble(ma: ma2.IndexIterator): scala.Double = ma.getIntNext().toDouble
  }

  object Long extends IndexMap {
    def nextFloat (ma: ma2.IndexIterator): scala.Float  = ma.getLongNext().toFloat
    def nextDouble(ma: ma2.IndexIterator): scala.Double = ma.getLongNext().toDouble
  }

  object Float extends IndexMap {
    def nextFloat (ma: ma2.IndexIterator): scala.Float  = ma.getFloatNext()
    def nextDouble(ma: ma2.IndexIterator): scala.Double = ma.getFloatNext().toDouble
  }

  object Double extends IndexMap {
    def nextFloat (ma: ma2.IndexIterator): scala.Float  = ma.getDoubleNext().toFloat
    def nextDouble(ma: ma2.IndexIterator): scala.Double = ma.getDoubleNext()
  }
}