/*
 *  IndexMap.scala
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

import ucar.ma2

sealed trait IndexMap {
  def next(ma: ma2.IndexIterator): scala.Float
}
object IndexMap {
  object Byte extends IndexMap {
    def next(ma: ma2.IndexIterator): scala.Float = ma.getByteNext().toFloat
  }

  object Short extends IndexMap {
    def next(ma: ma2.IndexIterator): scala.Float = ma.getShortNext().toFloat
  }

  object Int extends IndexMap {
    def next(ma: ma2.IndexIterator): scala.Float = ma.getIntNext().toFloat
  }

  object Long extends IndexMap {
    def next(ma: ma2.IndexIterator): scala.Float = ma.getLongNext().toFloat
  }

  object Float extends IndexMap {
    def next(ma: ma2.IndexIterator): scala.Float = ma.getFloatNext()
  }

  object Double extends IndexMap {
    def next(ma: ma2.IndexIterator): scala.Float = ma.getDoubleNext().toFloat
  }
}