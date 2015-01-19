/*
 *  Serializers.scala
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

import de.sciss.serial.{DataOutput, DataInput, ImmutableSerializer}

object Serializers {
  implicit object RangeSerializer extends ImmutableSerializer[Range] {
    def read(in: DataInput): Range = {
      val start       = in readInt()
      val end         = in readInt()
      val isInclusive = in readBoolean()
      val step        = in readInt()
      if (isInclusive)
        Range.inclusive(start, end, step)
      else
        Range.apply    (start, end, step)
    }

    def write(r: Range, out: DataOutput): Unit = {
      import r._
      out writeInt     start
      out writeInt     end
      out writeBoolean isInclusive
      out writeInt     step
    }
  }
}
