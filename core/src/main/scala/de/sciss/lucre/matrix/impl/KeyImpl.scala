/*
 *  KeyImpl.scala
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

import de.sciss.serial.{ImmutableSerializer, DataInput, DataOutput}

import scala.annotation.switch

object KeyImpl {
  def read(in: DataInput): Matrix.Key = {
    val opId = in.readInt()
    (opId: @switch) match {
      case Reduce         .`opId` => ReduceImpl     .readIdentifiedKey(in)
      case ConstMatrixImpl.`opId` => ConstMatrixImpl.readIdentifiedKey(in)
      case ZeroMatrixImpl .`opId` => ZeroMatrixImpl .readIdentifiedKey(in)
      case _                      => sys.error(s"Unexpected matrix key type $opId")
    }
  }

  implicit object serializer extends ImmutableSerializer[Matrix.Key] {
    def read(in: DataInput): Matrix.Key = KeyImpl.read(in)

    def write(key: Matrix.Key, out: DataOutput): Unit = key.write(out)
  }
}
trait KeyImpl extends Matrix.Key {
  // protected def streamDim: Int
  protected def opId: Int

  final def write(out: DataOutput): Unit = {
    out.writeInt(opId)
    // out.writeInt(streamDim)
    writeData(out)
  }

  protected def writeData(out: DataOutput): Unit
}
