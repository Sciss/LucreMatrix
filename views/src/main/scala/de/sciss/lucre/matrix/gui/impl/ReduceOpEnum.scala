/*
 *  ReduceOpEnum.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014 by Hanns Holger Rutz.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre.matrix
package gui
package impl

object ReduceOpEnum {
  case object Apply  extends ReduceOpEnum { val id = 0; val name = "Index"  }
  case object Slice  extends ReduceOpEnum { val id = 1; val name = "Slice"  }
  case object Stride extends ReduceOpEnum { val id = 2; val name = "Stride" }

  val seq = Vec[ReduceOpEnum](Apply, Slice, Stride)
}
sealed trait ReduceOpEnum {
  def id  : Int
  def name: String

  override def toString = name
}