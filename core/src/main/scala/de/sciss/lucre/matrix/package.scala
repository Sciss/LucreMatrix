/*
 *  package.scala
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

package de.sciss.lucre

import java.text.SimpleDateFormat
import java.util.{Locale, Date}

import scala.annotation.elidable
import scala.annotation.elidable._

package object matrix {
  type Sys[S <: Sys[S]] = de.sciss.lucre.stm.Sys[S]

  //  val Longs     = de.sciss.lucre.synth.expr.Longs
  //  val Strings   = de.sciss.lucre.synth.expr.Strings
  //  val Ints      = de.sciss.lucre.synth.expr.Ints

  type Vec[+A]  = scala.collection.immutable.IndexedSeq[A]
  val  Vec      = scala.collection.immutable.IndexedSeq

  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'SysSon'", Locale.US)

  var showLog = false

  @elidable(CONFIG) private[matrix] def log(what: => String): Unit =
    if (showLog) println(s"${logHeader.format(new Date())} - $what")

  def sampleRange(in: Range, by: Range): Range = {
    val drop  = by.start
    val stepM = by.step
    require(drop >= 0 && stepM > 0)
    val in1 = in.drop(drop)
    val in2 = if (stepM == 1)
      in1
    else if (in1.isInclusive)  // copy-method is protected
      new Range.Inclusive(start = in1.start, end = in1.end, step = in1.step * stepM)
    else
      new Range(start = in1.start, end = in1.end, step = in1.step * stepM)
    in2.take(by.size)
  }
}