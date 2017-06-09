/*
 *  DimensionIndexImpl.scala
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
package gui
package impl

import java.io.IOException

import de.sciss.lucre.swing.defer
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.model.impl.ModelImpl
import de.sciss.synth.proc.GenContext
import ucar.nc2.time.{CalendarDateFormatter, CalendarPeriod}

import scala.concurrent.{ExecutionContext, Future, Promise, blocking}
import scala.swing.Component
import scala.util.Try

object DimensionIndexImpl {
  def apply[S <: Sys[S]](dim: Matrix[S])(implicit tx: S#Tx, resolver: DataSource.Resolver[S],
                                         exec: ExecutionContext, context: GenContext[S]): DimensionIndex[S] = {
    if (dim.rank != 1) throw new IllegalArgumentException(s"Matrix must be 1-dimensional: $dim")
    val sz  = dim.size.toInt
    val key = dim.getKey(0)
    val p   = Promise[Unit]()
    val arr = new Array[Float](sz)
    val fut = p.future

    try {
      val rFut = key.reader()
      tx.afterCommit {
        val fut1 = rFut.map { r =>
          assert(r.numChannels == 1 && r.numFrames == sz)
          blocking {
            val buf = Array(arr)
            r.readFloat2D(buf, 0, sz)
            // XXX TODO - we should have something like r.close()
          }
        }
        p.completeWith(fut1)
      }
    } catch {
      case e: IOException => p.tryFailure(e)
    }

    val unitsFun = unitsStringFormatter(dim.units)
    val res = new Impl[S](arr, fut, unitsFun)
    fut.foreach(_ => defer(res.fireReady()))
    res
  }

  def shouldUseUnitsString(units: String): Boolean =
    units.startsWith("days since")    ||
    units.startsWith("hours since")   ||
    units.startsWith("seconds since")

  def mkUnitsString(units: String): String = units match {
    case "degrees_north"  => "\u00B0N"
    case "degrees_east"   => "\u00B0E"
    case "kg m-2 s-1"     => "kg/(m\u00B2s)"
    case "W m-2"          => "W/m\u00B2"
    case "m s-1"          => "m/s"
    case _ => units
  }

  // XXX TODO - these could be configurable / extensible
  def unitsStringFormatter(units: String): Double => String = units match {
    case ""               => (d: Double) => f"$d%1.2f"
    case "degrees_north"  => (d: Double) => if (d >= 0) f"$d%1.2f \u00B0N" else f"${-d}%1.2f \u00B0S"
    case "degrees_east"   => (d: Double) => if (d >= 0) f"$d%1.2f \u00B0E" else f"${-d}%1.2f \u00B0W"
    case "(0 - 1)"        => (d: Double) => f"${d * 100}%1.1f%%"
    case "kg m-2 s-1"     => (d: Double) => f"$d%1.2f kg/(m\u00B2s)"
    case "W m-2"          => (d: Double) => f"$d%1.2f W/m\u00B2"
    case "m s-1"          => (d: Double) => f"$d%1.2f m/s"
    case "Pa"             => (d: Double) => f"${d.toInt}%d Pa"
    case _ if units.startsWith("days since") =>
      val date = CalendarDateFormatter.isoStringToCalendarDate(null, units.substring(11))
      (d: Double) => {
        val dt = date.add(d, CalendarPeriod.Field.Day)
        CalendarDateFormatter.toDateTimeString(dt)
      }

    case _ if units.startsWith("hours since") =>
      val date = CalendarDateFormatter.isoStringToCalendarDate(null, units.substring(12))
      (d: Double) => {
        val dt = date.add(d, CalendarPeriod.Field.Hour)
        CalendarDateFormatter.toDateTimeString(dt)
      }

    case _ if units.startsWith("seconds since") =>
      val date = CalendarDateFormatter.isoStringToCalendarDate(null, units.substring(14))
      (d: Double) => {
        val dt = date.add(d, CalendarPeriod.Field.Second)
        CalendarDateFormatter.toDateTimeString(dt)
      }

    case _ => (d: Double) => f"$d%1.2f $units"
  }

  private final class Impl[S <: Sys[S]](arr: Array[Float], fut: Future[Unit], unitsFun: Double => String)
    extends DimensionIndex[S] with ModelImpl[DimensionIndex.Update] with ComponentHolder[Component] {

    def fireReady(): Unit = dispatch(DimensionIndex.Ready)

    def size: Int = arr.length

    def value(index: Int): Option[Double] = if (fut.isCompleted) Some(arr(index)) else None

    def format(value: Double): String = unitsFun(value)

    def tryFormat(index: Int): Option[Try[String]] = {
      val x = fut.value
      x.map(_.map(_ => format(arr(index))))
    }

    def dispose()(implicit tx: S#Tx): Unit = ()
  }
}