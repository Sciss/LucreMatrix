/*
 *  DimensionIndexImpl.scala
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

import java.io.{FileNotFoundException, IOException}

import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.model.impl.ModelImpl
import de.sciss.lucre.swing.defer
import ucar.nc2.time.{CalendarPeriod, CalendarDateFormatter}

import scala.concurrent.{ExecutionContext, Future, Promise, blocking}
import scala.swing.Component
import scala.util.{Try, Failure, Success}

object DimensionIndexImpl {
  def apply[S <: Sys[S]](dim: Matrix[S])(implicit tx: S#Tx, resolver: DataSource.Resolver[S],
                                         exec: ExecutionContext): DimensionIndex[S] = {
    if (dim.rank != 1) throw new IllegalArgumentException(s"Matrix must be 1-dimensional")
    val sz  = dim.size.toInt
    val key = dim.getKey(0)
    val p   = Promise[Unit]()
    val arr = new Array[Float](sz)
    val fut = p.future

    try {
      val r = key.reader()
      assert(r.numChannels == 1 && r.numFrames == sz)
      tx.afterCommit {
        val fut1 = Future {
          blocking {
            val buf = Array(arr)
            r.read(buf, 0, sz)
            // XXX TODO - we should have something like r.close()
          }
        }
        // println(s"completeWith($fut1")
        p.completeWith(fut1)
      }
    } catch {
      case e: IOException =>
        /* val res = */ p.tryFailure(e)
        // println(s"tryFailure($e) = $res")
    }

    val unitsFun = mkUnitsString(dim.units)
    val res = new Impl[S](arr, fut, unitsFun)
    fut.foreach(_ => defer(res.fireReady()))
    res
  }

  // def expr[S <: Sys[S]](dim: Matrix[S], index: Expr[S, Int])(fun: Int => Unit): DimensionIndex[S] = ...

  // XXX TODO - these could be configurable / extensible
  private def mkUnitsString(units: String): Double => String = units match {
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

    case _ => (d: Double) => f"$d%1.2f $units"
  }

  private final class Impl[S <: Sys[S]](arr: Array[Float], fut: Future[Unit], unitsFun: Double => String)
    extends DimensionIndex[S] with ModelImpl[DimensionIndex.Update] with ComponentHolder[Component] {

    def fireReady(): Unit = dispatch(DimensionIndex.Ready)

    def size = arr.length

    def value(index: Int): Option[Double] = if (fut.isCompleted) Some(arr(index)) else None

    def format(value: Double): String = unitsFun(value)

    def tryFormat(index: Int): Option[Try[String]] = {
      val x = fut.value
      // println(s"fut.value = $x")
      /* val res = */ x.map(_.map(_ => format(arr(index))))
      // println(s"res = $res")
      // res
    }

    //    fut.value.map {
    //      case Success(_) => format(arr(index))
    //      case Failure(fnf: FileNotFoundException) => "<offline>"
    //      case Failure(e) => s"error: ${e.getMessage}"
    //    }

    def dispose()(implicit tx: S#Tx) = ()
  }
}