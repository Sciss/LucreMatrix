/*
 *  ReaderFactory.scala
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

import de.sciss.file._
import de.sciss.lucre.matrix.DataSource.Resolver
import de.sciss.lucre.matrix.Matrix.Reader
import de.sciss.lucre.matrix.impl.ReduceImpl.{TransparentReader, rangeVecSer}
import de.sciss.serial.DataOutput

import scala.concurrent.{ExecutionContext, Future}

object ReaderFactory {
  final val TransparentType = 0
  final val CloudyType      = 1
  final val AverageType     = 2

  sealed trait HasSection extends ReaderFactory {
    var section: Vec[Range]
  }

  final case class Transparent(file: File, name: String, streamDim: Int, var section: Vec[Range])
    extends HasSection {

    private def rangeString(r: Range): String = {
      val con = if (r.isInclusive) "to" else "until"
      val suf = if (r.step == 1) "" else s" by ${r.step}"
      s"${r.start} $con ${r.end}$suf"
    }

    override def toString =
      s"Reduce.Key.Transparent(${file.base}, $name, streamDim = $streamDim, section = ${section.map(rangeString).mkString("[","][","]")})"

    protected def tpeID: Int = TransparentType

    def reader[S <: Sys[S]]()(implicit tx: S#Tx, resolver: DataSource.Resolver[S],
                              exec: ExecutionContext): Future[Reader] = {
      val net = resolver.resolve(file)
      import scala.collection.JavaConverters._
      val v = net.getVariables.asScala.find(_.getShortName == name).getOrElse(
        sys.error(s"Variable '$name' does not exist in data source ${file.base}")
      )

      val r: Reader = new TransparentReader(v, streamDim, section)
      Future.successful(r)
    }

    protected def writeFactoryData(out: DataOutput): Unit = {
      out.writeUTF(file.getPath)
      out.writeUTF(name)
      out.writeShort(streamDim)
      rangeVecSer.write(section, out)
    }
  }

  final case class Cloudy(source: Matrix.Key, streamDim: Int, var section: Vec[Range])
    extends HasSection {

    protected def tpeID: Int = CloudyType

    def reader[S <: Sys[S]]()(implicit tx: S#Tx, resolver: Resolver[S], exec: ExecutionContext): Future[Reader] = {
      source match {
        case const: ConstMatrixImpl.KeyImpl =>
          val r: Reader = new ConstMatrixImpl.ReducedReaderImpl(const.data, streamDim, section)
          Future.successful(r)

        case _ => ??? // later
      }
    }

    protected def writeFactoryData(out: DataOutput): Unit = {
      source.write(out)
      out.writeShort(streamDim)
      rangeVecSer.write(section, out)
    }
  }

  final case class Average(source: Matrix.Key, streamDim: Int, dimIdx: Int, var section: Vec[Range])
    extends HasSection {

    protected def tpeID: Int = AverageType

    protected def writeFactoryData(out: DataOutput): Unit = {
      ???
    }

    def reader[S <: Sys[S]]()(implicit tx: S#Tx, resolver: Resolver[S], exec: ExecutionContext): Future[Reader] = {
      /*

        what we'll do here:

        - run the FScape process
        - flat-map it to a transparent reader for the output file

       */
      ???
    }
  }
}
sealed trait ReaderFactory extends impl.KeyImpl {
  // ---- abstract ----

  protected def tpeID: Int

  protected def writeFactoryData(out: DataOutput): Unit

  // ---- impl ----

  protected def opID: Int = Reduce.opID

  final protected def writeData(out: DataOutput): Unit = {
    out.writeShort(tpeID)
    writeFactoryData(out)
  }
}