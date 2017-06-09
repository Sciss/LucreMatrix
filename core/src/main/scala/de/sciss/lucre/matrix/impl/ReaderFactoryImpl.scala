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
import de.sciss.fscape.Graph
import de.sciss.lucre.matrix.DataSource.Resolver
import de.sciss.lucre.matrix.Matrix.Reader
import de.sciss.lucre.matrix.impl.ReduceImpl.{TransparentReader, rangeVecSer}
import de.sciss.lucre.stm
import de.sciss.serial.DataOutput
import de.sciss.synth.proc.GenContext

import scala.concurrent.{ExecutionContext, Future}

object ReaderFactoryImpl {
  final val TransparentType = 0
  final val CloudyType      = 1
  final val AverageType     = 2

  trait HasSection[S <: Sys[S]] extends Matrix.ReaderFactory[S] {
    def section: Vec[Range]

    def reduce(dimIdx: Int, range: Range): HasSection[S]
  }

  trait KeyHasSection extends KeyImpl /* ReaderFactoryImpl */ {
    // ---- abstract ----

    def section: Vec[Range]

    protected def tpeID: Int

    protected def writeFactoryData(out: DataOutput): Unit

    // ---- impl ----

    protected def opID: Int = Reduce.opID

    final protected def writeData(out: DataOutput): Unit = {
      out.writeShort(tpeID)
      writeFactoryData(out)
    }
  }

  final case class TransparentKey(file: File, name: String, streamDim: Int, section: Vec[Range])
    extends KeyHasSection {

    protected def tpeID: Int = TransparentType

    protected def writeFactoryData(out: DataOutput): Unit = {
      out.writeUTF(file.getPath)
      out.writeUTF(name)
      out.writeShort(streamDim)
      rangeVecSer.write(section, out)
    }

    private def rangeString(r: Range): String = {
      val con = if (r.isInclusive) "to" else "until"
      val suf = if (r.step == 1) "" else s" by ${r.step}"
      s"${r.start} $con ${r.end}$suf"
    }

    override def toString: String = {
      val secStr = section.map(rangeString).mkString("[", "][", "]")
      s"Reduce.Key.Transparent(${file.base}, $name, streamDim = $streamDim, section = $secStr)"
    }
  }

  final class Transparent[S <: Sys[S]](val key: TransparentKey)
    extends HasSection[S] {

    override def toString = s"Reduce.ReaderFactory($key)"

    def reduce(dimIdx: Int, range: Range): HasSection[S] = {
      import key.{copy, file, name, streamDim}
      val newKey = copy(file = file, name = name, streamDim = streamDim, section = section.updated(dimIdx, range))
      new Transparent[S](newKey)
    }

    def section: Vec[Range] = key.section

    def reader()(implicit tx: S#Tx, resolver: DataSource.Resolver[S],
                 exec: ExecutionContext, context: GenContext[S]): Future[Reader] = {
      import key.{file, name, streamDim}
      val net = resolver.resolve(file)
      import scala.collection.JavaConverters._
      val v = net.getVariables.asScala.find(_.getShortName == name).getOrElse(
        sys.error(s"Variable '$name' does not exist in data source ${file.base}")
      )

      val r: Reader = new TransparentReader(v, streamDim, section)
      Future.successful(r)
    }
  }

  final case class CloudyKey(source: Matrix.Key, streamDim: Int, section: Vec[Range])
    extends KeyHasSection {

    protected def tpeID: Int = CloudyType

    protected def writeFactoryData(out: DataOutput): Unit = {
      source.write(out)
      out.writeShort(streamDim)
      rangeVecSer.write(section, out)
    }
  }

  final class Cloudy[S <: Sys[S]](val key: CloudyKey)
    extends HasSection[S] {

    protected def tpeID: Int = CloudyType

    def section: Vec[Range] = key.section

    def reduce(dimIdx: Int, range: Range): HasSection[S] = {
      import key.{copy, source}
      val newKey = copy(source = source, section = section.updated(dimIdx, range))
      new Cloudy[S](newKey)
    }

    def reader()(implicit tx: S#Tx, resolver: Resolver[S], exec: ExecutionContext,
                 context: GenContext[S]): Future[Reader] = {
      import key.{source, streamDim}
      source match {
        case const: ConstMatrixImpl.Key =>
          val r: Reader = new ConstMatrixImpl.ReducedReaderImpl(const.data, streamDim, section)
          Future.successful(r)

        case _ => ??? // later
      }
    }
  }

  final class Average[S <: Sys[S]](source: stm.Source[S#Tx, Matrix[S]], streamDim: Int, dimIdx: Int,
                                   val section: Vec[Range])
    extends HasSection[S] {

//    protected def tpeID: Int = AverageType
//
//    protected def writeFactoryData(out: DataOutput): Unit = {
//      ???
//    }

    def reduce(dimIdx: Int, range: Range): HasSection[S] = ???

    def key: Matrix.Key = ???

    def reader()(implicit tx: S#Tx, resolver: Resolver[S], exec: ExecutionContext,
                 context: GenContext[S]): Future[Reader] = {
      /*

        what we'll do here:

        - run the FScape process
        - flat-map it to a transparent reader for the output file

       */

      val g = Graph {

      }

      ??? // RRR
    }
  }
}
//sealed trait ReaderFactoryImpl extends impl.KeyImpl {
//  // ---- abstract ----
//
//  protected def tpeID: Int
//
//  protected def writeFactoryData(out: DataOutput): Unit
//
//  // ---- impl ----
//
//  protected def opID: Int = Reduce.opID
//
//  final protected def writeData(out: DataOutput): Unit = {
//    out.writeShort(tpeID)
//    writeFactoryData(out)
//  }
//}