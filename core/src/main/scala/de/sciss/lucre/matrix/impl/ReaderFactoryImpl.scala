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

import at.iem.sysson.fscape.graph
import de.sciss.file._
import de.sciss.fscape.lucre.{Cache, UGenGraphBuilder => UGB}
import de.sciss.fscape.lucre.impl.RenderingImpl
import de.sciss.fscape.lucre.impl.RenderingImpl.CacheValue
import de.sciss.fscape.stream.Control
import de.sciss.fscape.{GE, Graph}
import de.sciss.lucre.matrix.DataSource.Resolver
import de.sciss.lucre.matrix.Matrix.Reader
import de.sciss.lucre.matrix.impl.ReduceImpl.{TransparentReader, rangeVecSer}
import de.sciss.lucre.stm
import de.sciss.serial.DataOutput
import de.sciss.synth.proc.GenContext

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

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

  /** @param file       the NetCDF file
    * @param name       the variable name
    */
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

  final case class AverageKey(source: Matrix.Key, streamDim: Int, section: Vec[Range],
                              avgDims: Vec[String])
    extends KeyHasSection {

    protected def tpeID: Int = AverageType

    protected def writeFactoryData(out: DataOutput): Unit = {
      source.write(out)
      out.writeShort(streamDim)
      rangeVecSer.write(section, out)
      out.writeShort(avgDims.size)
      avgDims.foreach { name =>
        out.writeUTF(name)
      }
    }
  }

  private final class AvgUGBContext[S <: Sys[S]](avg: Average[S])(implicit protected val context: GenContext[S],
                                                                  protected val executionContext: ExecutionContext)
    extends UGBContextBase[S] with UGBContextImpl[S] {
    
    private[this] var fOut = Option.empty[File]
    
    def resources: List[File] = fOut.toList

    protected def findMatrix(vr: graph.Matrix)(implicit tx: S#Tx): Matrix[S] = {
      if (vr.name != "in") sys.error(s"Unknown matrix ${vr.name}")
      avg.input
    }

    protected def requestDim(vrName: String, dimNameL: String)(implicit tx: S#Tx): Option[(Matrix[S], Int)] =
      if (vrName != "in") None else {
        val mat     = avg.input
        val dimIdx  = mat.dimensions.indexWhere(_.name == dimNameL)
        if (dimIdx < 0) None else Some(mat -> dimIdx)
      }

    override def requestInput[Res](req: UGB.Input { type Value = Res }, 
                                   io: UGB.IO[S] with UGB)(implicit tx: S#Tx): Res = 
      req match {
        case UGB.Input.Attribute("out") =>
          if (fOut.isEmpty) {
            val res = Cache.createTempFile()
            fOut = Some(res)
          }
          UGB.Input.Attribute.Value(fOut)
          
        case _ => super.requestInput(req, io)
      } 
  }

  final class Average[S <: Sys[S]](inH: stm.Source[S#Tx, Matrix[S]], name: String, val key: AverageKey)
    extends HasSection[S] {

    def input(implicit tx: S#Tx): Matrix[S] = inH()

    def reduce(dimIdx: Int, range: Range): HasSection[S] = reduceAvgOpt(dimIdx, range, None)

    def reduceAvg(dimIdx: Int)(implicit tx: S#Tx): Average[S] = {
      val in0         = inH()
      val avgDimName  = in0.dimensions.apply(dimIdx).name
      reduceAvgOpt(dimIdx, 0 to 0, Some(avgDimName))
    }

    private def reduceAvgOpt(dimIdx: Int, range: Range, avgDimName: Option[String]): Average[S] = {
      import key.{copy, source, streamDim, avgDims}
      val newDims = avgDims ++ avgDimName
      val newKey  = copy(source = source, streamDim = streamDim,
        section = section.updated(dimIdx, range), avgDims = newDims)
      new Average[S](inH, name = name, key = newKey)
    }

    def section: Vec[Range] = key.section

    def reader()(implicit tx: S#Tx, resolver: Resolver[S], exec: ExecutionContext,
                 context: GenContext[S]): Future[Reader] = {
      /*

        what we'll do here:

        - run the FScape process
        - flat-map it to a transparent reader for the output file

       */

      val g = Graph {
        import at.iem.sysson.fscape.graph._
        import de.sciss.fscape.graph._
        val mIn     = Matrix("in")
        val dims    = key.avgDims.map(name => Dim(mIn, name))
        val dSz     = dims.map(_.size)
        val win     = mIn.valueWindow(dims: _*)
        val winSz   = dSz.reduce[GE](_ * _) // dSz1 * dSz2
        val isOk    = !win.isNaN
        val v       = Gate(win, isOk) // * isOk // XXX TODO --- NaN * 0 is not zero

        val tr      = Metro(winSz)
        val sum     = RunningSum(v   , tr)
        val count   = RunningSum(isOk, tr)
        val sumTrunc= ResizeWindow(sum  , size = winSz, start = winSz - 1)
        val cntTrunc= ResizeWindow(count, size = winSz, start = winSz - 1)
        val mOut    = sumTrunc / cntTrunc
        val specIn  = mIn.spec
        ??? // XXX TODO --- we need to add unit-less dimensions of size 1 instead of dropping them
        val specOut = dims.foldLeft(specIn)(_ drop _)
//        MkMatrix ("out", specOut, mOut)
        MatrixOut("out", specOut, mOut)
      }

      val ugbContext  = new AvgUGBContext(this)
      val ctlConfig   = Control.Config()
      ctlConfig.executionContext = exec
      implicit val control: Control = Control(ctlConfig)
      import context.{cursor, workspaceHandle}
      val uState = UGB.build(ugbContext, g)
      uState match {
        case res: UGB.Complete[S] =>
          val fut: Future[CacheValue] = RenderingImpl.acquire[S](res.structure) {
            try {
              control.runExpanded(res.graph)
              val fut = control.status
              fut.map { _ =>
                val resources   = ugbContext.resources
                val data        = Map.empty[String, Array[Byte]]
                new CacheValue(resources, data)
              }
            } catch {
              case NonFatal(ex) =>
                Future.failed(ex)
            }
          }

          fut.flatMap { cv =>
            val ncFile  = cv.resources.head
            val tKey    = TransparentKey(file = ncFile, name = name, streamDim = key.streamDim, section = key.section)
            val tFact   = new Transparent[S](tKey)
            cursor.step { implicit tx =>
              tFact.reader()
            }
          }

        case res: UGB.Incomplete[S] =>
          Future.failed(new Exception(res.rejectedInputs.mkString("Missing inputs: ", ", ", "")))
      }
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