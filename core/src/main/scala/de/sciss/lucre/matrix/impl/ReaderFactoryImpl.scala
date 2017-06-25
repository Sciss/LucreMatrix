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
import de.sciss.fscape.gui.SimpleGUI
import de.sciss.fscape.lucre.FScape.{Output, Rendering}
import de.sciss.fscape.lucre.impl.{AbstractOutputRef, AbstractUGenGraphBuilder, RenderingImpl}
import de.sciss.fscape.lucre.{FScape, UGenGraphBuilder => UGB}
import de.sciss.fscape.stream.Control
import de.sciss.fscape.{GE, Graph}
import de.sciss.lucre.matrix.DataSource.Resolver
import de.sciss.lucre.matrix.Matrix.Reader
import de.sciss.lucre.matrix.impl.ReduceImpl.{TransparentReader, rangeVecSer}
import de.sciss.lucre.stm
import de.sciss.serial.{DataInput, DataOutput}
import de.sciss.synth.proc.GenContext

import scala.concurrent.stm.Txn
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.swing.Swing
import scala.util.control.NonFatal

object ReaderFactoryImpl {
  final val TransparentType = 0
  final val CloudyType      = 1
  final val AverageType     = 2

  var DEBUG     = false
  var GUI_DEBUG = false

  trait HasSection[S <: Sys[S]] extends Matrix.ReaderFactory[S] {
    def section: Vec[Range]

    def reduce(dimIdx: Int, range: Range): HasSection[S]
  }

  trait KeyHasSection extends KeyImpl /* ReaderFactoryImpl */ {
    // ---- abstract ----

    def section: Vec[Range]

    final def shape : Vec[Int]  = section.map(_.size)
    final def rank  : Int       = section.size
    final def size  : Long      = (1L /: section)(_ * _.size)

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

    def size: Long = key.size

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

    def section : Vec[Range]  = key.section
    def size    : Long        = key.size

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

    override def toString: String = {
      val sectionS = section.mkString("section = [", ", ", "]")
      val avgDimsS = avgDims.mkString("avgDims = [", ", ", "]")
      s"$productPrefix($source, streamDim = $streamDim, $sectionS, $avgDimsS)"
    }

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

  private final class AvgUGB[S <: Sys[S]](avg: Average[S], nameIn: String, nameOut: String)
                                         (implicit protected val gen: GenContext[S],
                                         protected val executionContext: ExecutionContext)
    extends UGBContextBase[S] with UGBContextImpl[S] with AbstractUGenGraphBuilder[S] with AbstractOutputRef[S] {
    
//    private[this] var fOut = Option.empty[File]
    private[this] var rOut = Option.empty[Output.Reader]

    protected def context: UGB.Context[S] = this

    protected def findMatrix(vr: graph.Matrix)(implicit tx: S#Tx): Matrix[S] = {
      if (vr.name != nameIn) sys.error(s"Unknown matrix ${vr.name}")
      avg.input
    }

    protected def requestDim(vrName: String, dimNameL: String)(implicit tx: S#Tx): Option[(Matrix[S], Int)] =
      if (vrName != nameIn) None else {
        val mat     = avg.input
        val dimIdx  = mat.dimensions.indexWhere(_.name == dimNameL)
        if (dimIdx < 0) None else Some(mat -> dimIdx)
      }

    protected def requestOutputImpl(reader: Output.Reader): Option[UGB.OutputResult[S]] =
      if (reader.key != nameOut || rOut.isDefined) None else {
        if (DEBUG) avg.debugPrint(s"requestOutput(${reader.key})")
        rOut = Some(reader)
        Some(this)
      }

//    override def requestInput[Res](req: UGB.Input { type Value = Res },
//                                   io: UGB.IO[S] with UGB)(implicit tx: S#Tx): Res =
//      req match {
//        case UGB.Input.Attribute("avg-out") =>
//          if (fOut.isEmpty) {
//            val res = Cache.createTempFile()
//            fOut = Some(res)
//          }
//          UGB.Input.Attribute.Value(fOut)
//
//        case _ => super.requestInput(req, io)
//      }

    def reader: Output.Reader = rOut.getOrElse(sys.error("requestOutput was not called"))

    def updateValue(in: DataInput)(implicit tx: S#Tx): Unit = ()
  }

  final class Average[S <: Sys[S]](inH: stm.Source[S#Tx, Matrix[S]], name: String, val key: AverageKey)
    extends HasSection[S] { factory =>

    if (DEBUG) debugPrint(s"new($name, $key)")

    def size: Long = key.size

    def input(implicit tx: S#Tx): Matrix[S] = inH()

    def reduce(dimIdx: Int, range: Range): HasSection[S] = reduceAvgOpt(dimIdx, range, None)

    def reduceAvg(dimIdx: Int)(implicit tx: S#Tx): Average[S] = {
      val in0         = inH()
      val avgDimName  = in0.dimensions.apply(dimIdx).name
      reduceAvgOpt(dimIdx, 0 to 0, Some(avgDimName))
    }

    private def reduceAvgOpt(dimIdx: Int, range: Range, avgDimName: Option[String]): Average[S] = {
      import key.{avgDims, copy, source, streamDim}
      val newDims = avgDims ++ avgDimName
      val newKey  = copy(source = source, streamDim = streamDim,
        section = section.updated(dimIdx, range), avgDims = newDims)
      new Average[S](inH, name = name, key = newKey)
    }

    def section: Vec[Range] = key.section

    def debugPrint(what: String): Unit = {
      val s = s"--RF-- avg${factory.hashCode().toHexString} $what"
      val txnOpt = Txn.findCurrent
      txnOpt.fold[Unit](println(s)) { implicit itx => Txn.afterCommit(_ => println(s)) }
    }

    def reader()(implicit tx: S#Tx, resolver: Resolver[S], exec: ExecutionContext,
                 context: GenContext[S]): Future[Reader] = {
      /*

        what we'll do here:

        - run the FScape process
        - flat-map it to a transparent reader for the output file

       */

      val nameIn  = s"in-$name"
      val nameOut = s"out-$name"

      val g = Graph {
        import at.iem.sysson.fscape.graph._
        import de.sciss.fscape.graph._
//        if (DEBUG) {
//          (0: GE).poll(0, s"--RF-- avg${factory.hashCode().toHexString} graph")
//        }

        val mIn     = Matrix(nameIn)
        val dims    = key.avgDims.map(name => Dim(mIn, name))
        val dSz     = dims.map(_.size)
        val win     = mIn.valueWindow(dims: _*)
        val winSz   = dSz.reduce[GE](_ * _) // dSz1 * dSz2
        val isOk    = !win.isNaN
        val v       = Gate(win.elastic(), isOk) // * isOk // XXX TODO --- NaN * 0 is not zero

        val tr      = Metro(winSz)
        val sum     = RunningSum(v   , tr)
        val count   = RunningSum(isOk, tr)
        val sumTrunc= ResizeWindow(sum  , size = winSz, start = winSz - 1)
        val cntTrunc= ResizeWindow(count, size = winSz, start = winSz - 1)
        val mOut    = sumTrunc / cntTrunc
        val specIn  = mIn.spec
//        val specOut = dims.foldLeft(specIn)(_ drop _)
        val specOut = dims.foldLeft(specIn)(_ reduce _)
        val framesOut = MkMatrix(nameOut, specOut, mOut)
//        MatrixOut("avg-out", specOut, mOut)

        if (DEBUG) {
          mIn    .size     .poll(0, s"--RF-- avg${factory.hashCode().toHexString} matrix-in size")
          winSz            .poll(0, s"--RF-- avg${factory.hashCode().toHexString} winSz")
          specOut.size     .poll(0, s"--RF-- avg${factory.hashCode().toHexString} spec-out size")
          Length(mOut     ).poll(0, s"--RF-- avg${factory.hashCode().toHexString} mOut-length")
          Length(framesOut).poll(0, s"--RF-- avg${factory.hashCode().toHexString} frames-out")
        }
      }

      val ugb         = new AvgUGB(this, nameIn = nameIn, nameOut = nameOut)
      val cfgDefault  = FScape.defaultConfig
      val ctlConfig   = if (exec == cfgDefault.executionContext) cfgDefault else {
        val b = cfgDefault.toBuilder
        b.executionContext = exec
        b.build
      }
//      ctlConfig.materializer = ActorMaterializer()(ctlConfig.actorSystem)
      implicit val control: Control = Control(ctlConfig)
      import context.cursor
      val uState      = ugb.tryBuild(g) // UGB.build(ugb, g)

      if (GUI_DEBUG) {
        tx.afterCommit(Swing.onEDT(SimpleGUI(control)))
      }

      if (DEBUG) debugPrint(s"reader(); uState = $uState")

      val rendering = RenderingImpl.withState[S](uState, force = true)
      // XXX TODO --- we observed many retries of the transaction.
      // That may point to a conflict with the execution context and
      // the mapping of the future. Should we enforce SoundProcesses.executionContext?
      val promise = Promise[Reader]()

      // XXX TODO --- this is quite ugly and tricky; should we force Rendering
      // to provide a future that we can flatMap?
      rendering.reactNow { implicit tx => {
        case Rendering.Completed =>
          val fut: Future[Reader] = try {
            val cvOT  = rendering.cacheResult
            if (DEBUG) debugPrint(s"cvOT $cvOT")
            val cv    = cvOT.get.get
            val ncFile :: Nil = cv.resources // ugb.cacheFiles
            val tKey = TransparentKey(file = ncFile, name = name, streamDim = key.streamDim, section = key.section)
            if (DEBUG) debugPrint(s"tKey $tKey")
            val tFact = new Transparent[S](tKey)
            tFact.reader()
          } catch {
            case NonFatal(ex) =>
              if (DEBUG) debugPrint(s"cv failed $ex")
              Future.failed(ex)
          }
          tx.afterCommit(promise.completeWith(fut))

        case _ =>
      }}

      promise.future
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