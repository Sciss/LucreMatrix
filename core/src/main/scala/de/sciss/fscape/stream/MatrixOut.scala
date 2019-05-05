/*
 *  MatrixOut.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.fscape
package stream

import java.util

import akka.stream.stage.{GraphStageLogic, InHandler, OutHandler}
import akka.stream.{Attributes, FlowShape}
import at.iem.sysson.fscape.graph.Matrix
import de.sciss.file._
import de.sciss.fscape.stream.impl.{BlockingGraphStage, NodeHasInitImpl, NodeImpl}
import de.sciss.lucre.matrix.Vec
import de.sciss.lucre.matrix.impl.ReaderImpl
import ucar.ma2.DataType
import ucar.nc2.NetcdfFileWriter
import ucar.nc2.constants.CDM
import ucar.{ma2, nc2}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object MatrixOut {
//  private def log(what: => String): Unit =
//    println(s"[DEBUG] $what")

  import de.sciss.fscape.{logStream => log}

  def apply(file: File, spec: Matrix.Spec.Value, in: OutD)(implicit b: Builder): OutL = {
    val source  = new Stage(b.layer, file, spec)
    val stage   = b.add(source)
    b.connect(in, stage.in)
    stage.out
  }

  private final val name = "MatrixOut"

  private type Shape = FlowShape[BufD, BufL]

  private final class Stage(layer: Layer, file: File, spec: Matrix.Spec.Value)(implicit ctrl: Control)
    extends BlockingGraphStage[Shape](s"$name($file)") {

    val shape = FlowShape(InD(s"$name.in"), OutL(s"$name.out"))

    def createLogic(attr: Attributes): NodeImpl[Shape] =
      new Logic(layer, shape, file, spec)
  }

  private final class Logic(layer: Layer, shape: Shape, protected val file: File, protected val spec: Matrix.Spec.Value)
                           (implicit ctrl: Control)
    extends NodeImpl(s"$name($file)", layer, shape) with AbstractLogic

  trait AbstractLogic extends Node with NodeHasInitImpl with InHandler with OutHandler { logic: GraphStageLogic =>
    // ---- abstract ----

    protected def file : File
    protected def spec : Matrix.Spec.Value

    def shape: Shape

    // ---- impl ----

    private[this] var framesRead  = 0L
    private[this] val numFrames   = spec.size
    private[this] var _isSuccess  = false
    private[this] val matShape    = spec.shape.toArray
    private[this] val rank        = matShape.length
    private[this] val arrShape    = new Array[Int](rank)
    private[this] val origin      = new Array[Int](rank)
    private[this] var outClosed   = false

    private[this] var writer: NetcdfFileWriter  = _
    private[this] var outVar: nc2.Variable      = _

    setHandler(shape.in , this)
    setHandler(shape.out, this)

    protected final def isSuccess: Boolean  = _isSuccess

    final def onPull(): Unit =
      if (isAvailable(shape.in) && writer != null) {
        process()
      }

    final def onPush(): Unit =
      if ((outClosed || isAvailable(shape.out)) && writer != null) {
        process()
      }

    override def onUpstreamFinish(): Unit = {
      val av = isAvailable(shape.in)
      log(s"onUpstreamFinish() $av $this")
      if (!av) {
        super.onUpstreamFinish()
      }
    }

    // ignore if frames-written observation stops
    override def onDownstreamFinish(): Unit = {
      log(s"onDownstreamFinish() $this")
      outClosed = true
      onPull()
    }

//    private def tryProcess(): Unit =
//      if (isAvailable(shape.in) && isAvailable(shape.out) && writer != null) {
//        process()
//      }

    override protected def init(): Unit = {
      super.init()

      val ctrl = control
      import ctrl.config.executionContext

      val dimsFutT = spec.dimensions.map(_.values)
      val dimsFut: Future[Vec[Vec[Double]]] = Future.sequence(dimsFutT)

      // in order for the asynchronous wait to work,
      // we must make sure to _not_ call `pull` immediately,
      // otherwise the outside graph might terminate
      // prematurely. we have to call `pull` after the future has
      // completed.

      val callback = getAsyncCallback[Try[Vec[Vec[Double]]]] {
        case Success(dimsData) =>
          log(s"callback.success $this")
          writer = NetcdfFileWriter.createNew(NetcdfFileWriter.Version.netcdf3, file.path, null)
          val varDims  = new util.ArrayList[nc2.Dimension](rank)
          val varName  = spec.name
          var varIsDim = false
          val dimsVars: Vec[nc2.Variable] = spec.dimensions.map { dimSpec =>
            val dim   = writer.addDimension(null, dimSpec.name, dimSpec.size)
            val dimL  = new util.ArrayList[nc2.Dimension](1)
            dimL.add(dim)
            varDims.add(dim)
            val dimName = dimSpec.name
            if (dimName == varName) varIsDim = true

            val dimVar  = writer.addVariable(null, dimSpec.name, DataType.DOUBLE, dimL)
            if (!dimSpec.units.isEmpty)
              dimVar.addAttribute(new nc2.Attribute(CDM.UNITS, dimSpec.units))

            dimVar
          }

          outVar = if (varIsDim) {
            writer.findVariable(varName)
          } else {
            writer.addVariable(null, varName, DataType.DOUBLE, varDims)
          }

          if (outVar == null) {
            throw new IllegalStateException(s"Trying to define variable $varName twice")
          }

          if (!spec.units.isEmpty)
            outVar.addAttribute(new nc2.Attribute(CDM.UNITS, spec.units))

          // create the file; ends "define mode"
          writer.create()

          (dimsVars zip dimsData).foreach { case (dimVar, dimData) =>
            val arr = ma2.Array.factory(dimData.toArray)
            writer.write(dimVar, arr)
          }

          if (isAvailable(shape.in) && (outClosed || isAvailable(shape.out))) process()

        case Failure(ex) =>
          log(s"callback.failure $this")
          failStage(ex)
      }

      // NOT:
//      pull(shape.in)

      dimsFut.onComplete(callback.invoke)
    }

    override protected def stopped(): Unit = {
      if (!_isSuccess && writer != null) writer.abort()
      super.stopped()
    }

    private def process(): Unit = {
      val bufIn   = grab(shape.in)
      val chunk = math.min(bufIn.size, numFrames - framesRead).toInt
      if (chunk > 0) {
        var _framesRead = framesRead
        var off         = 0

        ReaderImpl.partition(matShape, _framesRead, _framesRead + chunk) { ranges =>
          val _arrShape = arrShape
          val _origin   = origin
          var i = 0
          var len = 1
          while (i < _arrShape.length) {
            val r  = ranges(i)
            val sh = r.size
            _arrShape(i) = sh
            _origin  (i) = r.start
            len *= sh
            i   += 1
          }
          val arr   = ma2.Array.factory(DataType.DOUBLE, _arrShape)
          val arrD  = arr.getStorage.asInstanceOf[Array[Double]]
          System.arraycopy(bufIn.buf, off, arrD, 0, len)
          writer.write(outVar, _origin, arr)
          off += len
        }

        if (outClosed) {
          _framesRead += chunk

        } else {
          off = 0
          val bufOut  = control.borrowBufL()
          val out = bufOut.buf
          while (off < chunk) {
            _framesRead += 1
            out(off) = _framesRead
            off += 1
          }

          bufOut.size = chunk
          push(shape.out, bufOut)
        }

        framesRead = _framesRead
      }

      bufIn.release()

      if (framesRead == numFrames) {
        log(s"completeStage() success $this")
        writer.close()
        _isSuccess = true
        completeStage()

      } else {
        if (isClosed(shape.in)) {
          log(s"completeStage() closed $this")
          completeStage()
        } else {
          pull(shape.in)
        }
      }
    }
  }
}