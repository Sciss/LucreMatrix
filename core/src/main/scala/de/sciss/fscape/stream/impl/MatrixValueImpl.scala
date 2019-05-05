/*
 *  MatrixValueImpl.scala
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

package de.sciss.fscape
package stream
package impl

import akka.stream.SourceShape
import akka.stream.stage.OutHandler
import de.sciss.lucre.matrix.Matrix

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

abstract class MatrixValueImpl(name: String, layer: Layer, shape: SourceShape[BufD], matrixF: Future[Matrix.Reader])
                              (implicit ctrl: Control)
  extends NodeImpl(s"$name(${matrixF.value})", layer, shape) with NodeHasInitImpl with OutHandler {

  private[this] var matrix: Matrix.Reader = _

  setHandler(shape.out, this)

  private def setMatrix(m: Matrix.Reader): Unit = {
    matrix = m
    if (isAvailable(shape.out)) process(m)
  }

  override protected def init(): Unit = {
    super.init()
    matrixF.value match {
      case Some(Success(m)) => setMatrix(m)
      case _ =>
        val callback = getAsyncCallback[Try[Matrix.Reader]] {
          case Success(m)   => setMatrix(m)
          case Failure(ex)  => failStage(ex)
        }
        import ctrl.config.executionContext
        matrixF.onComplete(callback.invoke)
    }
  }

  def onPull(): Unit = {
    val m = matrix
    if (m != null) process(m)
  }

  // ---- abstract ----

  protected def process(m: Matrix.Reader): Unit
}