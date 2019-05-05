/*
 *  MatrixValueWindow.scala
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

import akka.stream.{Attributes, SourceShape}
import de.sciss.fscape.stream.impl.{BlockingGraphStage, MatrixValueImpl, NodeImpl}
import de.sciss.lucre.matrix.Matrix

import scala.collection.immutable.{Seq => ISeq}
import scala.concurrent.Future

object MatrixValueWindow {
  def apply(matrix: Future[Matrix.Reader], winSize: Int, dims: ISeq[Int])(implicit b: Builder): OutD = {
    val source  = new Stage(b.layer, matrix, winSize = winSize, dims = dims.toArray)
    val stage   = b.add(source)
    stage.out
  }

  private final val name = "MatrixValueWindow"

  private type Shape = SourceShape[BufD]

  private final class Stage(layer: Layer, matrix: Future[Matrix.Reader], winSize: Int, dims: Array[Int])
                           (implicit ctrl: Control)
    extends BlockingGraphStage[Shape](s"$name($matrix)") {

    val shape = SourceShape(OutD(s"$name.out"))

    def createLogic(attr: Attributes): NodeImpl[Shape] =
      new Logic(layer, shape, matrix, winSize = winSize, dims = dims)
  }

  private final class Logic(layer: Layer, shape: Shape, matrixF: Future[Matrix.Reader], winSize: Int, dims: Array[Int])
                           (implicit ctrl: Control)
    extends MatrixValueImpl(name, layer, shape, matrixF) {

    private[this] val bufSize: Int = ctrl.blockSize
    private[this] var winBuf = new Array[Double](winSize)
    private[this] var bufOff: Int = winSize

    private[this] var framesRead  = 0L

    override protected def stopped(): Unit = {
      super.stopped()
      winBuf = null
    }

    protected def process(matrix: Matrix.Reader): Unit = {
      val chunk = math.min(bufSize, matrix.size - framesRead).toInt
      if (chunk == 0) {
        logStream(s"completeStage() $this")
        completeStage()
      } else {
        val bufOut  = ctrl.borrowBufD()
        val _out    = bufOut.buf
        var outOff  = 0
        var remain  = chunk
        while (remain > 0) {
          val chunk1 = math.min(remain, winSize - bufOff)
          if (chunk1 > 0) {
            System.arraycopy(winBuf, bufOff, _out, outOff, chunk1)
            outOff += chunk1
            bufOff += chunk1
            remain -= chunk1
          }
          if (remain > 0) {
            assert(bufOff == winSize)
            matrix.readWindowDouble1D(dims = dims, buf = winBuf, off = 0)
            bufOff = 0
          }
        }
        bufOut.size = chunk
        framesRead += chunk
        push(shape.out, bufOut)
      }
    }
  }
}