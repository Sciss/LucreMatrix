/*
 *  MatrixVarImpl.scala
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

package de.sciss.lucre
package matrix
package impl

import de.sciss.lucre.event.Targets
import de.sciss.lucre.stm.{Copy, Elem, NoSys}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.synth.proc.GenContext

import scala.concurrent.{ExecutionContext, Future}

object MatrixVarImpl {
  def apply[S <: Sys[S]](init: Matrix[S])(implicit tx: S#Tx): Matrix.Var[S] = {
    val targets = Targets[S]
    val ref     = tx.newVar(targets.id, init)
    new Impl[S](targets, ref).connect()
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Matrix.Var[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  private final class Ser[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Matrix.Var[S]] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Matrix.Var[S] =
      Matrix.read(in, access) match {
        case mv: Matrix.Var[S] => mv
        case other => sys.error(s"Type mismatch, expected Matrix.Var but got $other")
      }

    def write(v: Matrix.Var[S], out: DataOutput): Unit = v.write(out)
  }

  private[matrix] def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                                 (implicit tx: S#Tx): Matrix.Var[S] = {
    val ref = tx.readVar[Matrix[S]](targets.id, in)
    new Impl[S](targets, ref)
  }

  private final class Impl[S <: Sys[S]](protected val targets: Targets[S],
                                        protected val ref: S#Var[Matrix[S]])
    extends Matrix.Var[S]
    with MatrixProxy[S] with VarImpl[S, Matrix.Update[S], Matrix[S], Matrix.Var.Update[S]] {

//    def mkCopy()(implicit tx: S#Tx): Matrix[S] = {
//      val tgt     = Targets[S]
//      val peerCpy = tx.newVar(tgt.id, ref().mkCopy())
//      new Impl(tgt, peerCpy)
//    }

    def copy[Out <: stm.Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      MatrixVarImpl(context(ref()))
//      val targetsOut  = Targets[Out]
//      val refOut      = tx.newVar
//      val ref     = tx.newVar(targets.id, init)
//      new Impl[S](targets, ref).connect()
    }

    protected def matrixPeer(implicit tx: S#Tx): Matrix[S] = ref()

    //    def reader(streamDim: Int)(implicit tx: S#Tx, resolver: Resolver[S]): Reader = matrixPeer.reader(streamDim)

    def prepareReader(streamDim: Int)(implicit tx: S#Tx): Matrix.ReaderFactory[S] =
      matrixPeer.prepareReader(streamDim)

    def prepareDimensionReader(index: Int, useChannels: Boolean)(implicit tx: S#Tx): Matrix.ReaderFactory[S] =
      matrixPeer.prepareDimensionReader(index = index, useChannels = useChannels)

    def debugFlatten(implicit tx: S#Tx, resolver: DataSource.Resolver[S], exec: ExecutionContext,
                     context: GenContext[S]): Future[Vec[Double]] =
      matrixPeer.debugFlatten

    def shape     (implicit tx: S#Tx): Vec[Int]           = matrixPeer.shape
    def ranges    (implicit tx: S#Tx): Vec[Option[Range]] = matrixPeer.ranges

    // ---- event ----

    protected def mapUpdate(in: Matrix.Update[S]): Matrix.Var.Update[S] =
      Matrix.Var.Update.Element(this, in)

    protected def mkUpdate(before: Matrix[S], now: Matrix[S]): Matrix.Var.Update[S] =
      Matrix.Var.Update.Changed(this, Change(before, now))

    // protected def reader: evt.Reader[S, Matrix[S]] = Matrix.serializer
  }
}
