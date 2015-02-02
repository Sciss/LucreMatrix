/*
 *  MatrixVarImpl.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 by Hanns Holger Rutz.
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

import de.sciss.serial.DataInput
import de.sciss.lucre.{event => evt}
import de.sciss.model.Change

object MatrixVarImpl {
  def apply[S <: Sys[S]](init: Matrix[S])(implicit tx: S#Tx): Matrix.Var[S] = {
    val targets = evt.Targets[S]
    val ref     = tx.newVar(targets.id, init)
    new Impl[S](targets, ref)
  }

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Matrix.Var[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[evt.InMemory]

  private final class Ser[S <: Sys[S]] extends evt.EventLikeSerializer[S, Matrix.Var[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Matrix.Var[S] = {
      val cookie = in.readByte()
      require(cookie == 0, s"Unexpected cookie (found $cookie, expected 0)")
      readIdentified(in, access, targets)
    }

    def readConstant(in: DataInput)(implicit tx: S#Tx): Matrix.Var[S] =
      sys.error("Unsupported operation: constant matrix variable")
  }

  private[matrix] def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                 (implicit tx: S#Tx): Matrix.Var[S] = {
    val ref = tx.readVar[Matrix[S]](targets.id, in)
    new Impl[S](targets, ref)
  }

  private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                        protected val ref: S#Var[Matrix[S]])
    extends Matrix.Var[S]
    with MatrixProxy[S] with VarImpl[S, Matrix.Update[S], Matrix[S], Matrix.Var.Update[S]] {

    def mkCopy()(implicit tx: S#Tx): Matrix[S] = {
      val tgt     = evt.Targets[S]
      val peerCpy = tx.newVar(tgt.id, ref().mkCopy())
      new Impl(tgt, peerCpy)
    }

    protected def matrixPeer(implicit tx: S#Tx): Matrix[S] = ref()

    //    def reader(streamDim: Int)(implicit tx: S#Tx, resolver: Resolver[S]): Reader = matrixPeer.reader(streamDim)

    def getKey(streamDim: Int)(implicit tx: S#Tx): Matrix.Key = matrixPeer.getKey(streamDim)

    def getDimensionKey(index: Int, useChannels: Boolean)(implicit tx: S#Tx): Matrix.Key =
      matrixPeer.getDimensionKey(index = index, useChannels = useChannels)

    def debugFlatten(implicit tx: S#Tx): Vec[Double]  = matrixPeer.debugFlatten

    def shape     (implicit tx: S#Tx): Vec[Int]       = matrixPeer.shape
    def ranges    (implicit tx: S#Tx): Vec[Range]     = matrixPeer.ranges

    // ---- event ----

    protected def mapUpdate(in: Matrix.Update[S]): Matrix.Var.Update[S] =
      Matrix.Var.Update.Element(this, in)

    protected def mkUpdate(before: Matrix[S], now: Matrix[S]): Matrix.Var.Update[S] =
      Matrix.Var.Update.Changed(this, Change(before, now))

    protected def reader: evt.Reader[S, Matrix[S]] = Matrix.serializer
  }
}
