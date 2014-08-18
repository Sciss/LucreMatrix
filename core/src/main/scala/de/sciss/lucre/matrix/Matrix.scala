/*
 *  Matrix.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre
package matrix

import de.sciss.lucre.{event => evt}
import evt.Publisher
import de.sciss.serial.{ImmutableSerializer, DataInput, Writable, Serializer}
import stm.Disposable
import de.sciss.model.Change
import impl.{MatrixImpl => Impl}

object Matrix {
  final val typeID = 0x30001

  // ---- variables ----

  object Var {
    def apply[S <: Sys[S]](init: Matrix[S])(implicit tx: S#Tx): Var[S] = impl.MatrixVarImpl(init)

    def unapply[S <: Sys[S], A](matrix: Matrix[S]): Option[Var[S]] =
      if (matrix.isInstanceOf[Var[_]]) Some(matrix.asInstanceOf[Var[S]]) else None

    implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Var[S]] = impl.MatrixVarImpl.serializer[S]

    object Update {
      case class Changed[S <: Sys[S]](matrix: Var[S], change: Change[Matrix[S]]) extends Var.Update[S]
      case class Element[S <: Sys[S]](matrix: Var[S], update: Matrix.Update[S])  extends Var.Update[S]
    }
    sealed trait Update[S <: Sys[S]] extends Matrix.Update[S] {
      def matrix: Var[S]
    }
  }
  trait Var[S <: Sys[S]] extends Matrix[S] with matrix.Var[S, Matrix[S]] with Publisher[S, Var.Update[S]]

  // ---- events ----

  object Update {
    case class Generic[S <: Sys[S]](matrix: Matrix[S]) extends Update[S]
  }
  sealed trait Update[S <: Sys[S]] {
    def matrix: Matrix[S]
  }

  // ---- reader ----

  trait Reader {
    def numFrames: Long
    def numChannels: Int
    def read(buf: Array[Array[Float]], off: Int, len: Int): Unit
  }

  // ---- key ----

  object Key {
    def read(in: DataInput): Key = impl.KeyImpl.read(in)

    implicit def serializer: ImmutableSerializer[Key] = impl.KeyImpl.serializer
  }
  trait Key extends Writable {
    def reader[S <: Sys[S]]()(implicit tx: S#Tx, resolver: DataSource.Resolver[S]): Matrix.Reader
  }

  // ---- serialization ----

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Matrix[S]] with evt.Reader[S, Matrix[S]] =
    Impl.serializer[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Matrix[S] = serializer[S].read(in, access)
}
trait Matrix[S <: Sys[S]] extends Writable with Disposable[S#Tx] with Publisher[S, Matrix.Update[S]] {
  def name(implicit tx: S#Tx): String

  def rank(implicit tx: S#Tx): Int    = shape.size
  def size(implicit tx: S#Tx): Long   = (1L /: shape)(_ * _)

  def shape(implicit tx: S#Tx): Vec[Int]

  def dimensions(implicit tx: S#Tx): Vec[Dimension.Value]

  def ranges(implicit tx: S#Tx): Vec[Range] // XXX TODO: this might get problematic with averaging reductions

  def reducedRank      (implicit tx: S#Tx): Int                   = shape.count (_ > 1)
  def reducedShape     (implicit tx: S#Tx): Vec[Int]              = shape.filter(_ > 1)
  def reducedDimensions(implicit tx: S#Tx): Vec[Dimension.Value]  = reduce(dimensions)
  def reducedRanges    (implicit tx: S#Tx): Vec[Range]            = reduce(ranges)

  private def reduce[A](coll: Vec[A])(implicit tx: S#Tx): Vec[A] =
    (coll zip shape).collect { case (x, sz) if sz > 1 => x }

  private[matrix] def debugFlatten(implicit tx: S#Tx): Vec[Double]

  def reader(streamDim: Int)(implicit tx: S#Tx, resolver: DataSource.Resolver[S]): Matrix.Reader =
    getKey(streamDim).reader()

  def getKey(streamDim: Int)(implicit tx: S#Tx): Matrix.Key
}