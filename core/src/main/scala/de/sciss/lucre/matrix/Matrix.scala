/*
 *  Matrix.scala
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

import de.sciss.lucre.event.Publisher
import de.sciss.lucre.matrix.impl.{MatrixImpl => Impl}
import de.sciss.lucre.stm.Obj
import de.sciss.model.Change
import de.sciss.serial.{DataInput, ImmutableSerializer, Serializer, Writable}

object Matrix extends Obj.Type {
  // ---- Elem.Type ----

  final val typeID = 0x30001

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

  // ---- variables ----

  object Var {
    def apply[S <: Sys[S]](init: Matrix[S])(implicit tx: S#Tx): Var[S] = impl.MatrixVarImpl(init)

    def unapply[S <: Sys[S], A](matrix: Matrix[S]): Option[Var[S]] =
      if (matrix.isInstanceOf[Var[_]]) Some(matrix.asInstanceOf[Var[S]]) else None

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Var[S]] = impl.MatrixVarImpl.serializer[S]

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

  /** A reader is a two dimensional view of the matrix, where time
    * proceeds along the streaming dimension used when calling `getKey`.
    * The streaming dimension is reflected by the number of frames.
    *
    * The reader is stateful and remembers the number of frames read
    * (current position). When created, the reader's position is zero.
    * If one tries to read more than `numFrames` frames, an exception
    * will be thrown.
    */
  trait Reader {
    /** The number of frames which is the size of the streaming dimension,
      * or `1` if no streaming is used.
      */
    def numFrames: Long

    /** The number of channels is the matrix size divided by the number of frames. */
    def numChannels: Int

    def size: Long

    /** Reads a chunk of matrix data into a provided buffer. If the stream-transposed
      * matrix has more than two dimensions, the de-interleaving is regularly from
      * from to back. E.g. in a matrix of shape `[a][b][c]`, if `a` is the streaming
      * dimension, the first channel is `b0, c0`, the second channel is `b0, c1`,
      * and so on until `b0, ck-1` where `k` is the size of third dimension, followed
      * by `b1, c0` etc.
      *
      * @param buf  the buffer to read into. This must be two-dimensional with the
      *             outer dimension corresponding to channels, and the inner arrays
      *             having at least a size of `off + len`.
      * @param off  the offset into each channel of `buf`
      * @param len  the number of frames to read
      */
    def readFloat2D (buf: Array[Array[Float]], off: Int, len: Int): Unit

    def readDouble1D(buf: Array[Double]      , off: Int, len: Int): Unit
  }

  // ---- key ----

  object Key {
    def read(in: DataInput): Key = impl.KeyImpl.read(in)

    implicit def serializer: ImmutableSerializer[Key] = impl.KeyImpl.serializer
  }
  trait Key extends Writable {
    def streamDim: Int

    def isStreaming: Boolean = streamDim >= 0

    /** Creates a reader instance that can the be used to retrieve the actual matrix data.
      *
      * @param resolver   the resolver is used for matrices backed up by NetCDF files.
      */
    def reader[S <: Sys[S]]()(implicit tx: S#Tx, resolver: DataSource.Resolver[S]): Matrix.Reader
  }

  // ---- serialization ----

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Matrix[S]] /* with evt.Reader[S, Matrix[S]] */ =
    Impl.serializer[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Matrix[S] = serializer[S].read(in, access)
}
trait Matrix[S <: Sys[S]] extends Obj[S] with Publisher[S, Matrix.Update[S]] {
  final def tpe: Obj.Type = Matrix

  // def mkCopy()(implicit tx: S#Tx): Matrix[S]

  /** A matrix has a name. For example, when coming from a NetCDF data source,
    * the matrix name corresponds to a variable name.
    */
  def name(implicit tx: S#Tx): String

  /** The units of the matrix cells, or an empty string to indicate that no units are specified. */
  def units(implicit tx: S#Tx): String

  /** The rank is the number of dimensions. */
  def rank(implicit tx: S#Tx): Int    = shape.size

  /** The size is the number of matrix cells, that is the product of the shape. */
  def size(implicit tx: S#Tx): Long   = (1L /: shape)(_ * _)

  /** The shape is the vector of dimensional sizes. This is equivalent
    * to `ranges.map(_.size)`. Note that the `dimensions` themselves
    * may be larger.
    */
  def shape(implicit tx: S#Tx): Vec[Int]

  // def fillValue(implicit tx: S#Tx): Double

  /** The sequence of underlying dimension matrices. The part of each dimension
    * covered by this matrix is reflected by the corresponding entry in `ranges`!
    *
    * In other words, the reductions and transformations associated with the
    * current (and possibly any number of preceding) matrices are not reflected
    * in the returned objects! To read the dimensional values with respect to
    * the actual matrix coordinates, the `getDimensionKey` method can be used.
    */
  def dimensions(implicit tx: S#Tx): Vec[Matrix[S]]

  /** Produces a matrix key for the dimension of a given index. Since a
    * dimension is 1-dimensional, the key will either have a streaming-index of
    * zero (when `useChannels` is `false`), resulting in a 1-channel reader
    * with `shape(index)` frames; or it will have a streaming-index of `-1`
    * (when `useChannels` is `true`), resulting in an n-channel reader with
    * one frame, where `n == shape(index)`.
    *
    * @param index        the index of the dimension, from zero until `rank`
    * @param useChannels  if `true` produces multi-channel file of one frame,
    *                     if `false` produces monophonic file of several frames.
    */
  def getDimensionKey(index: Int, useChannels: Boolean)(implicit tx: S#Tx): Matrix.Key

  /** The ranges specify the regions inside the underlying dimension matrices
    * covered by this matrix. For example if a 4 x 5 matrix is reduced in its
    * first dimension using a slice 1 to 2 operator, then the _dimension_
    * will have a size of four, but the first range will be 1 to 2 (and thus
    * size 2).
    */
  def ranges(implicit tx: S#Tx): Vec[Range]

  def reducedRank       (implicit tx: S#Tx): Int            = shape.count (_ > 1)
  def reducedShape      (implicit tx: S#Tx): Vec[Int]       = shape.filter(_ > 1)
  // def reducedDimensions(implicit tx: S#Tx): Vec[Dimension.Value]  = reduce(dimensions)
  def reducedRanges     (implicit tx: S#Tx): Vec[Range]     = reduce(ranges)

  def reducedDimensions(implicit tx: S#Tx): Vec[Matrix[S]]  = reduce(dimensions)

  private def reduce[A](coll: Vec[A])(implicit tx: S#Tx): Vec[A] =
    (coll zip shape).collect { case (x, sz) if sz > 1 => x }

  private[matrix] def debugFlatten(implicit tx: S#Tx): Vec[Double]

  def reader(streamDim: Int)(implicit tx: S#Tx, resolver: DataSource.Resolver[S]): Matrix.Reader =
    getKey(streamDim).reader()

  /** The key of a matrix is an immutable value that represents its current state,
    * possibly prepared with a transposition to be streamed along one of its dimensions.
    *
    * @param streamDim  the index of the dimension to stream the matrix data through, or `-1`
    *                   to read the whole matrix in one frame.
    */
  def getKey(streamDim: Int)(implicit tx: S#Tx): Matrix.Key
}