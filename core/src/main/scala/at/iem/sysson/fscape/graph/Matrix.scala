/*
 *  Matrix.scala
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

package at.iem.sysson
package fscape
package graph

import de.sciss.fscape.UGen.Aux
import de.sciss.fscape.graph.{ConstantI, ConstantL}
import de.sciss.fscape.lucre.{UGenGraphBuilder => UGB}
import de.sciss.fscape.stream.{StreamIn, StreamOut, Builder => SBuilder}
import de.sciss.fscape.{GE, Lazy, UGen, UGenGraph, UGenIn, UGenInLike, UGenSource, stream}
import de.sciss.lucre.matrix.{Vec, Matrix => LMatrix}
import de.sciss.serial.{DataOutput, ImmutableSerializer, Writable}

import scala.concurrent.Future

object Matrix {
  object ValueSeq {
    trait Value extends UGB.Value with Aux {
      def matrix: LMatrix.Key
      def reader: Future[LMatrix.Reader]

      final def write(out: DataOutput): Unit = {
        out.writeByte(100)
        matrix.write(out)
      }
    }

    final case class WithRef(ref: Value) extends UGenSource.SingleOut {

      protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike =
        makeUGen(Vector.empty)

      protected def makeUGen(args: Vec[UGenIn])(implicit b: UGenGraph.Builder): UGenInLike =
        UGen.SingleOut(this, args, aux = ref :: Nil)

      def makeStream(args: Vec[StreamIn])(implicit b: SBuilder): StreamOut = {
        val reader = ref.reader
        // println(s"makeStream. size = ${reader.size}")
        stream.MatrixValueSeq(matrix = reader)
      }

      override def productPrefix: String = s"Matrix$$PlayLinear$$WithRef"
    }
  }
  final case class ValueSeq(variable: Matrix) extends GE.Lazy with UGB.Input {
    type Key    = Matrix
    type Value  = ValueSeq.Value

    def key: Key = variable

    override def productPrefix: String  = s"Matrix$$ValueSeq"
    override def toString               = s"$variable.valueSeq"

    def isFill: GE = IsFill(variable, this)

    protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = {
      val ub    = UGB.get(b)
      val value = ub.requestInput(this)
      // println(s"streamDim = ${value.matrix.streamDim}")
      ValueSeq.WithRef(value)
    }
  }

  object ValueWindow {
    trait Value extends UGB.Value with Aux {
      def matrix  : LMatrix.Key
      def reader  : Future[LMatrix.Reader]
      def winSize : Long
      def dims    : List[Int]

      final def write(out: DataOutput): Unit = {
        out.writeByte(105)
        matrix.write(out)
        out.writeLong(winSize)
        val sz = dims.size
        out.writeShort(sz)
        dims.foreach(out.writeShort)
      }
    }

    final case class WithRef(ref: Value) extends UGenSource.SingleOut {

      protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike =
        makeUGen(Vector.empty)

      protected def makeUGen(args: Vec[UGenIn])(implicit b: UGenGraph.Builder): UGenInLike =
        UGen.SingleOut(this, args, aux = ref :: Nil)

      def makeStream(args: Vec[StreamIn])(implicit b: SBuilder): StreamOut = {
        val reader = ref.reader
        // println(s"makeStream. size = ${reader.size}")
        require (ref.winSize < 0x7FFFFFFF, s"Matrix.ValueWindow - window too large (${ref.winSize} exceeds 32-bit)")
        val winSizeI = ref.winSize.toInt
        stream.MatrixValueWindow(matrix = reader, winSize = winSizeI, dims = ref.dims)
      }

      override def productPrefix: String = s"Matrix$$PlayLinear$$WithRef"
    }
  }
  final case class ValueWindow(variable: Matrix, dims: Vec[Dim]) extends GE.Lazy with UGB.Input {
    type Key    = Matrix
    type Value  = ValueWindow.Value

    def key: Key = variable

    override def productPrefix: String  = s"Matrix$$ValueWindow"
    override def toString               = s"$variable.valueWindow(${dims.mkString(", ")})"

    def isFill: GE = IsFill(variable, this)

    protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = {
      val ub    = UGB.get(b)
      val value = ub.requestInput(this)
      ValueWindow.WithRef(value)
    }
  }

  object Op {
    final case class Drop(dim: Dim) extends Op {
      override def productPrefix: String  = s"Matrix$$Op$$Drop"
      override def toString               = s"Drop($dim)"
    }

    final case class MoveLast(dim: Dim) extends Op {
      override def productPrefix: String  = s"Matrix$$Op$$MoveLast"
      override def toString               = s"MoveLast($dim)"
    }

    final case class Append(dim: Dim.Def) extends Op {
      override def productPrefix: String  = s"Matrix$$Op$$Append"
      override def toString               = s"Append($dim)"
    }

    final case class Reduce(dim: Dim) extends Op {
      override def productPrefix: String  = s"Matrix$$Op$$Reduce"
      override def toString               = s"Reduce($dim)"
    }
  }
  sealed trait Op

  object Spec {
    sealed trait Dim extends Writable {
      def name  : String
      def units : String
      def size  : Int

      def values: Future[Vec[Double]]

      override def toString = s"Dim($name, units = $units, size = $size)"
    }

    final case class DimConst(name: String, units: String, valuesC: Vec[Double])
      extends Dim {

      override def productPrefix: String = s"Matrix$$Spec$$DimConst"

      def size: Int = valuesC.size

      def values: Future[Vec[Double]] = Future.successful(valuesC)

      def write(out: DataOutput): Unit = {
        out.writeByte(106)
        out.writeUTF(name )
        out.writeUTF(units)
        ImmutableSerializer.indexedSeq[Double].write(valuesC, out)
      }
    }

    final case class DimRead(name: String, units: String, size: Int, key: LMatrix.Key, values: Future[Vec[Double]])
      extends Dim {

      override def productPrefix: String = s"Matrix$$Spec$$DimRead"

//      def values: Future[Vec[Double]] = reader.map { r =>
//        val lenL  = r.size
//        require(lenL <= 0x7FFFFFFF)
//        val len   = lenL.toInt
//        val buf   = new Array[Double](len)
//        r.readDouble1D(buf, 0, len)
//        buf.toIndexedSeq
//      }

      def write(out: DataOutput): Unit = {
        out.writeByte(103)
        out.writeUTF(name )
        out.writeUTF(units)
        out.writeInt(size)
        key.write(out)
      }
    }

    final case class Value(name: String, units: String, dimensions: Vec[Spec.Dim])
      extends UGB.Value with Aux {

//      lazy val shape : Vec[Int]  = dimensions.map(_.values.size)
      lazy val shape : Vec[Int]  = dimensions.map(_.size)
      lazy val rank  : Int       = shape.size
      lazy val size  : Long      = if (shape.isEmpty) 0L else (1L /: shape)(_ * _)

      def write(out: DataOutput): Unit = {
        out.writeByte(102)
        out.writeUTF(name )
        out.writeUTF(units)
        out.writeShort(dimensions.size)
        dimensions.foreach(_.write(out))
      }
    }
  }
  final case class Spec(variable: Matrix, ops: Vec[Op]) extends UGB.Input with UGB.Key {
    type Key    = Spec
    type Value  = Spec.Value

    def key: Key = this

    def drop    (dim: Dim    ): Spec = copy(ops = ops :+ Op.Drop    (dim))
    def moveLast(dim: Dim    ): Spec = copy(ops = ops :+ Op.MoveLast(dim))
    def append  (dim: Dim.Def): Spec = copy(ops = ops :+ Op.Append  (dim))
    /** Like `drop` but keeps the dimension at size 1 */
    def reduce  (dim: Dim    ): Spec = copy(ops = ops :+ Op.Reduce  (dim))

    def size: GE = SpecSize(this)

    private def unCapitalize(s: String): String =
      if (s.isEmpty || s.charAt(0).isLower) s
      else {
        val chars = s.toCharArray
        chars(0) = chars(0).toLower
        new String(chars)
      }

    override def productPrefix: String  = s"Matrix$$Spec"
    override def toString     : String  = (s"$variable.spec" /: ops)((res, op) => s"$res.${unCapitalize(op.toString)}")
  }

  trait InfoLike {
    def matrix: LMatrix.Key

    def shape : Vec[Int]
    def name  : String
    def units : String

    final def rank: Int   = shape.size
    final def size: Long  = if (shape.isEmpty) 0L else (1L /: shape)(_ * _)
  }

  trait Info extends InfoLike with UGB.Value with Aux {
    final def write(out: DataOutput): Unit = {
      out.writeByte(101)
      matrix.write(out)
    }
  }

  sealed trait InfoGE extends GE.Lazy with UGB.Input with UGB.Key {
    type Key      = InfoGE
    type Value    = Info

    def variable: Matrix

    final def key: Key  = this
  }

  final case class SpecSize(spec: Spec) extends GE.Lazy {
    type Key    = Spec
    type Value  = Spec.Value

    override def productPrefix: String  = s"Matrix$$SpecSize"
    override def toString               = s"$spec.size"

//    def variable: Matrix = spec.variable

    protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = {
      val ub    = UGB.get(b)
      val value = ub.requestInput(spec)
      ConstantL(value.size)
    }
  }

  final case class Size(variable: Matrix) extends InfoGE {

    override def productPrefix: String  = s"Matrix$$Size"
    override def toString               = s"$variable.size"

    protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = {
      val ub    = UGB.get(b)
      val value = ub.requestInput(this)
      ConstantL(value.size)
    }
  }

  final case class Rank(variable: Matrix) extends InfoGE {

    override def productPrefix: String  = s"Matrix$$Rank"
    override def toString               = s"$variable.size"

    protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = {
      val ub    = UGB.get(b)
      val value = ub.requestInput(this)
      ConstantI(value.rank)
    }
  }
}
final case class Matrix(name: String) extends Lazy.Expander[Unit] with UGB.Key {
  protected def makeUGens(implicit b: UGenGraph.Builder): Unit = ()

  def size: Matrix.Size = Matrix.Size(this)
  def rank: Matrix.Rank = Matrix.Rank(this)

  /** Unrolls all dimensions in time. */
  def valueSeq               : Matrix.ValueSeq    = Matrix.ValueSeq   (this)
  def valueWindow(dims: Dim*): Matrix.ValueWindow = Matrix.ValueWindow(this, dims.toIndexedSeq)

  def spec: Matrix.Spec = Matrix.Spec(this, Vector.empty)
}