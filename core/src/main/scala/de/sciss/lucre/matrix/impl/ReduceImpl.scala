/*
 *  ReduceImpl.scala
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

import de.sciss.file._
import de.sciss.lucre.event.{EventLike, Targets}
import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.matrix.Dimension.Selection
import de.sciss.lucre.matrix.Reduce.Op
import de.sciss.lucre.matrix.Reduce.Op.Update
import de.sciss.lucre.stm.impl.{ConstElemImpl, ElemSerializer}
import de.sciss.lucre.stm.{Copy, Elem, NoSys}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer, Serializer}
import de.sciss.synth.proc.GenContext
import ucar.{ma2, nc2}

import scala.annotation.{switch, tailrec}
import scala.collection.breakOut
import scala.concurrent.{ExecutionContext, Future}

object ReduceImpl {
  def apply[S <: Sys[S]](in : Matrix[S], dim: Selection[S], op: Op[S])(implicit tx: S#Tx): Reduce[S] = {
    val targets = evt.Targets[S]
    new Impl[S](targets, in, dim, op).connect()
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Reduce[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  def readIdentifiedOp[S <: stm.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Op[S] = {
    def readNode(targets: Targets[S]): Op[S] = {
      val tpe   = in.readInt()
      if (tpe != Op.typeID) sys.error(s"Unexpected type id (found $tpe, expected ${Op.typeID})")
      val opID  = in.readInt()
      (opID: @switch) match {
        case Op.Apply.opID =>
          val index = IntObj.read(in, access)
          new OpApplyImpl[S](targets, index)

        case Op.Slice.opID =>
          val from  = IntObj.read(in, access)
          val to    = IntObj.read(in, access)
          new OpSliceImpl[S](targets, from, to)

        case 2 => // OLD SERIALIZED FORM
          /* val from  = */ IntObj.read(in, access)
          /* val to    = */ IntObj.read(in, access)
          val step  = IntObj.read(in, access)
          new OpStrideImpl[S](targets, /* from = from, to = to, */ step = step)

        case Op.Stride.opID =>
          val step  = IntObj.read(in, access)
          new OpStrideImpl[S](targets, step = step)

        case _ => sys.error(s"Unsupported operator id $opID")
      }
    }

    def readConst(): Op[S] = {
      val cookie = in.readByte()
      if (cookie != 1) sys.error(s"Unexpected cookie (found $cookie, expected 1)")
      val tpe   = in.readInt()
      if (tpe != Op.typeID) sys.error(s"Unexpected type id (found $tpe, expected ${Op.typeID})")
      val opID  = in.readInt()
      (opID: @switch) match {
        case Op.Average.opID =>
          new OpAverageImpl[S]

        case _ => sys.error(s"Unsupported operator id $opID")
      }
    }

    def readIdentifiedOpVar(targets: Targets[S]): Op.Var[S] = {
      val ref = tx.readVar[Op[S]](targets.id, in)
      new OpVarImpl[S](targets, ref)
    }

    (in.readByte(): @switch) match {
      case 0 =>
        val targets = Targets.readIdentified(in, access)
        (in.readByte(): @switch) match {
          case 0      => readIdentifiedOpVar(targets)
          case 1      => readNode           (targets)
          case other  => sys.error(s"Unsupported cookie $other")
        }

      case 3 => readConst()

      case other => sys.error(s"Unsupported cookie $other")
    }
  }

  private final class Ser[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Reduce[S]] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Reduce[S] =
      Matrix.read(in, access) match {
        case r: Reduce[S] => r
        case other => sys.error(s"Type mismatch, expected Reduce, found $other")
      }

    def write(v: Reduce[S], out: DataOutput): Unit = v.write(out)
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Reduce[S] = serializer[S].read(in, access)
  
  private[matrix] def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                 (implicit tx: S#Tx): Reduce[S] = {
    val matrix  = Matrix    .read(in, access)
    val dim     = Selection .read(in, access)
    val op      = Op        .read(in, access)
    new Impl(targets, matrix, dim, op)
  }

  implicit def opSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Op[S]] = anyOpSer.asInstanceOf[OpSer[S]]

  private val anyOpSer = new OpSer[NoSys]

  implicit def opVarSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Op.Var[S]] = anyOpVarSer.asInstanceOf[OpVarSer[S]]

  private val anyOpVarSer = new OpVarSer[NoSys]

  private final class OpSer[S <: Sys[S]] extends ElemSerializer[S, Op[S]] {
    def tpe: Elem.Type = Op
  }

  private final class OpVarSer[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Op.Var[S]] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Op.Var[S] =
      Op.read(in, access) match {
        case ov: Op.Var[S] => ov
        case other => sys.error(s"Type mismatch, expected Op.Var, found $other")
      }

    def write(v: Op.Var[S], out: DataOutput): Unit = v.write(out)
  }

  def applyOpVar[S <: Sys[S]](init: Op[S])(implicit tx: S#Tx): Reduce.Op.Var[S] = {
    val targets = evt.Targets[S]
    val ref     = tx.newVar(targets.id, init)
    new OpVarImpl[S](targets, ref).connect()
  }

  def applyOpApply[S <: Sys[S]](index: IntObj[S])(implicit tx: S#Tx): Op.Apply[S] = {
    val targets = evt.Targets[S]
    new OpApplyImpl[S](targets, index).connect()
  }

  def applyOpSlice[S <: Sys[S]](from: IntObj[S], to: IntObj[S])(implicit tx: S#Tx): Op.Slice[S] = {
    val targets = evt.Targets[S]
    new OpSliceImpl[S](targets, from = from, to = to).connect()
  }

  def applyOpStride[S <: Sys[S]](/* from: IntObj[S], to: IntObj[S], */ step: IntObj[S])
                                (implicit tx: S#Tx): Op.Stride[S] = {
    val targets = evt.Targets[S]
    new OpStrideImpl[S](targets, /* from = from, to = to, */ step = step).connect()
  }

  def applyOpAverage[S <: Sys[S]](implicit tx: S#Tx): Op.Average[S] =
    new OpAverageImpl[S]

  // ---- actual implementations ----

  private final class OpVarImpl[S <: Sys[S]](protected val targets: Targets[S],
                                             protected val ref: S#Var[Op[S]])
    extends Op.Var[S] with VarImpl[S, Op.Update[S], Op[S], Op.Update[S]] {

    def copy[Out <: stm.Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      val targetsOut  = Targets[Out]
      val refOut      = txOut.newVar(targetsOut.id, context(ref()))
      new OpVarImpl[Out](targetsOut, refOut).connect()
    }

    def size(in: Int)(implicit tx: S#Tx): Int = apply().size(in)

    protected def mapUpdate(in: Update[S]): Op.Update[S] = in.copy(op = this)

    protected def mkUpdate(before: Op[S], now: Op[S]): Op.Update[S] = Op.Update(this)

    // protected def reader: evt.Reader[S, Op[S]] = Op.serializer
  }

  private sealed trait OpNativeImpl[S <: Sys[S]]
    extends evt.impl.SingleNode[S, Op.Update[S]] {

    _: Op[S] =>

    // ---- abstract ----

    protected def writeOpData(out: DataOutput): Unit

    protected def disconnect()(implicit tx: S#Tx): Unit

    def map(in: Range)(implicit tx: S#Tx): Range

    // ---- impl ----

    final protected def writeData(out: DataOutput): Unit = {
      out writeByte 1 // cookie
      out writeInt Op.typeID
      writeOpData(out)
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = disconnect()
  }

  private final class OpApplyImpl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                               val index: IntObj[S])
    extends OpNativeImpl[S] with Op.Apply[S] { self =>

    def copy[Out <: stm.Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      val targetsOut  = Targets[Out]
      val indexOut    = context(index)
      new OpApplyImpl(targetsOut, indexOut).connect()
    }

    override def toString = s"Apply$id($index)"

    def size(in: Int)(implicit tx: S#Tx): Int = math.min(in, 1)

    def map(in: Range)(implicit tx: S#Tx): Range = {
      val iv = index.value
      if (iv >= 0 && iv < in.size) {
        val x = in(iv)
        Range.inclusive(x, x)
      } else {
        Range(in.start, in.start) // empty -- which index to choose?
      }
    }

    protected def writeOpData(out: DataOutput): Unit = {
      // out writeByte 1 // cookie
      // out writeInt Op.typeID
      out writeInt Op.Apply.opID
      index write out
    }

    // ---- event ----

    object changed extends Changed {
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Update[S]] =
        pull(index.changed).map(_ => Op.Update(self))
    }

    def connect   ()(implicit tx: S#Tx): this.type = {
      index.changed ---> changed
      this
    }

    protected def disconnect()(implicit tx: S#Tx): Unit = index.changed -/-> changed
  }

  private final class OpSliceImpl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                               val from: IntObj[S], val to: IntObj[S])
    extends OpNativeImpl[S] with Op.Slice[S] { self =>

    def copy[Out <: stm.Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      val targetsOut  = Targets[Out]
      val fromOut     = context(from)
      val toOut       = context(to)
      new OpSliceImpl(targetsOut, fromOut, toOut).connect()
    }

    override def toString = s"Slice$id($from, $to)"

    def size(in: Int)(implicit tx: S#Tx): Int = {
      val lo  = from .value
      val hi  = to   .value
      val lo1 = math.max(0, lo)
      val hi1 = math.min(in, hi + 1)
      val res = hi1 - lo1
      math.max(0, res)
    }

    def map(in: Range)(implicit tx: S#Tx): Range = {
      val lo  = from .value
      val hi  = to   .value
      val by  = lo to hi
      sampleRange(in, by)
    }

    protected def writeOpData(out: DataOutput): Unit = {
      // out writeByte 1   // cookie
      // out writeInt Op.typeID
      out writeInt Op.Slice.opID
      from  write out
      to    write out
    }

    // ---- event ----

    object changed extends Changed {
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Op.Update[S]] = {
        val e0 =       pull.contains(from .changed) && pull(from .changed).isDefined
        val e1 = e0 || pull.contains(to   .changed) && pull(to   .changed).isDefined

        if (e1) Some(Op.Update(self)) else None
      }
    }

    def connect()(implicit tx: S#Tx): this.type = {
      from .changed ---> changed
      to   .changed ---> changed
      this
    }

    protected def disconnect()(implicit tx: S#Tx): Unit = {
      from .changed -/-> changed
      to   .changed -/-> changed
    }
  }

  private final class OpStrideImpl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                                /* val from: IntObj[S], val to: IntObj[S], */ val step: IntObj[S])
    extends OpNativeImpl[S] with Op.Stride[S] { self =>

    def copy[Out <: stm.Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      val targetsOut  = Targets[Out]
      val stepOut     = context(step)
      new OpStrideImpl(targetsOut, stepOut).connect()
    }

    override def toString = s"Stride$id($step)"

    def size(in: Int)(implicit tx: S#Tx): Int = {
      // val lo  = from .value
      // val hi  = to   .value
      val s   = step .value
      // note: in NetCDF, ranges must be non-negative, so
      // we don't check invalid cases here, but simply truncate.
      val lo1 = 0       // math.max(0, lo)
      val hi1 = in - 1  // math.min(in - 1, hi)
      val szm = hi1 - lo1
      val res = szm / s + 1
      math.max(0, res)
    }

    def map(in: Range)(implicit tx: S#Tx): Range = {
      val lo  = 0 // from .value
      val hi  = in.size - 1 // to   .value
      val s   = step .value
      val by  = lo to hi by s
      sampleRange(in, by)
    }

    protected def writeOpData(out: DataOutput): Unit = {
      // out writeByte 1   // cookie
      // out writeInt Op.typeID
      out writeInt Op.Stride.opID
      // from  write out
      // to    write out
      step  write out
    }

    // ---- event ----

    object changed extends Changed {
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Op.Update[S]] = {
        //      val e0 =       pull.contains(from .changed) && pull(from .changed).isDefined
        //      val e1 = e0 || pull.contains(to   .changed) && pull(to   .changed).isDefined
        //      val e2 = e1 || pull.contains(step .changed) && pull(step .changed).isDefined
        //
        //      if (e2) Some(Op.Update(this)) else None
        Some(Op.Update(self))
      }
    }

    def connect()(implicit tx: S#Tx): this.type = {
      // from .changed ---> this
      // to   .changed ---> this
      step .changed ---> changed
      this
    }

    protected def disconnect()(implicit tx: S#Tx): Unit = {
      // from .changed -/-> this
      // to   .changed -/-> this
      step .changed -/-> changed
    }
  }

  private final class OpAverageImpl[S <: Sys[S]]
    extends /* OpNativeImpl[S] with */ Op.Average[S] with ConstElemImpl[S] { self =>

    def copy[Out <: stm.Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      this.asInstanceOf[OpAverageImpl[Out]]

    override def toString = "Average"

    def size(in: Int)(implicit tx: S#Tx): Int = math.min(in, 1)

    protected def writeData(out: DataOutput): Unit = {
      out writeByte 1           // cookie XXX TODO --- what was the purpose of this ceremony again?
      out writeInt Op.typeID    //        XXX TODO --- what was the purpose of this ceremony again?
      out writeInt Op.Average.opID
    }

    def changed: EventLike[S, Op.Update[S]] = evt.Dummy.apply
  }

  private def mkReduceReaderFactory[S <: Sys[S]](op0: Reduce.Op[S], in0: Matrix[S], inF: Matrix.ReaderFactory[S],
                                                 inShape: Vec[Int],
                                                 dimIdx: Int, streamDim: Int)
                                                (implicit tx: S#Tx): Matrix.ReaderFactory[S] = {
//    val inF   = in0.prepareReader(streamDim = streamDim)
    val inKey = inF.key

    @tailrec def loop(op: Reduce.Op[S]): Matrix.ReaderFactory[S] = op match {
      case op: OpNativeImpl[S] =>
        inF match {
          case t0: ReaderFactoryImpl.HasSection[S] =>
            val t1 = if (dimIdx < 0) t0 else t0.reduce(dimIdx, op.map(t0.section(dimIdx)))
            t1

          case _ /* t: ReaderFactory.Opaque */ =>
            val section0 = mkAllRange(inShape)
            val section  = if (dimIdx < 0) section0 else section0.updated(dimIdx, op.map(section0(dimIdx)))
            val newKey   = ReaderFactoryImpl.CloudyKey(inKey /*.source */, streamDim, section)
            new ReaderFactoryImpl.Cloudy[S](newKey)
        }

      case _: Op.Average[S] =>
        inF match {
          // group several averaging operations into one offline program
          case t: ReaderFactoryImpl.Average[S] =>
            t.reduceAvg(dimIdx)

          case t: ReaderFactoryImpl.HasSection[S] =>
            val rangeOld = t.section(dimIdx)
            if (rangeOld.size <= 1 || dimIdx < 0) {
              t
            } else {
              val sectionNew  = t.section.updated(dimIdx, 0 to 0)
              val shapeOut    = sectionNew.map(_.size)
              val sectionOut  = mkAllRange(shapeOut)
              val inH         = tx.newHandle(in0)
              val avgDimName  = in0.dimensions.apply(dimIdx).name
              val newKey      = ReaderFactoryImpl.AverageKey(inKey, streamDim = streamDim, section = sectionOut,
                avgDims = Vector(avgDimName))
              new ReaderFactoryImpl.Average[S](inH, in0.name, newKey)
            }

          case _ =>
            ??? // XXX TODO --- we could have a variant of ReaderFactoryImpl.Average that doesn't support HasSection
        }

      case op: Op.Var[S] => loop(op())

      case _ =>
        // val rd = op.map(rInF.reader(), r.in.shape, idx, streamDim)
        // new ReaderFactory.Opaque(rd)
        ??? // later
    }

    loop(op0)
  }

  def mkAllRange(shape: Seq[Int]): Vec[Range] = shape.map(0 until _)(breakOut)

  final class TransparentReader(v: nc2.Variable, protected val streamDim: Int, protected val section: Vec[Range])
    extends ReaderImpl {

    // NetcdfFile is not thread-safe
    private val sync = v.getParentGroup.getNetcdfFile

    // `isNumeric` is guaranteed. The types are: BYTE, FLOAT, DOUBLE, INT, SHORT, LONG

    protected val indexMap: IndexMap = v.getDataType match {
      case ma2.DataType.FLOAT   => IndexMap.Float
      case ma2.DataType.DOUBLE  => IndexMap.Double
      case ma2.DataType.INT     => IndexMap.Int
      case ma2.DataType.LONG    => IndexMap.Long
      case ma2.DataType.BYTE    => IndexMap.Byte
      case ma2.DataType.SHORT   => IndexMap.Short
      case other                => throw new UnsupportedOperationException(s"Unsupported variable data type $other")
    }

    protected def mkArray(sect: ma2.Section): ma2.Array = sync.synchronized(v.read(sect))
  }

  final val rangeVecSer: ImmutableSerializer[Vec[Range]] = ImmutableSerializer.indexedSeq(Serializers.RangeSerializer)

  private[matrix] def readIdentifiedKey(in: DataInput): Matrix.Key = {
    val tpeID     = in.readShort()
    (tpeID: @switch) match {
      case ReaderFactoryImpl.TransparentType =>
        val f         = file(in.readUTF())
        val name      = in.readUTF()
        val streamDim = in.readShort()
        val section   = rangeVecSer.read(in)
        ReaderFactoryImpl.TransparentKey(file = f, name = name, streamDim = streamDim, section = section)

      case ReaderFactoryImpl.CloudyType =>
        val source    = Matrix.Key.read(in)
        val streamDim = in.readShort()
        val section   = rangeVecSer.read(in)
        ReaderFactoryImpl.CloudyKey(source = source, streamDim = streamDim, section = section)

      case ReaderFactoryImpl.AverageType =>
        val source    = Matrix.Key.read(in)
        val streamDim = in.readShort()
        val section   = rangeVecSer.read(in)
        val avgDimsSz = in.readShort()
        val avgDims   = Vector.fill(avgDimsSz)(in.readUTF())
        ReaderFactoryImpl.AverageKey(source = source, streamDim = streamDim, section = section, avgDims = avgDims)

      case _ => sys.error(s"Unexpected reduce key op $tpeID")
    }
  }

  private final class Impl[S <: Sys[S]](protected val targets: Targets[S], val in: Matrix[S],
                                        val dim: Selection[S], val op: Op[S])
    extends Reduce[S]
    with MatrixProxy[S]
    with evt.impl.SingleNode[S, Matrix.Update[S]] { self =>

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      val targetsOut  = Targets[Out]
      val inOut       = context(in)
      val dimOut      = context(dim)
      val opOut       = context(op)
      new Impl(targetsOut, inOut, dimOut, opOut).connect()
    }

    override def toString = s"Reduce$id($in, $dim, $op)"

    protected def matrixPeer(implicit tx: S#Tx): Matrix[S] = in

    def prepareReader(streamDim: Int)(implicit tx: S#Tx): Matrix.ReaderFactory[S] = {
      // mkReduceReaderFactory(this, streamDim)
      val inF = in.prepareReader(streamDim = streamDim)
      mkReduceReaderFactory(op, in, inF, inShape = in.shape, dimIdx = indexOfDim, streamDim = streamDim)
    }

    def prepareDimensionReader(index: Int, useChannels: Boolean)(implicit tx: S#Tx): Matrix.ReaderFactory[S] = {
      val redIdx  = indexOfDim
      val inF     = in.prepareDimensionReader(index = index, useChannels = useChannels)
      if (redIdx != index) {  // if the reference is to a dimension other than the reduced, simply fall back
        inF
      } else {
        // re-use mkReduceReaderFactory with the 1-dimensional matrix
        // ; therefore, dimIdx and streamDim become zero,
        //   and the input shape becomes 1-D
        val inShape   = Vec(in.shape.apply(index))
        val streamDim = if (useChannels) -1 else 0
        val inD       = dimensions.apply(index)
        mkReduceReaderFactory(op, inD, inF, inShape = inShape, dimIdx = 0, streamDim = streamDim)
      }
    }

    def debugFlatten(implicit tx: S#Tx, resolver: DataSource.Resolver[S],
                     exec: ExecutionContext, context: GenContext[S]): Future[Vec[Double]] = {
      val rFut = reader(-1)
      rFut.map { r =>
        val buf = Array.ofDim[Float](r.numChannels, r.numFrames.toInt)
        r.readFloat2D(buf, 0, 1)
        val res = Vec.tabulate(r.numChannels)(ch => buf(ch)(0).toDouble)
        res
      }

//      val data  = in.debugFlatten
//      val idx   = indexOfDim
//      if (idx == -1) return data
//
//      // currently support only `Apply` and `Slice`.
//      // Flat indices work as follows: dimensions are flatten from inside to outside,
//      // so the last dimension uses consecutive samples.
//      // (d0_0, d1_0, d2_0), (d0_0, d1_0, d2_1), ... (d0_0, d1_0, d2_i),
//      // (d0_0, d1_1, d2_0), (d0_0, d1_1, d2_1), ... (d0_0, d1_1, d2_i),
//      // ...
//      // (d0_0, d1_j, d2_0), (d0_0, d1_j, d2_1), ... (d0_0, d1_j, d2_i),
//      // (d0_1, d1_0, d2_0), (d0_0, d1_0, d2_1), ... (d0_0, d1_0, d2_i),
//      // ... ...
//      // ... ... (d0_k, d1_j, d2_i)
//
//      // therefore, if the selected dimension index is 0 <= si < rank,
//      // and the operator's start index is `lo` and the stop index is `hi` (exclusive),
//      // the copy operations is as follows:
//
//      // val num    = shape.take(si    ).product  // d0: 1, d1: k, d2: k * j
//      // val stride = shape.drop(si    ).product  // d0: k * j * i, d1: j * i, d2: i
//      // val block  = shape.drop(si + 1).product  // d0: j * i, d1: i, d2: 1
//      // for (x <- 0 until num) {
//      //   val offset = x * stride
//      //   copy `lo * block + offset` until `hi * block + offset`
//      // }
//
//      val (lo, hi): (Int, Int) = throw new Exception() // rangeOfDim(idx)
//      val sz = hi - lo + 1
//      // if (sz <= 0) return Vec.empty  // or throw exception?
//
//      val sh      = in.shape
//      val num     = sh.take(idx    ).product
//      val block   = sh.drop(idx + 1).product
//      val stride  = block * sh(idx)
//      val szFull  = num * stride        // full size
//      val szRed   = num * block * sz    // reduced size
//
//      val b     = Vec.newBuilder[Double]
//      b.sizeHint(szRed)
//      for (x <- 0 until szFull by stride) {
//        for (y <- lo * block + x until (hi+1) * block + x) {
//          b += data(y)
//        }
//      }
//
//      b.result()
    }

    def shape(implicit tx: S#Tx): Vec[Int] = {
      val sh        = in.shape
      val idx       = indexOfDim
      if (idx == -1) return sh

      val szIn      = sh(idx)
      val sz        = op.size(szIn) // val (idx, sz) = indexAndSize
      if (sz <= 0) Vec.empty  // or throw exception?
      else sh.updated(idx, sz)
    }

    def ranges(implicit tx: S#Tx): Vec[Option[Range]] = {
      val section = in.ranges
      val idx     = indexOfDim
      if (idx < 0) section else {
        val s0Opt = section(idx)
        val s1Opt = s0Opt.flatMap { s0 =>
          @tailrec def loop(op1: Reduce.Op[S]): Option[Range] = op match {
            case op2: OpNativeImpl[S] => Some(op2.map(s0))
            case op2: Op.Var[S]       => loop(op2())
            case _                    => None
          }
          loop(op)
        }
        section.updated(idx, s1Opt)
      }
    }

    private def validateIndex(idx: Int)(implicit tx: S#Tx): Int =
      if (idx >= 0 && idx < in.rank) idx else -1

    def indexOfDim(implicit tx: S#Tx): Int = {
      @tailrec def loop(sel: Selection[S])(implicit tx: S#Tx): Int = sel match {
        case si: Selection.Index[S] => si.expr.value
        case sn: Selection.Name [S] => in.dimensions.indexWhere(_.name == sn.expr.value)
        case sv: Selection.Var  [S] => loop(sv())
      }
      validateIndex(loop(dim))
    }

//    private def rangeOfDim(idx: Int)(implicit tx: S#Tx): (Int, Int) = {
//      @tailrec def loop(_op: Op[S]): (Int, Int) = _op match {
//        case oa: Op.Apply[S] =>
//          val _lo  = oa.index.value
//          val _hi  = _lo // + 1
//          (_lo, _hi)
//
//        case os: Op.Slice[S] =>
//          val _lo = os.from .value
//          val _hi = os.to   .value
//          (_lo, _hi)
//
//        case os: Op.Stride[S] => ...
//
//        case ov: Op.Var  [S] => loop(ov())
//      }
//
//      val (lo, hi) = loop(op)
//      (math.max(0, lo), math.min(in.shape.apply(idx) - 1, hi))
//    }

//    private def indexAndSize(implicit tx: S#Tx): (Int, Int) = {
//      val idx = indexOfDim
//      if (idx == -1) return (-1, -1)   // or throw exception?
//
//      val (lo, hi) = rangeOfDim(idx)
//      val sz = hi - lo + 1
//      (idx, sz)
//    }

    protected def writeData(out: DataOutput): Unit = {
      out writeByte 1   // cookie
      out writeInt Matrix.typeID
      out writeInt Reduce.opID
      in  write out
      dim write out
      op  write out
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = disconnect()

    // ---- event ----

    // def changed: EventLike[S, Matrix.Update[S]] = this

    // protected def reader: evt.Reader[S, Matrix[S]] = Matrix.serializer

    def connect()(implicit tx: S#Tx): this.type = {
      in .changed ---> changed
      dim.changed ---> changed
      op .changed ---> changed
      this
    }

    private def disconnect()(implicit tx: S#Tx): Unit = {
      in .changed -/-> changed
      dim.changed -/-> changed
      op .changed -/-> changed
    }

    object changed extends Changed {
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Matrix.Update[S]] = {
        val e0 =       pull.contains(in .changed) && pull(in .changed).isDefined
        val e1 = e0 || pull.contains(dim.changed) && pull(dim.changed).isDefined
        val e2 = e1 || pull.contains(op .changed) && pull(op .changed).isDefined
        if (e2) Some(Matrix.Update.Generic(self)) else None
      }
    }
  }
}