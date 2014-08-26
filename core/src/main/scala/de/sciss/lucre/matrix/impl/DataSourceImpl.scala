/*
 *  DataSourceImpl.scala
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

package de.sciss.lucre.matrix
package impl

import ucar.nc2
import de.sciss.serial.{ImmutableSerializer, Serializer, DataInput, DataOutput}
import de.sciss.lucre.{event => evt}
import de.sciss.file._
import de.sciss.lucre.matrix.DataSource.{Variable, Resolver}
import de.sciss.lucre.stm.Mutable
import scala.collection.mutable
import scala.collection.{JavaConversions, breakOut}
import scala.annotation.tailrec
import de.sciss.lucre.event.{InMemory, Event, EventLike}
import de.sciss.lucre.expr.{Int => IntEx}

object DataSourceImpl {
  private final val SOURCE_COOKIE = 0x737973736F6E6400L   // "syssond\0"

  // private final val VAR_COOKIE    = 0x76617200            // "var\0"

  @tailrec private[this] def parentsLoop(g: nc2.Group, res: List[String]): List[String] = {
    val p = g.getParentGroup
    if (p == null) res else parentsLoop(p, g.getFullName :: res)
  }

  // ---- create a new data source (list of variables) ----

  def apply[S <: Sys[S]](file: File)(implicit tx: S#Tx, resolver: Resolver[S]): DataSource[S] = {
    val netFile = resolver.resolve(file)
    val f0      = file

    new Impl[S] {
      ds =>

      val id                = tx.newID()
      val file              = f0
      protected val varRef  = tx.newVar[List[Variable[S]]](id, Nil)
      import JavaConversions._
      val numericVars = netFile.getVariables.filter(_.getDataType.isNumeric)
      val netMap: mutable.Map[String, nc2.Variable] = numericVars.map { net =>
        val name = net.getShortName
        (name, net)
      } (breakOut)
      val matMap  = mutable.Map.empty[String, Variable[S]]
      val list: List[Variable[S]] = numericVars.map { net =>
        val name = net.getShortName
        matMap.getOrElse(name, mkVariable(ds, net, netMap, matMap))
      } (breakOut)
      varRef() = list   // tricky decoupling of recursive serialization
    }
  }

  // ---- create a new variable ----

  private def mkVariable[S <: Sys[S]](source: DataSource[S], net: nc2.Variable,
                                      netMap: mutable.Map[String, nc2.Variable],
                                      matMap: mutable.Map[String, Variable[S]])(implicit tx: S#Tx): Variable[S] = {
    val targets   = evt.Targets[S]
    // val sourceRef = tx.newVar(id, source)
    val parents   = parentsLoop(net.getParentGroup, Nil)
    val name      = net.getShortName

    val dimArr    = net.getDimensions
    val rangeArr  = net.getRanges
    val rank      = dimArr.size()
    if (rank != rangeArr.size()) throw new IllegalStateException(
      s"For variable $name, nr of dimensions ($rank) is different from nr of ranges (${rangeArr.size()})")

    val shapeInfo = Vec.tabulate(rank) { i =>
      val dim     = dimArr  .get(i)
      val rangeJ  = rangeArr.get(i)
      val range   = Range.inclusive(rangeJ.first(), rangeJ.last(), rangeJ.stride())
      val n0      = dim.getShortName
      val dimName = if (n0 == null) "?" else n0
      ShapeInfo(Dimension.Value(dimName, dim.getLength), range)
    }

    ??? // for 1D matricies, the only dimension is most likely referring to itself

    // we assume that there are no recursive variable references... (we don't check for that condition)
    // -- if not, we could also use an S#Var in this case
    val dimensions: Vec[Matrix[S]] = shapeInfo.map { i =>
      val dimName = i.dim.name
      val r       = i.range // guaranteed to be inclusive, therefore we can directly test for `end == size - 1`
      val full    = matMap.getOrElse(dimName, netMap.get(dimName).fold[Matrix[S]] {
        // dimension not found (perhaps not numeric)
        // we're creating a dummy matrix then.
        MatrixFactoryImpl.newConst1D[S](dimName, r.map(_.toDouble))
      } { net1 =>
        mkVariable(source, net1, netMap, matMap)
      })
      // if the variable's dimensional range covers the whole range, then just
      // use the matrix wrapper for the dimensional variable. otherwise, we must
      // reduce it.
      if (r.start == 0 && r.step == 1 && r.end == full.size - 1) full else {
        val dimSel = Dimension.Selection.Index[S](IntEx.newConst(0))
        val op = if (r.size == 1)
          Reduce.Op.Apply [S](IntEx.newConst(r.start))
        else if (r.step == 1)
          Reduce.Op.Slice [S](IntEx.newConst(r.start), IntEx.newConst(r.end))
        else
          Reduce.Op.Stride[S](IntEx.newConst(r.start), IntEx.newConst(r.end), IntEx.newConst(r.step))

        Reduce(full, dimSel, op)
      }
    }

    new VariableImpl(targets, source /* sourceRef */, parents, name, /* shapeInfo, */ dimensions)
  }

  def readVariable[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Variable[S] = {
    val targets = evt.Targets.read(in, access)
    val cookie = in.readByte() // 'node'
    require (cookie == 1, s"Unexpected cookie (found $cookie, expected 1")
    val tpe     = in.readInt()  // 'type'
    require (tpe == Matrix.typeID, s"Unexpected type id (found $tpe, expected ${Matrix.typeID}")
    val opID  = in.readInt()    // 'op'
    require (opID == Variable.opID, s"Unexpected operator id (found $opID, expected ${Variable.opID}")
    readIdentifiedVariable(in, access, targets)
  }

  def readIdentifiedVariable[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                         (implicit tx: S#Tx): Variable[S] with evt.Node[S] = {
    val source    = DataSource.read(in, access)
    // val sourceRef = tx.readVar[DataSource[S]](id, in)
    val parents   = parentsSer.read(in)
    val name      = in.readUTF()
    // val shape     = shapeSer.read(in)
    ??? // new VariableImpl(targets, source /* sourceRef */, parents, name, shape)
  }
  
  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, DataSource[S]] =
    anySer.asInstanceOf[Ser[S]]

  implicit def varSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Variable[S]] =
    anyVarSer.asInstanceOf[VarSer[S]]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): DataSource[S] = new Impl[S] {
    val id      = tx.readID(in, access)
    val cookie  = in.readLong()
    require(cookie == SOURCE_COOKIE,
      s"Unexpected cookie (found ${cookie.toHexString}, expected ${SOURCE_COOKIE.toHexString})")
    val file    = new File(in.readUTF())
    val varRef  = tx.readVar[List[Variable[S]]](id, in)
  }

  private val anySer    = new Ser   [evt.InMemory]

  private val anyVarSer = new VarSer[evt.InMemory]

  private class Ser[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, DataSource[S]] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): DataSource[S] = DataSourceImpl.read(in, access)

    def write(source: DataSource[S], out: DataOutput): Unit = source.write(out)
  }

  private class VarSer[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Variable[S]] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Variable[S] = DataSourceImpl.readVariable(in, access)

    def write(v: Variable[S], out: DataOutput): Unit = v.write(out)
  }

  private val parentsSer  = ImmutableSerializer.list[String]
  import Serializers.RangeSerializer

  private object ShapeInfo {
    private val SHAPE_COOKIE = 0x73686170 // "shap"

    implicit object Ser extends ImmutableSerializer[ShapeInfo] {
      def read(in: DataInput): ShapeInfo = {
        val cookie = in.readInt()
        require (cookie == SHAPE_COOKIE, s"Unexpected cookie (found $cookie, expected $SHAPE_COOKIE)")
        val dName = in.readUTF()
        val dSize = in.readInt()
        val range = RangeSerializer.read(in)
        ShapeInfo(Dimension.Value(dName, dSize), range)
      }

      def write(info: ShapeInfo, out: DataOutput): Unit = {
        out.writeInt(SHAPE_COOKIE)
        val dim = info.dim
        out.writeUTF(dim.name)
        out.writeInt(dim.size)
        RangeSerializer.write(info.range, out)
      }
    }
  }
  private final case class ShapeInfo(dim: Dimension.Value, range: Range)

  private def dimsSer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Vec[Matrix[S]]] =
    anyDimsSer.asInstanceOf[Serializer[S#Tx, S#Acc, Vec[Matrix[S]]]]

  private val anyDimsSer = mkDimsSer[InMemory]

  private def mkDimsSer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Vec[Matrix[S]]] =
    Serializer.indexedSeq[S#Tx, S#Acc, Matrix[S]]

  //  private final class ReaderImpl[S <: Sys[S]](shapeInfo: Vec[ShapeInfo], streamDim: Int, v: nc2.Variable)
  //    extends Matrix.Reader {
  //
  //    private val numFramesI = if (streamDim < 0) 1 else shapeInfo(streamDim).dim.size
  //
  //    def numFrames: Long = numFramesI
  //
  //    val numChannels: Int = {
  //      val sz = (1L /: shapeInfo)(_ * _.dim.size)
  //      val n  = sz / numFramesI
  //      if (n > 0x7FFFFFFF) throw new IndexOutOfBoundsException(s"numChannels $n exceeds 32-bit range")
  //      n.toInt
  //    }
  //
  //    def read(buf: Array[Array[Float]], off: Int, len: Int): Unit = {
  //      ...
  //    }
  //  }

  private final class VariableImpl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                                val source: DataSource[S],
                                                val parents: List[String],
                                                _name: String, dimConst: Vec[Matrix[S]])
    extends Variable[S] with evt.Node[S] {

    // ---- event dummy ----

    def changed: EventLike[S, Matrix.Update[S]] = evt.Dummy.apply

    def select(slot: Int): Event[S, Any, Any] = throw new UnsupportedOperationException

    // ----

    def name(implicit tx: S#Tx): String = _name

    def debugFlatten(implicit tx: S#Tx): Vec[Double] = {
      // if (size > 256) throw new UnsupportedOperationException(s"debugFlatten is restricted to matrices with size <= 256")
      throw new UnsupportedOperationException("debugFlatten on a NetCDF backed matrix")
    }

    def shape     (implicit tx: S#Tx): Vec[Int            ] = dimConst.map(_.size.toInt)  // XXX TODO - check Int overflow

    def dimensions(implicit tx: S#Tx): Vec[Matrix[S]] = dimConst

    def getKey(streamDim: Int)(implicit tx: S#Tx): Matrix.Key =
      new ReduceImpl.ReaderFactory.Transparent(file = source.file, name = name, streamDim = streamDim,
        section = ReduceImpl.mkAllRange(shape))

    protected def writeData(out: DataOutput): Unit = {
      out writeByte 1   // cookie
      out writeInt Matrix.typeID
      out writeInt Variable.opID
      source    .write(out)
      parentsSer.write(parents, out)
      out       .writeUTF(_name)
      dimsSer   .write(dimConst, out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = dimConst.foreach(_.dispose())

    def data()(implicit tx: S#Tx, resolver: Resolver[S]): nc2.Variable = {
      val net = source.data()
      import JavaConversions._
      net.getVariables.find(_.getShortName == _name).getOrElse(
        sys.error(s"Variable '$name' does not exist in data source ${source.file.base}")
      )
    }
  }

  private abstract class Impl[S <: Sys[S]]
    extends DataSource[S] with Mutable.Impl[S] {

    protected def varRef: S#Var[List[Variable[S]]]

    override def toString() = s"DataSource($path)"

    // def file = new File(path)

    def path: String = file.path

    protected def writeData(out: DataOutput): Unit = {
      out.writeLong(SOURCE_COOKIE)
      out.writeUTF(path)
      varRef.write(out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      varRef.dispose()
    }

    def variables(implicit tx: S#Tx): List[Variable[S]] = varRef()

    def data()(implicit tx: S#Tx, resolver: Resolver[S]): nc2.NetcdfFile = resolver.resolve(file)
  }
}