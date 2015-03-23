/*
 *  DataSourceImpl.scala
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

package de.sciss.lucre.matrix
package impl

import de.sciss.lucre.artifact.Artifact
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

  def apply[S <: Sys[S]](artifact: Artifact[S])(implicit tx: S#Tx, resolver: Resolver[S]): DataSource[S] = {
    val file    = artifact.value
    val netFile = resolver.resolve(file)
    val f0      = artifact

    new Impl[S] {
      ds =>

      val id                = tx.newID()
      val artifact          = f0
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
  // stores the result in `matMap` and returns it.
  private def mkVariable[S <: Sys[S]](source: DataSource[S], net: nc2.Variable,
                                      netMap: mutable.Map[String, nc2.Variable],
                                      matMap: mutable.Map[String, Variable[S]])(implicit tx: S#Tx): Variable[S] = {
    val targets   = evt.Targets[S]
    // val sourceRef = tx.newVar(id, source)
    val parents   = parentsLoop(net.getParentGroup, Nil)
    val name      = net.getShortName
    val units0    = net.getUnitsString
    val units     = if (units0 == null) "" else units0

    val dimArr    = net.getDimensions
    val rangeArr  = net.getRanges
    val rank      = dimArr.size()
    if (rank != rangeArr.size()) throw new IllegalStateException(
      s"For variable $name, nr of dimensions ($rank) is different from nr of ranges (${rangeArr.size()})")

    // a 1D recursive variable indicates a pure dimension
    val res: Variable[S] = if (rank == 1 && dimArr.get(0).getShortName == name) {
      val size = dimArr.get(0).getLength
      new DimensionImpl(targets, source, parents, name, units, size)

    } else {
      // we assume that there are no recursive variable references... (we don't check for that condition)
      // -- if not, we could also use an S#Var in this case
      val dimensions: Vec[Matrix[S]] = Vec.tabulate(rank) { i =>
        val dim     = dimArr  .get(i)
        val n0      = dim.getShortName
        val dimName = if (n0 == null) "?" else n0
        // val dimName = i.dim.name
        val rangeJ  = rangeArr.get(i)
        // val r       = i.range // guaranteed to be inclusive, therefore we can directly test for `end == size - 1`
        val rStart  = rangeJ.first()
        val rEnd    = rangeJ.last()
        val rStep   = rangeJ.stride()
        val full    = matMap.getOrElse(dimName, netMap.get(dimName).fold[Matrix[S]] {
          // dimension not found (perhaps not numeric)
          // we're creating a dummy matrix then.
          MatrixFactoryImpl.newConst1D[S](dimName, Range.Double.inclusive(rStart, rEnd, rStep))
        } { net1 =>
          mkVariable(source, net1, netMap, matMap)
        })
        // if the variable's dimensional range covers the whole range, then just
        // use the matrix wrapper for the dimensional variable. otherwise, we must
        // reduce it.
        if (rStart == 0 && rStep == 1 && rEnd == full.size - 1) full else {
          val dimSel = Dimension.Selection.Index[S](IntEx.newConst(0))
          val op = if (rStart == rEnd)
            Reduce.Op.Apply [S](IntEx.newConst(rStart))
          else if (rStep == 1)
            Reduce.Op.Slice [S](IntEx.newConst(rStart), IntEx.newConst(rEnd))
          else {
            require(rStart == 0 && rEnd == full.size - 1)
            Reduce.Op.Stride[S](/* IntEx.newConst(rStart), IntEx.newConst(rEnd), */ IntEx.newConst(rStep))
          }

          Reduce(full, dimSel, op)
        }
      }

      new VariableImpl(targets, source, parents, name, units, dimensions)
    }

    matMap.put(name, res)
    res
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
    val source      = DataSource.read(in, access)
    val parents     = parentsSer.read(in)
    val name        = in.readUTF()
    val units       = in.readUTF()
    val isLeaf      = in.readBoolean()
    if (isLeaf) {
      val size      = in.readInt()
      new DimensionImpl(targets, source, parents, name, units, size)
    } else {
      val dimensions  = dimsSer[S].read(in, access)
      new VariableImpl(targets, source, parents, name, units, dimensions)
    }
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
    // val file    = new File(in.readUTF())
    val artifact  = Artifact.read(in, access)
    val varRef    = tx.readVar[List[Variable[S]]](id, in)
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

  // private final case class ShapeInfo(dim: Dimension.Value, range: Range)

  private def dimsSer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Vec[Matrix[S]]] =
    anyDimsSer.asInstanceOf[Serializer[S#Tx, S#Acc, Vec[Matrix[S]]]]

  private val anyDimsSer = mkDimsSer[InMemory]

  private def mkDimsSer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Vec[Matrix[S]]] =
    Serializer.indexedSeq[S#Tx, S#Acc, Matrix[S]]

  private final class VariableImpl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                                val source: DataSource[S],
                                                val parents: List[String],
                                                protected val nameConst: String,
                                                protected val unitsConst: String,
                                                dimConst: Vec[Matrix[S]])
    extends VariableImplLike[S] {


    def mkCopy()(implicit tx: S#Tx): Matrix[S] = {
      val tgt         = evt.Targets[S]
      val sourceCpy   = source  // XXX TODO - ok?
      val dimCpy      = dimConst.map(_.mkCopy())
      new VariableImpl(tgt, sourceCpy, parents, nameConst, unitsConst, dimCpy)
    }

    protected def writeDimensions(out: DataOutput): Unit = dimsSer[S].write(dimConst, out)

    def shape (implicit tx: S#Tx): Vec[Int  ] = dimConst.map(        _.size.toInt)  // XXX TODO - check Int overflow
    def ranges(implicit tx: S#Tx): Vec[Range] = dimConst.map(0 until _.size.toInt)

    def dimensions(implicit tx: S#Tx): Vec[Matrix[S]] = dimConst

    def isLeaf = false

    protected def disposeData()(implicit tx: S#Tx): Unit = dimConst.foreach(_.dispose())
  }

  private final class DimensionImpl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                                 val source: DataSource[S],
                                                 val parents: List[String],
                                                 protected val nameConst: String,
                                                 protected val unitsConst: String,
                                                 sizeConst: Int)
    extends VariableImplLike[S] {

    def mkCopy()(implicit tx: S#Tx): Matrix[S] = this

    protected def writeDimensions(out: DataOutput): Unit = out.writeInt(sizeConst)

    def shape (implicit tx: S#Tx): Vec[Int  ] = Vec(        sizeConst)
    def ranges(implicit tx: S#Tx): Vec[Range] = Vec(0 until sizeConst)

    override def rank(implicit tx: S#Tx) = 1
    override def size(implicit tx: S#Tx) = sizeConst.toLong

    def dimensions(implicit tx: S#Tx): Vec[Matrix[S]] = Vec(this)

    def isLeaf = true

    protected def disposeData()(implicit tx: S#Tx) = ()
  }

  private abstract class VariableImplLike[S <: Sys[S]]
    extends MatrixRoot[S] with Variable[S] with evt.Node[S] {

    // ---- abstract ----

    protected def nameConst : String
    protected def unitsConst: String

    protected def isLeaf: Boolean

    protected def writeDimensions(out: DataOutput): Unit

    // ---- event dummy ----

    final def changed: EventLike[S, Matrix.Update[S]] = evt.Dummy.apply

    final def select(slot: Int): Event[S, Any, Any] = throw new UnsupportedOperationException

    // ----

    final def name (implicit tx: S#Tx): String = nameConst
    final def units(implicit tx: S#Tx): String = unitsConst

    final def debugFlatten(implicit tx: S#Tx): Vec[Double] = {
      // if (size > 256) throw new UnsupportedOperationException(s"debugFlatten is restricted to matrices with size <= 256")
      throw new UnsupportedOperationException("debugFlatten on a NetCDF backed matrix")
    }

    final def getKey(streamDim: Int)(implicit tx: S#Tx): Matrix.Key =
      new ReduceImpl.ReaderFactory.Transparent(file = source.artifact.value, name = name, streamDim = streamDim,
        section = ReduceImpl.mkAllRange(shape))

    final protected def writeData(out: DataOutput): Unit = {
      out writeByte 1   // cookie
      out writeInt Matrix.typeID
      out writeInt Variable.opID
      source    .write(out)
      parentsSer.write(parents, out)
      out       .writeUTF(nameConst)
      out       .writeUTF(unitsConst)
      out       .writeBoolean(isLeaf)
      writeDimensions(out)
    }

    final def data()(implicit tx: S#Tx, resolver: Resolver[S]): nc2.Variable = {
      val net = source.data()
      import JavaConversions._
      net.getVariables.find(_.getShortName == nameConst).getOrElse(
        sys.error(s"Variable '$nameConst' does not exist in data source ${source.artifact.value.base}")
      )
    }
  }

  private abstract class Impl[S <: Sys[S]]
    extends DataSource[S] with Mutable.Impl[S] {

    protected def varRef: S#Var[List[Variable[S]]]

    override def toString() = s"DataSource$id"

    // def file = new File(path)

    // def path: String = file.path

    protected def writeData(out: DataOutput): Unit = {
      out.writeLong(SOURCE_COOKIE)
      artifact.write(out)
      varRef.write(out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = varRef.dispose()

    def variables(implicit tx: S#Tx): List[Variable[S]] = varRef()

    def data()(implicit tx: S#Tx, resolver: Resolver[S]): nc2.NetcdfFile = resolver.resolve(artifact.value)
  }
}