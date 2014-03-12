package de.sciss.lucre.matrix
package impl

import ucar.nc2
import de.sciss.serial.{ImmutableSerializer, Serializer, DataInput, DataOutput}
import de.sciss.lucre.{event => evt}
import de.sciss.file._
import DataSource.Variable
import de.sciss.lucre.stm.Mutable
import scala.collection.{JavaConversions, breakOut}
import DataSource.Resolver
import scala.annotation.tailrec

object DataSourceImpl {
  private final val SOURCE_COOKIE = 0x737973736F6E6400L   // "syssond\0"

  private final val VAR_COOKIE    = 0x76617200            // "var\0"

  @tailrec private[this] def parentsLoop(g: nc2.Group, res: List[String]): List[String] = {
    val p = g.getParentGroup
    if (p == null) res else parentsLoop(p, g.getFullName :: res)
  }

  // XXX TODO: eliminate here
  implicit private[this] class RichNetcdfFile(val peer: nc2.NetcdfFile)
    extends AnyVal {

    import JavaConversions._
    def variableMap: Map[String, nc2.Variable]  = peer.getVariables.map(a => a.getShortName -> a)(breakOut)
  }

  // XXX TODO: eliminate here
  implicit private[this] class RichVariable(val peer: nc2.Variable)
    extends AnyVal {

    import JavaConversions._
    def dimensions : Vec[nc2.Dimension] = peer.getDimensions.toIndexedSeq
    def ranges     : Vec[Range]         = peer.getRanges.map {
      ma => Range.inclusive(ma.first(), ma.last(), ma.stride())
    } (breakOut)
  }

  def apply[S <: Sys[S]](file: File)(implicit tx: S#Tx, resolver: Resolver[S]): DataSource[S] = {
    val netFile = resolver.resolve(file)
    val f0      = file

    new Impl[S] {
      ds =>

      val id                = tx.newID()
      val file              = f0
      protected val varRef  = tx.newVar[List[Variable[S]]](id, Nil)
      import JavaConversions._
      varRef() = netFile.getVariables.map(variable(ds, _))(breakOut)   // tricky decoupling of recursive serialization
    }
  }

  private def variable[S <: Sys[S]](source: DataSource[S], net: nc2.Variable)(implicit tx: S#Tx): Variable[S] = {
    val id        = tx.newID()
    // val sourceRef = tx.newVar(id, source)
    val parents   = parentsLoop(net.getParentGroup, Nil)
    val name      = net.getShortName
    val shape     = net.dimensions.map { dim =>
      val n0 = dim.getShortName
      if (n0 == null) "?" else n0
    } zip net.ranges
    new VariableImpl(id, source /* sourceRef */, parents, name, shape)
  }

  def readVariable[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Variable[S] = {
    val id        = tx.readID(in, access)
    val cookie    = in.readInt()
    require(cookie == VAR_COOKIE,
      s"Unexpected cookie (found ${cookie.toHexString}, expected ${VAR_COOKIE.toHexString})")
    val source    = DataSource.read(in, access)
    // val sourceRef = tx.readVar[DataSource[S]](id, in)
    val parents   = parentsSer.read(in)
    val name      = in.readUTF()
    val shape     = shapeSer.read(in)
    new VariableImpl(id, source /* sourceRef */, parents, name, shape)
  }

  //  private def resolveFile[S <: Sys[S]](workspace: Workspace[S], file: File)(implicit tx: S#Tx): nc2.NetcdfFile =
  //    workspace.fileCache.get(file)(tx.peer).getOrElse {
  //      val net = nc2.NetcdfFile.open(file.path).setImmutable()
  //      workspace.fileCache.put(file, net)(tx.peer)
  //      Txn.afterRollback { _ =>
  //        net.close() // a bit tricky doing I/O inside a transaction...
  //      } (tx.peer)
  //      net
  //    }

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
  private val shapeSer    = ImmutableSerializer.indexedSeq[(String, Range)]

  private final class VariableImpl[S <: Sys[S]](val id: S#ID, val source: DataSource[S] /* sourceRef: S#Var[DataSource[S]] */,
                                                val parents: List[String],
                                                val name: String, val shape: Vec[(String, Range)])
    extends Variable[S] with Mutable.Impl[S] {

    // def source(implicit tx: S#Tx): DataSource[S] = _source // sourceRef()

    protected def writeData(out: DataOutput): Unit = {
      out       .writeInt(VAR_COOKIE)
      // sourceRef .write(out)
      source    .write(out)
      parentsSer.write(parents, out)
      out       .writeUTF(name)
      shapeSer  .write(shape  , out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      // sourceRef.dispose()
    }

    def rank: Int = shape.size

    lazy val size: Long = {
      var res = 0L
      shape.foreach { tup => res += tup._2.size }
      res
    }

    def data()(implicit tx: S#Tx, resolver: Resolver[S]): nc2.Variable = {
      val net = source.data()
      net.variableMap.getOrElse(name, sys.error(s"Variable '$name' does not exist in data source ${source.file.base}"))
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