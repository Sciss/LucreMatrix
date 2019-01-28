/*
 *  DataSource.scala
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

package de.sciss.lucre.matrix

import java.io.File

import de.sciss.lucre.artifact.Artifact
import de.sciss.lucre.matrix.impl.{DataSourceImpl => Impl}
import de.sciss.lucre.stm.Obj
import de.sciss.serial.{DataInput, Serializer}
import ucar.nc2

object DataSource extends Obj.Type {
  // ---- Obj.Type ----

  final val typeId = 0x30005

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

  // ----

  /** Creates a new data source from a given path that points to a NetCDF file. This
    * will open the file and build a model of its variables.
    *
    * @param artifact   file reference pointing to a NetCDF file
    * @param resolver   the resolver is used to actually open (a possibly cached) NetCDF file
    */
  def apply[S <: Sys[S]](artifact: Artifact[S])(implicit tx: S#Tx, resolver: Resolver[S]): DataSource[S] =
    Impl(artifact)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, DataSource[S]] =
    Impl.serializer[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): DataSource[S] =
    serializer[S].read(in, access)

  object Variable {
    final val opId = 3

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Variable[S]] = Impl.varSerializer

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Variable[S] =
      serializer[S].read(in, access)
  }
  trait Variable[S <: Sys[S]] extends Matrix[S] {
    def source /* (implicit tx: S#Tx) */: DataSource[S]
    def parents: List[String]

    def data()(implicit tx: S#Tx, resolver: Resolver[S]): nc2.Variable
  }

  object Resolver {
    def seq[S <: Sys[S]](files: nc2.NetcdfFile*): Seq[S] = {
      val res = empty[S]
      files.foreach(res += _)
      res
    }

    def empty[S <: Sys[S]]: Seq[S] = new Seq[S]()

    final class Seq[S <: Sys[S]] private[Resolver]() extends Resolver[S] {
      private[this] val sync = new AnyRef

      @volatile
      private[this] var map = Map.empty[String, nc2.NetcdfFile]

      def += (file: nc2.NetcdfFile): Unit = sync.synchronized(map += file.getLocation -> file)

      def resolve(file: File)(implicit tx: S#Tx): nc2.NetcdfFile = {
        val p = file.getPath
        map.getOrElse(file.getPath, throw new NoSuchElementException(p))
      }
    }
  }
  trait Resolver[S <: Sys[S]] {
    def resolve(file: File)(implicit tx: S#Tx): nc2.NetcdfFile
  }
}
/** A document represents one open data file. */
trait DataSource[S <: Sys[S]] extends Obj[S] {
  def artifact: Artifact[S]

  def data()(implicit tx: S#Tx, resolver: DataSource.Resolver[S]): nc2.NetcdfFile

  def variables(implicit tx: S#Tx): List[DataSource.Variable[S]]

  final def tpe: Obj.Type = DataSource
}