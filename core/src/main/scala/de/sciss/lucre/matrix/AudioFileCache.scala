package de.sciss.lucre.matrix

import de.sciss.file.File
import de.sciss.filecache.Limit
import de.sciss.lucre.matrix.DataSource.Resolver
import de.sciss.lucre.stm
import de.sciss.serial.ImmutableSerializer
import de.sciss.synth.io.AudioFileSpec
import impl.{AudioFileCacheImpl => Impl}

import scala.concurrent.{Future, ExecutionContext}
import scala.language.implicitConversions

object AudioFileCache {
  def apply(config: Config = Config().build): AudioFileCache = Impl(config)

  object Value {
    implicit def serializer: ImmutableSerializer[Value] = ???
  }
  final case class Value(file: File, spec: AudioFileSpec)

  sealed trait ConfigLike {
    /** The directory where the cached values are stored. If this directory does not exist
      * upon cache creation, it will be created on the fly.
      */
    def folder: File

    /** The maximum capacity of the cache. */
    def capacity: Limit

    /** The context used by the cache to spawn future computations. */
    def executionContext: ExecutionContext
  }

  object Config {
    /** Creates a new configuration builder with default values. */
    def apply() = new ConfigBuilder
    /** Implicitly converts a builder to an immutable configuration value. */
    implicit def build(b: ConfigBuilder): Config = b.build
  }

  /** The configuration for the producer, containing information about the cache folder, cache capacity, etc. */
  final case class Config private[AudioFileCache](folder: File, capacity: Limit,
                                                  executionContext: ExecutionContext)
    extends ConfigLike

  /** A configuration builder is a mutable version of `Config` and will be implicitly converted to the latter
    * when passed into `Producer.apply`.
    */
  final class ConfigBuilder private[AudioFileCache]() extends ConfigLike {
    private var _folder     = Option.empty[File]

    /** @inheritdoc
      *
      * By default this will lazily create a temporary directory deleted on application exit.
      * If this value is set via `folder_=`, that setting replaces the default behavior.
      */
    def folder: File = _folder.getOrElse {
      val f = File.createTemp(".cache", "")
      f.delete()
      f.mkdir()
      f.deleteOnExit()
      _folder = Some(f)
      f
    }
    def folder_=(value: File): Unit = _folder = Some(value)

    /** @inheritdoc
      *
      * The default value is 500 files or 10 GB space.
      */
    var capacity  = Limit(count = 500, space = 10L * 1024 * 1024 * 1024)

    /** @inheritdoc
      *
      * The default value is `ExecutionContext.global`.
      */
    var executionContext: ExecutionContext = ExecutionContext.global

    override def toString = s"Cache.ConfigBuilder@${hashCode().toHexString}"

    def build: Config = Config(folder = folder, capacity = capacity, executionContext = executionContext)
  }
}
trait AudioFileCache {
  import AudioFileCache.Value
  def acquire[S <: Sys[S]](key: Matrix.Key)
                          (implicit tx: S#Tx, resolver: Resolver[S], cursor: stm.Cursor[S]): Future[Value]
}
