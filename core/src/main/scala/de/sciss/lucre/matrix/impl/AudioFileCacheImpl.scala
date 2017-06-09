/*
 *  AudioFileCacheImpl.scala
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

package de.sciss.lucre.matrix
package impl

import de.sciss.file._
import de.sciss.filecache
import de.sciss.lucre.matrix.DataSource.Resolver
import de.sciss.lucre.stm.TxnLike
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.synth.io.{AudioFile, AudioFileSpec}
import de.sciss.synth.proc.GenContext

import scala.concurrent.stm.{TMap, atomic}
import scala.concurrent.{Future, blocking}
import scala.util.control.NonFatal

object AudioFileCacheImpl {
  //  private val KEY_COOKIE  = 0x6166636B  // "afck"
  private val VAL_COOKIE  = 0x61666376  // "afcv"

  def apply(config: AudioFileCache.Config): AudioFileCache = new Impl(config)

  private val DEBUG   = false

  private def debug(what: => String): Unit = if (DEBUG) println(s"<cache> $what")

  type Result = AudioFileCache.Value // (File, AudioFileSpec) // Grapheme.Value.Audio

  //  private object CacheKey {
  //    implicit object Serializer extends ImmutableSerializer[CacheKey] {
  //      def read(in: DataInput): CacheKey = {
  //        val cookie = in readInt()
  //        require(cookie == KEY_COOKIE,
  //          s"Unexpected cookie (expected ${KEY_COOKIE.toHexString}, found ${cookie.toHexString})")
  //        val path      = in readUTF()
  //        val parents   = parentsSer.read(in)
  //        val variable  = in readUTF()
  //        val section   = sectionSer.read(in)
  //        val streamDim = in readInt()
  //        CacheKey(file = file(path), parents = parents, variable = variable, section = section, streamDim = streamDim)
  //
  //      }
  //
  //      def write(v: CacheKey, out: DataOutput): Unit = {
  //        import v._
  //        out writeInt  KEY_COOKIE
  //        v.write(out)
  //        //        out writeUTF  file.path
  //        //        parentsSer.write(parents, out)
  //        //        out writeUTF  variable
  //        //        sectionSer.write(section, out)
  //        //        out writeInt  streamDim
  //      }
  //    }
  //  }

  private type CacheKey = Matrix.Key
  // private val  CacheKey = Matrix.Key

  //  /* Key part of the cache
  //   *
  //   * @param file      the NetCDF file
  //   * @param parents   the parent groups of the variable, excluding the root group
  //   * @param variable  the variable name
  //   * @param section   the sectioning of the variable
  //   * @param streamDim  the dimension index to stream or `-1`
  //   */
  //  private case class CacheKey(file: File, parents: List[String], variable: String, section: Vec[Range],
  //                              streamDim: Int) {
  //    // lazy val spec: AudioFileSpec = keyToSpec(this)
  //  }

  //  private def sectionToSpec(vs: VariableSection, streamDim: Int /* , key: CacheKey */): AudioFileSpec = {
  //    // import key._
  //    val isStreaming   = streamDim >= 0
  //    val shape         = vs.shape
  //    val numFrames     = if (!isStreaming) 1 else shape(streamDim)
  //    val size          = (1L /: shape)((sum, sz) => sum * sz)
  //    val numChannelsL  = size / numFrames
  //    require(numChannelsL <= 0xFFFF, s"The number of channels ($numChannelsL) is larger than supported")
  //    val numChannels   = numChannelsL.toInt
  //    log(s"sectionToSpec($vs): shape = $shape, numFrames = $numFrames, size = $size, numChannels = $numChannels")
  //    AudioFileSpec(numFrames = numFrames, numChannels = numChannels, sampleRate = 44100 /* rate */)
  //  }

  object valueSerializer extends ImmutableSerializer[CacheValue] {
    def write(v: CacheValue, out: DataOutput): Unit = {
      import v._
      out writeInt  VAL_COOKIE
      out writeUTF  file.getPath
      AudioFileSpec.Serializer.write(spec, out)
    }

    def read(in: DataInput): CacheValue = {
      val cookie = in.readInt()
      require(cookie == VAL_COOKIE, s"Serialized version $cookie does not match $VAL_COOKIE")
      val f     = new File(in.readUTF())
      val spec  = AudioFileSpec.Serializer.read(in)
      CacheValue(file = f, spec = spec)
    }
  }

  //  private case class CacheValue(netSize: Long, netModified: Long, data: File, spec: AudioFileSpec) {
  //    override def toString =
  //      s"$productPrefix(size = $netSize, lastModified = ${new java.util.Date(netModified)}, data = ${data.getName})"
  //  }
  private type CacheValue = AudioFileCache.Value
  private val  CacheValue = AudioFileCache.Value

  private final class Impl(config: AudioFileCache.Config) extends AudioFileCache {
    /*
      The cache is organised as follows:
      - the lookup part of the key is the NetCDF file (`get` takes a `NetcdfFile`, we actually just use its path).
      - the value completion of the key is an instance of `CacheValue`, which maintains verifiable information about
       the NetCDF file's identity (size and modification date), along with a pointer `data` to the actually
       generated stats file which is associated with the cache

      This stats file is a straight forward serialization of the `Stats` trait.

     */
    private[this] val cache = {
      val cfg2              = filecache.Config[CacheKey, CacheValue]()
      cfg2.capacity         = config.capacity
      cfg2.accept           = (_ /* key */, _ /* value */) => {
        val res = true // XXX TODO - check if netcdf file changed: key.file.lastModified() == value.netModified && key.file.length() == value.netSize
        // debug(s"accept key = ${key.file.name} (lastModified = ${new java.util.Date(key.file.lastModified())}}), value = $value? $res")
        res
      }
      cfg2.space            = (_  , value) => value.file /* data */.length()
      cfg2.evict            = (_  , value) => {
        debug(s"evict $value")
        value.file /* data */.delete()
      }
      cfg2.folder           = config.folder
      cfg2.executionContext = config.executionContext
      atomic { implicit tx =>
        filecache.TxnProducer(cfg2)
      }
    }

    private[this] val map = TMap.empty[Matrix.Key, Entry]

    private def produceValue[S <: Sys[S]](reader: Matrix.Reader): CacheValue = {
      //      val v             = /* workspace. */ cursor.step { implicit tx => source.data() }
      //      val vs            = VariableSection(v, section.map(OpenRange.closed))
      //      val arr           = vs.readSafe()

      // cf. Arrays.txt for (de-)interleaving scheme
      // val spec          = sectionToSpec(vs, streamDim)
      val spec          = AudioFileSpec(numFrames = reader.numFrames, numChannels = reader.numChannels,
        sampleRate = 44100 /* rate */)
      import spec.{numChannels, numFrames}
      // val afF           = File.createTemp("sysson", ".aif")
      val afF           = java.io.File.createTempFile("sysson", ".aif", config.folder)
      val af            = AudioFile.openWrite(afF, spec)
      val fBufSize      = math.max(1, math.min(8192 / numChannels, numFrames)).toInt // file buffer
      assert(fBufSize > 0)

      log(s"Audio file cache: '${afF.name}', numChannels = $numChannels, numFrames = $numFrames")

      val fBuf          = af.buffer(fBufSize)
      var framesWritten = 0L
      //      val t             = if (streamDim <= 0) arr else arr.transpose(0, streamDim)
      //      import at.iem.sysson.Implicits._
      //      val it            = t.float1Diterator
      while (framesWritten < numFrames) {
        val chunk = math.min(fBufSize, numFrames - framesWritten).toInt
        reader.readFloat2D(fBuf, 0, chunk)
        //        var i = 0
        //        while (i < chunk) {
        //          var ch = 0
        //          while (ch < numChannels) {
        //            fBuf(ch)(i) = it.next() // XXX TODO: would be better to have inner loop iterate for frames
        //            ch += 1
        //          }
        //          i += 1
        //        }
        af.write(fBuf, 0, chunk)
        framesWritten += chunk
      }
      af.close()
      //
      //      val file = vs.file.file
      CacheValue(/* netSize = file.length(), netModified = file.lastModified(), */ file = afF, spec = spec)
    }

    def acquire[S <: Sys[S]](key: Matrix.Key)
                            (implicit tx: S#Tx, resolver: Resolver[S], context: GenContext[S]): Future[Result] = {
      implicit val itx = tx.peer
      map.get(key).fold {
        val fut0 = cache.acquireWith(key) {
          import cache.executionContext
          val readerFut = context.cursor.step { implicit tx => key.reader() }
          readerFut.map { reader =>
            blocking {
              produceValue(reader)
            }
          }
        }
//        // import cache.executionContext
//        val fut = fut0.map { value =>
//          // Grapheme.Value.Audio(artifact = value.data, spec = value.spec, offset = 0L, gain = 1.0)
//          AudioFileCache.Value(value.data, value.spec)
//        } (executionContext)
        val fut = fut0
        val e = new Entry(future = fut)
        map.put(key, e)
        fut.recover {
          case NonFatal(t) =>
            map.single.remove(key)
            throw t
        } (config.executionContext)
        fut

      } { e0 =>
        val e1 = e0.inc
        map.put(key, e1)
        e1.future
      }
    }

    def release(key: Matrix.Key)(implicit tx: TxnLike): Unit = {
      implicit val itx = tx.peer
      map.get(key).foreach { e0 =>
        val e1 = e0.dec
        if (e1.isEmpty) {
          map.remove(key)
          cache.release(key)
        } else {
          map.put(key, e1)
        }
      }
    }
  }

  final private class Entry(val useCount: Int = 1, val future: Future[Result]) {
    def inc               = new Entry(useCount + 1, future)
    def dec               = new Entry(useCount - 1, future)
    def isEmpty: Boolean  = useCount == 0
  }
}
