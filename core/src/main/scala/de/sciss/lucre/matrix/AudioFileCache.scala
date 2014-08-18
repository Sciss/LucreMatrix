package de.sciss.lucre.matrix

import de.sciss.file.File
import de.sciss.synth.io.AudioFileSpec

object AudioFileCache {
  final case class Value(file: File, spec: AudioFileSpec)
}
trait AudioFileCache {
}
