package de.sciss.lucre.matrix

import de.sciss.file.File
import de.sciss.serial.ImmutableSerializer
import de.sciss.synth.io.AudioFileSpec

object AudioFileCache {
  object Value {
    implicit def serializer: ImmutableSerializer[Value] = ???
  }
  final case class Value(file: File, spec: AudioFileSpec)
}
trait AudioFileCache {
}
