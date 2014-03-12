package de.sciss.lucre.matrix

import Implicits._
import de.sciss.lucre.event.InMemory

object Test extends App {
  val sys = InMemory()
  sys.step { implicit tx => run[InMemory]() }

  def run[S <: Sys[S]]()(implicit tx: S#Tx): Unit = {
    val m = Matrix.zeros(4, 5)
    println(m.debugFlatten)
  }
}
