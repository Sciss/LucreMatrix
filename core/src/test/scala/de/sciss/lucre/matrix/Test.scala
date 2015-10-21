package de.sciss.lucre.matrix

import de.sciss.lucre.matrix.Implicits._
import de.sciss.lucre.stm.InMemory

object Test extends App {
  val sys = InMemory()
  sys.step { implicit tx => run[InMemory]() }

  def run[S <: Sys[S]]()(implicit tx: S#Tx): Unit = {
    val m = Matrix.zeros(4, 5)
    println(m.debugFlatten)
  }
}
