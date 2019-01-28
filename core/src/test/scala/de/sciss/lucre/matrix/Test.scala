package de.sciss.lucre.matrix

import de.sciss.lucre.matrix.Implicits._
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{InMemory, Workspace}
import de.sciss.synth.proc.GenContext

object Test {
  def main(args: Array[String]): Unit = {
    initTypes()
    type S = InMemory
    implicit val sys: S = InMemory()
    sys.step { implicit tx => run[S]() }
  }

  def run[S <: Sys[S]]()(implicit tx: S#Tx, cursor: S with stm.Cursor[S]): Unit = {
    val m = Matrix.zeros(4, 5)
    import scala.concurrent.ExecutionContext.Implicits.global
    implicit val resolver : DataSource.Resolver [S] = DataSource.Resolver.empty
    implicit val ws       : Workspace           [S] = Workspace.Implicits.dummy[S]
    implicit val context  : GenContext          [S] = GenContext[S]
    println(m.debugFlatten.value.get.get)
  }
}