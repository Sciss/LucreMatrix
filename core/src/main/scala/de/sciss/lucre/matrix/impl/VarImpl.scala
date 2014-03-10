package de.sciss.lucre
package matrix
package impl

import de.sciss.lucre.{event => evt}
import de.sciss.serial.DataOutput

// XXX TODO: this should go back into LucreEvent
trait VarImpl[S <: Sys[S], EU, Elem <: evt.Publisher[S, EU], U]
  extends Var[S, Elem]
  with evt.impl.StandaloneLike[S, U, Elem]
  with evt.impl.Generator     [S, U, Elem] {

  _: Elem =>

  protected def ref: S#Var[Elem]

  protected def mkUpdate(before: Elem, now: Elem): U
  protected def mapUpdate(in: EU): U

  def apply()(implicit tx: S#Tx): Elem = ref()

  def update(v: Elem)(implicit tx: S#Tx): Unit = {
    val before = ref()
    if (before != v) {
      val con = targets.nonEmpty
      if (con) before.changed -/-> this
      ref() = v
      if (con) {
        v.changed ---> this
        fire(mkUpdate(before, v))
      }
    }
  }

  protected def disposeData()(implicit tx: S#Tx): Unit = ref.dispose()

  protected def writeData(out: DataOutput): Unit = {
    out.writeByte(0)    // cookie
    ref.write(out)
  }

  // ---- event ----

  def changed: evt.EventLike[S, U] = this

  def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[U] = {
    if (pull.parents(this /* select() */).isEmpty) {
      Some(pull.resolve[U])
    } else {
      pull(this().changed).map(mapUpdate)
    }
  }

  def connect   ()(implicit tx: S#Tx): Unit = ref().changed ---> this
  def disconnect()(implicit tx: S#Tx): Unit = ref().changed -/-> this
}
