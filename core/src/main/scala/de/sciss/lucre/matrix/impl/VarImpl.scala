/*
 *  VarImpl.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 by Hanns Holger Rutz.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre
package matrix
package impl

import de.sciss.lucre.{event => evt}
import de.sciss.serial.DataOutput

// XXX TODO: this should go back into LucreEvent
trait VarImpl[S <: Sys[S], EU, Elem <: evt.Publisher[S, EU], U <: EU]
  extends Var[S, Elem]
  with evt.impl.SingleNode[S, U]
  // with evt.impl.Generator     [S, U, Elem]
  {

  // _: Elem =>

  protected def ref: S#Var[Elem]

  protected def mkUpdate(before: Elem, now: Elem): U
  protected def mapUpdate(in: EU): U

  def apply()(implicit tx: S#Tx): Elem = ref()

  def update(v: Elem)(implicit tx: S#Tx): Unit = {
    val before = ref()
    if (before != v) {
      val con = targets.nonEmpty
      if (con) before.changed -/-> changed
      ref() = v
      if (con) {
        v.changed ---> changed
        changed.fire(mkUpdate(before, v))
      }
    }
  }

  protected def disposeData()(implicit tx: S#Tx): Unit = {
    disconnect()
    ref.dispose()
  }

  protected def writeData(out: DataOutput): Unit = {
    out.writeByte(0)    // cookie
    ref.write(out)
  }

  // ---- event ----

  // def changed: evt.EventLike[S, U] = this
  object changed extends Changed with evt.impl.Generator[S, U] {
    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[U] = {
      if (pull.parents(this /* select() */).isEmpty) {
        Some(pull.resolve[U])
      } else {
        pull(this).map(mapUpdate)
      }
    }
  }

  final def connect()(implicit tx: S#Tx): this.type = {
    ref().changed ---> changed
    this
  }

  private def disconnect()(implicit tx: S#Tx): Unit = ref().changed -/-> changed
}
