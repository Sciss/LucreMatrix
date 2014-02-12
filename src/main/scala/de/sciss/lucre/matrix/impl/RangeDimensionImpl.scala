package de.sciss.lucre.matrix
package impl

import de.sciss.lucre.expr.Expr
import de.sciss.lucre.{event => evt}
import evt.EventLike

class RangeDimensionImpl[S <: Sys[S]](_name: String, range: Range) extends Dimension[S] {
  def changed: EventLike[S, Dimension.Update[S]] = evt.Dummy.apply

  def flatten(implicit tx: S#Tx): Vec[Double] = range.map(_.toDouble)

  def size: Expr[S, Int   ] = Ints   .newConst(range.size)

  def name: Expr[S, String] = Strings.newConst(_name)
}
