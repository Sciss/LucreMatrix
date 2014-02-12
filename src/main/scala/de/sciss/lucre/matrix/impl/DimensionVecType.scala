package de.sciss.lucre.matrix
package impl

import de.sciss.lucre.expr.Expr

object DimensionVecType {
  type V [S <: Sys[S]] = Vec[Dimension[S]]
  type Ex[S <: Sys[S]] = Expr[S, V[S]]

  def newConst[S <: Sys[S]](vec: Vec[Dimension[S]])(implicit tx: S#Tx): Ex[S] = ???
}
