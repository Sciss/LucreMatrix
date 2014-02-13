/*
 *  MatrixProxy.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre.matrix
package impl

import de.sciss.lucre.expr.Expr

trait MatrixProxy[S <: Sys[S]] extends Matrix[S] {
  m =>

  protected def matrixPeer: Matrix[S]

  import m.{matrixPeer => peer}

  def flatten(implicit tx: S#Tx): Vec[Double] = peer.flatten

  def shape: Expr[S, Vec[Dimension[S]]] = peer.shape

  def size : Expr[S, Long  ] = peer.size
  def rank : Expr[S, Int   ] = peer.rank
  def name : Expr[S, String] = peer.name
}
