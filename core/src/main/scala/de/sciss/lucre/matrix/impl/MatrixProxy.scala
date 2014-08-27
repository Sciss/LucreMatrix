/*
 *  MatrixProxy.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre.matrix
package impl

trait MatrixProxy[S <: Sys[S]] extends Matrix[S] {
  m =>

  protected def matrixPeer(implicit tx: S#Tx): Matrix[S]

  import m.{matrixPeer => peer}

  def debugFlatten(implicit tx: S#Tx): Vec[Double] = peer.debugFlatten

  def shape     (implicit tx: S#Tx): Vec[Int]             = peer.shape
  // def dimensions(implicit tx: S#Tx): Vec[Dimension.Value] = peer.dimensions
  def ranges    (implicit tx: S#Tx): Vec[Range]           = peer.ranges

  def dimensions(implicit tx: S#Tx): Vec[Matrix[S]] = peer.dimensions

  def name(implicit tx: S#Tx): String = peer.name
}
