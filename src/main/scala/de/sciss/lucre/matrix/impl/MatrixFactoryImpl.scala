/*
 *  MatrixFactoryImpl.scala
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

object MatrixFactoryImpl extends MatrixFactory {
  def zeros   [S <: Sys[S]](ns: Int*)(implicit tx: S#Tx): Matrix[S] = ZeroMatrixImpl(ns.toIndexedSeq)

  def newConst[S <: Sys[S]](v1: Vec[Double])(implicit tx: S#Tx): Matrix[S] =  ???
  def newConst[S <: Sys[S]](v1: Vec[Double], v2: Vec[Double])(implicit tx: S#Tx): Matrix[S] = ???
  def newConst[S <: Sys[S]](v1: Vec[Double], v2: Vec[Double], v3: Vec[Double])(implicit tx: S#Tx): Matrix[S] = ???
}
