/*
 *  MatrixFactoryImpl.scala
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

object MatrixFactoryImpl extends MatrixFactory {
  def zeros   [S <: Sys[S]](ns: Int*)(implicit tx: S#Tx): Matrix[S] = ZeroMatrixImpl(ns.toIndexedSeq)

  def newConst1D[S <: Sys[S]](v: Vec[        Double  ])(implicit tx: S#Tx): Matrix[S] = ConstMatrixImpl.apply1D(v)
  def newConst2D[S <: Sys[S]](v: Vec[Vec[    Double ]])(implicit tx: S#Tx): Matrix[S] = ConstMatrixImpl.apply2D(v)
  def newConst3D[S <: Sys[S]](v: Vec[Vec[Vec[Double]]])(implicit tx: S#Tx): Matrix[S] = ConstMatrixImpl.apply3D(v)
}
