/*
 *  MatrixFactoryImpl.scala
 *  (LucreMatrix)
 *
 *  Copyright (c) 2014-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2019 by Hanns Holger Rutz.
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

  def newConst1D[S <: Sys[S]](name: String, v: Vec[        Double  ], units: String)(implicit tx: S#Tx): Matrix[S] =
    ConstMatrixImpl.apply1D(name, units, v)

  def newConst2D[S <: Sys[S]](name: String, v: Vec[Vec[    Double ]], units: String)(implicit tx: S#Tx): Matrix[S] =
    ConstMatrixImpl.apply2D(name, units, v)

  def newConst3D[S <: Sys[S]](name: String, v: Vec[Vec[Vec[Double]]], units: String)(implicit tx: S#Tx): Matrix[S] =
    ConstMatrixImpl.apply3D(name, units, v)
}
