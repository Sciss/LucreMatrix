/*
 *  MatrixFactory.scala
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

package de.sciss.lucre.matrix

trait MatrixFactory extends Any {
  def zeros[S <: Sys[S]](ns: Int*)(implicit tx: S#Tx): Matrix[S]

  def newConst1D[S <: Sys[S]](name: String, v: Vec[        Double  ], units: String = "")(implicit tx: S#Tx): Matrix[S]
  def newConst2D[S <: Sys[S]](name: String, v: Vec[Vec[    Double ]], units: String = "")(implicit tx: S#Tx): Matrix[S]
  def newConst3D[S <: Sys[S]](name: String, v: Vec[Vec[Vec[Double]]], units: String = "")(implicit tx: S#Tx): Matrix[S]

  // def newVar[S <: Sys[S]](init: Matrix[S])(implicit tx: S#Tx): Matrix[S]
}