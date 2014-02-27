/*
 *  Implicits.scala
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

object Implicits {
  implicit class MatrixFactoryOps(val `this`: Matrix.type) extends AnyVal with MatrixFactory {
    import impl.{MatrixFactoryImpl => Impl}

    def zeros[S <: Sys[S]](ns: Int*)(implicit tx: S#Tx): Matrix[S] = Impl.zeros(ns: _*)

    def newConst1D[S <: Sys[S]](v: Vec[        Double  ])(implicit tx: S#Tx): Matrix[S] = Impl.newConst1D(v)
    def newConst2D[S <: Sys[S]](v: Vec[Vec[    Double ]])(implicit tx: S#Tx): Matrix[S] = Impl.newConst2D(v)
    def newConst3D[S <: Sys[S]](v: Vec[Vec[Vec[Double]]])(implicit tx: S#Tx): Matrix[S] = Impl.newConst3D(v)
  }

  implicit class MatrixOps[S <: Sys[S]](val `this`: Matrix[S]) extends AnyVal {
    def slice(dim: Int, index: Int)(implicit tx: S#Tx): Matrix[S] =
      Reduce(`this`, Dimension.Selection.Index(Ints.newConst(dim)), Reduce.Op.Apply(Ints.newConst(index)))
  }
}
