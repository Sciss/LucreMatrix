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

    def newConst[S <: Sys[S]](v1: Vec[Double])(implicit tx: S#Tx): Matrix[S] = Impl.newConst(v1)
    def newConst[S <: Sys[S]](v1: Vec[Double], v2: Vec[Double])(implicit tx: S#Tx): Matrix[S] = Impl.newConst(v1, v2)

    def newConst[S <: Sys[S]](v1: Vec[Double], v2: Vec[Double], v3: Vec[Double])(implicit tx: S#Tx): Matrix[S] =
      Impl.newConst(v1, v2, v3)

    // def newVar[S <: Sys[S]](init: Matrix[S])(implicit tx: S#Tx): Matrix[S] = Impl.newVar(init)
  }

  implicit class MatrixOps[S <: Sys[S]](val `this`: Matrix[S]) extends AnyVal {
    def slice(dim: Int, index: Int)(implicit tx: S#Tx): Matrix[S] =
      Reduce(`this`, Dimension.Selection.Index(Ints.newConst(dim)), Reduce.Op.Apply(Ints.newConst(index)))
  }
}
