/*
 *  Implicits.scala
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

import de.sciss.lucre.expr.IntObj

object Implicits {
  implicit class MatrixFactoryOps(val `this`: Matrix.type) extends AnyVal with MatrixFactory {
    import impl.{MatrixFactoryImpl => Impl}

    def zeros[S <: Sys[S]](ns: Int*)(implicit tx: S#Tx): Matrix[S] = Impl.zeros(ns: _*)

    def newConst1D[S <: Sys[S]](name: String, v: Vec[        Double  ], units: String)(implicit tx: S#Tx): Matrix[S] =
      Impl.newConst1D(name = name, v = v, units = units)

    def newConst2D[S <: Sys[S]](name: String, v: Vec[Vec[    Double ]], units: String)(implicit tx: S#Tx): Matrix[S] =
      Impl.newConst2D(name = name, v = v, units = units)

    def newConst3D[S <: Sys[S]](name: String, v: Vec[Vec[Vec[Double]]], units: String)(implicit tx: S#Tx): Matrix[S] =
      Impl.newConst3D(name = name, v = v, units = units)
  }

  implicit class MatrixOps[S <: Sys[S]](val `this`: Matrix[S]) extends AnyVal {
    def slice(dim: Int, index: Int)(implicit tx: S#Tx): Matrix[S] =
      Reduce(`this`, Dimension.Selection.Index(IntObj.newConst(dim)), Reduce.Op.Apply(IntObj.newConst(index)))
  }
}
