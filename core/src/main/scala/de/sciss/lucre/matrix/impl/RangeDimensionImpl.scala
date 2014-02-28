///*
// *  RangeDimensionImpl.scala
// *  (LucreMatrix)
// *
// *  Copyright (c) 2014 Institute of Electronic Music and Acoustics, Graz.
// *  Written by Hanns Holger Rutz.
// *
// *	This software is published under the GNU General Public License v2+
// *
// *
// *	For further information, please contact Hanns Holger Rutz at
// *	contact@sciss.de
// */
//
//package de.sciss.lucre.matrix
//package impl
//
//import de.sciss.lucre.expr
//import expr.Expr
//import de.sciss.serial.DataOutput
//import Serializers.RangeSerializer
//
//class RangeDimensionImpl[S <: Sys[S]](_name: String, range: Range)
//  extends Dimension[S] with expr.impl.ConstImpl[S, Dimension.Value] {
//
//  def flatten(implicit tx: S#Tx): Vec[Double] = range.map(_.toDouble)
//
//  def size: Expr[S, Int   ] = Ints   .newConst(range.size)
//  def name: Expr[S, String] = Strings.newConst(_name)
//
//  protected def constValue = Dimension.Value(name = _name, size = range.size)
//
//  protected def writeData(out: DataOutput): Unit = {
//    out.writeUTF(_name)
//    RangeSerializer.write(range, out)
//  }
//}
