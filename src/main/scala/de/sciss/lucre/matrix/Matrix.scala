/*
 *  Matrix.scala
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

package de.sciss.lucre
package matrix

import de.sciss.lucre.stm
import de.sciss.lucre.{event => evt}
import evt.{InMemory, Publisher}
import de.sciss.serial.{DataInput, Writable}
import stm.Disposable
import scala.annotation.switch
import de.sciss.serial.Serializer

object Matrix {
  final val typeID = 0x30001

  object Var {
    def apply[S <: Sys[S]](init: Matrix[S])(implicit tx: S#Tx): Var[S] = impl.MatrixVarImpl(init)

    implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Var[S]] = impl.MatrixVarImpl.serializer
  }
  trait Var[S <: Sys[S]] extends Matrix[S] with matrix.Var[S, Matrix[S]]

  case class Update[S <: Sys[S]](matrix: Matrix[S])

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Matrix[S]] with evt.Reader[S, Matrix[S]] =
    anySer.asInstanceOf[Ser[S]]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Matrix[S] = serializer[S].read(in, access)

  private val anySer = new Ser[InMemory]

  private final class Ser[S <: Sys[S]] extends evt.EventLikeSerializer[S, Matrix[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Matrix[S] with evt.Node[S] = {
      // 0 = var, 1 = op
      (in.readByte(): @switch) match {
        case 0      => readVar (in, access, targets)
        case 1      => readNode(in, access, targets)
        case other  => sys.error(s"Unexpected cookie $other")
      }
    }

    private def readVar(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Var[S] =
      impl.MatrixVarImpl.readIdentified(in, access, targets)

    private def readNode(in: DataInput, access: S#Acc, targets: evt.Targets[S])
                        (implicit tx: S#Tx): Matrix[S] with evt.Node[S] = {
      val tpe   = in.readInt()
      require(tpe == typeID, s"Unexpected type (found $tpe, expected $typeID)")
      val opID  = in.readInt()
      (opID: @switch) match {
        case Reduce.opID  => Reduce.readIdentified(in, access, targets)
        case _            => sys.error("Unknown operator id $opID")
      }
    }

    def readConstant(in: DataInput)(implicit tx: S#Tx): Matrix[S] = {
      val opID = in.readInt()
      (opID: @switch) match {
        case impl.ZeroMatrixImpl .opID => impl.ZeroMatrixImpl .readIdentified(in)
        case impl.ConstMatrixImpl.opID => impl.ConstMatrixImpl.readIdentified(in)
        case _                         => sys.error(s"Unexpected operator $opID")
      }
    }
  }
}
trait Matrix[S <: Sys[S]] extends Writable with Disposable[S#Tx] with Publisher[S, Matrix.Update[S]] {
  def name(implicit tx: S#Tx): String

  // def rank: Expr[S, Int   ]
  // def size: Expr[S, Long  ]

  def rank(implicit tx: S#Tx): Int    = shape.size
  def size(implicit tx: S#Tx): Long   = (1L /: shape)(_ * _)

  def shape(implicit tx: S#Tx): Vec[Int]

  def dimensions(implicit tx: S#Tx): Vec[Dimension.Value]

  def ranges(implicit tx: S#Tx): Vec[Range] // XXX TODO: this might get problematic with averaging reductions

  def reducedRank      (implicit tx: S#Tx): Int                   = shape.count (_ > 1)
  def reducedShape     (implicit tx: S#Tx): Vec[Int]              = shape.filter(_ > 1)
  def reducedDimensions(implicit tx: S#Tx): Vec[Dimension.Value]  = reduce(dimensions)
  def reducedRanges    (implicit tx: S#Tx): Vec[Range]            = reduce(ranges)

  private def reduce[A](coll: Vec[A])(implicit tx: S#Tx): Vec[A] =
    (coll zip shape).collect { case (x, sz) if sz > 1 => x }

  // def shape: Expr[S, Vec[Dimension[S]]]   // ...or use a mutable collection?
  // def shape(implicit tx: S#Tx): Vec[Dimension.Value]

  def flatten(implicit tx: S#Tx): Vec[Double]

  // def read(...) = ...

  // def view[I <: Sys[S]](implicit bridge: S#Tx => I#Tx): Matrix[I]
}
