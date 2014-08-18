package de.sciss.lucre.matrix
package impl

import de.sciss.lucre.{event => evt}
import de.sciss.lucre.event.InMemory
import de.sciss.serial.{Serializer, DataInput}

import scala.annotation.switch

object MatrixImpl {
  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Matrix[S]] with evt.Reader[S, Matrix[S]] =
    anySer.asInstanceOf[Ser[S]]

  // ---- impl ----

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

    private def readVar(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Matrix.Var[S] =
      impl.MatrixVarImpl.readIdentified(in, access, targets)

    private def readNode(in: DataInput, access: S#Acc, targets: evt.Targets[S])
                        (implicit tx: S#Tx): Matrix[S] with evt.Node[S] = {
      val tpe   = in.readInt()
      if (tpe != Matrix.typeID) sys.error(s"Unexpected type (found $tpe, expected ${Matrix.typeID})")
      val opID  = in.readInt()
      (opID: @switch) match {
        case Reduce.opID              => Reduce             .readIdentified        (in, access, targets)
        case DataSource.Variable.opID => impl.DataSourceImpl.readIdentifiedVariable(in, access, targets)
        case _                        => sys.error(s"Unknown operator id $opID")
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