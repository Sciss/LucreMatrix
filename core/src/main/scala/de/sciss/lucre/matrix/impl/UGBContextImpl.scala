package de.sciss.lucre.matrix.impl

import at.iem.sysson.WorkspaceResolver
import at.iem.sysson.fscape.graph.{Dim, Matrix}
import de.sciss.fscape.lucre.UGenGraphBuilder
import de.sciss.fscape.lucre.UGenGraphBuilder.{IO, Input, MissingIn}
import de.sciss.lucre.matrix.{DataSource, Matrix => LMatrix}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc.{GenContext, WorkspaceHandle}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.{ExecutionContext, Future}

trait UGBContextBase[S <: Sys[S]] extends UGenGraphBuilder.Context[S] {
  def requestInput[Res](req: Input {type Value = Res}, io: IO[S] with UGenGraphBuilder)
                       (implicit tx: S#Tx): Res =
    throw new IllegalStateException(s"Unsupported input request $req")
}

trait UGBContextImpl[S <: Sys[S]] extends UGenGraphBuilder.Context[S] {

  // ---- abstract ----

  implicit protected def context: GenContext[S]

  implicit protected def executionContext: ExecutionContext

  // (protected val fscape: FScape[S])(implicit context: GenContext[S])

  protected def findMatrix(vr: Matrix)(implicit tx: S#Tx): LMatrix[S]

  protected def requestDim(vrName: String, dimNameL: String)(implicit tx: S#Tx): Option[(LMatrix[S], Int)]

  // ---- impl ----

  implicit protected final def cursor    : stm.Cursor[S]       = context.cursor
  implicit protected final def workspace : WorkspaceHandle[S]  = context.workspaceHandle

  abstract override def requestInput[Res](req: Input {type Value = Res}, io: IO[S] with UGenGraphBuilder)
                                         (implicit tx: S#Tx): Res = req match {
    case Matrix.ValueSeq    (vr)       => requestMatrixValueSeq   (vr)
    case Matrix.ValueWindow (vr, dims) => requestMatrixValueWindow(vr, dims)
    case i: Dim.Size                   => requestDimInfo   (i)
    case i: Dim.SuccSize               => requestDimInfo   (i)
    case i: Matrix.Size                => requestMatrixInfo(i)
    case i: Matrix.Rank                => requestMatrixInfo(i)
    case i: Matrix.Spec                => requestVarSpec   (i, io)
    //    case i: UserValue                  => requestUserValue (i)

    case _ => super.requestInput(req, io)
  }

  private def requestDim(dim: Dim)(implicit tx: S#Tx): (LMatrix[S], Int) = {
//    val f       = fscape
    val vrName  = dim.variable.name
    val dimNameL= dim.name
    val mOpt    = requestDim(vrName, dimNameL)
//    val mOpt0   = f.attr.$[LMatrix](vrName)
//    val mOpt    = mOpt0.fold {
//      for {
//        sonif     <- AuralSonification.find[S]()
//        source    <- sonif.sources.get(vrName)
//        dimNameEx <- source.dims.get(dimNameL)
//        dimName    = dimNameEx.value
//        m          = source.matrix
//        dimIdx     = m.dimensions.indexWhere(_.name == dimName)
//        if dimIdx >= 0
//      } yield (m, dimIdx)
//    } { _m =>
//      val dimIdx = _m.dimensions.indexWhere(_.name == dimNameL)
//      if (dimIdx < 0) None else Some((_m, dimIdx))
//    }
    mOpt.getOrElse(throw MissingIn(vrName))
  }

  private def requestDimInfo(i: Dim.InfoGE)(implicit tx: S#Tx): Dim.Info = {
    val (m, dimIdx) = requestDim(i.dim)
    val d = m.dimensions.apply(dimIdx)

    val res: Dim.Info = new Dim.Info {
      val variable: Matrix.Info = mkMatrixInfo(m)
      val index   : Int         = dimIdx
      val matrix  : LMatrix.Key = {
        val f: LMatrix.ReaderFactory[S] = m.prepareDimensionReader(dimIdx, useChannels = false)
        f.key
      }
      val shape   : Vec[Int]    = d.shape
      val name    : String      = d.name
      val units   : String      = d.units
    }
    res
  }

  private def requestMatrixInfo(i: Matrix.InfoGE)(implicit tx: S#Tx): Matrix.Info = {
    val m = findMatrix(i.variable)
    mkMatrixInfo(m)
  }

  private def mkMatrixInfo(m: LMatrix[S])(implicit tx: S#Tx): Matrix.Info =
    new Matrix.Info {
      val matrix  : LMatrix.Key = {
        val f: LMatrix.ReaderFactory[S] = m.prepareReader(-1)
        f.key
      }
      val shape   : Vec[Int]    = m.shape
      val name    : String      = m.name
      val units   : String      = m.units
    }

  private def requestVarSpec(i: Matrix.Spec, io: IO[S] with UGenGraphBuilder)
                            (implicit tx: S#Tx): Matrix.Spec.Value = {
    val mRef = i.variable
    val spec0: Matrix.Spec.Value = if (mRef != null) /* XXX TODO --- dirty hack */  {
      val m = findMatrix(mRef)

      implicit val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]

      val dimsIn  = m.dimensions
      val rank    = dimsIn.size // m.rank
      val dims0: Vec[Matrix.Spec.Dim] = Vector.tabulate(rank) { dimIdx =>
        val dimRF  = m.prepareDimensionReader(dimIdx, useChannels = false)
        //        val reader: LMatrix.Reader = dim.reader(-1)
        val readerF: Future[LMatrix.Reader] = dimRF.reader()
        val valuesF = readerF.map { reader =>
          val lenL = reader.size
          require(lenL <= 0x7FFFFFFF)
          val len = lenL.toInt
          val buf = new Array[Double](len)
          reader.readDouble1D(buf, 0, len)
          buf.toIndexedSeq
        }
        val dim   = dimsIn(dimIdx)
        val szL   = dim.size
        val sz    = if (szL <= 0x7FFFFFFF) szL.toInt else sys.error(s"Integer overflow: $szL")
        Matrix.Spec.DimRead(dim.name, dim.units, sz, dimRF.key, valuesF)
      }
      Matrix.Spec.Value(name = m.name, units = m.units, dimensions = dims0)
    } else {
      Matrix.Spec.Value(name = "test", units = "", dimensions = Vector.empty)
    }

    def resolveDimIdx(specIn: Matrix.Spec.Value, dimRef: Dim): Int = {
      require(dimRef.variable == i.variable)
      // N.B. Because we may drop multiple times,
      // we have to "relocate" the dimensional index,
      // simply by looking up its name
      val (m0, dimIdx0) = requestDim(dimRef)
      val dimName       = m0.dimensions.apply(dimIdx0).name
      val dimIdx        = specIn.dimensions.indexWhere(_.name == dimName)
      require(dimIdx >= 0)
      dimIdx
    }

    val spec = (spec0 /: i.ops) {
      case (specIn, Matrix.Op.Drop(dimRef)) =>
        val dimIdx = resolveDimIdx(specIn, dimRef)
        specIn.copy(dimensions = specIn.dimensions.patch(dimIdx, Nil, 1))

      case (specIn, Matrix.Op.MoveLast(dimRef)) =>
        val dimIdx = resolveDimIdx(specIn, dimRef)
        val dims0 = specIn.dimensions
        val dim   = dims0(dimIdx)
        specIn.copy(dimensions = dims0.patch(dimIdx, Nil, 1) :+ dim)

      case (specIn, Matrix.Op.Append(dimDef)) =>
        val dims0   = specIn.dimensions
        val valuesC = UGenGraphBuilder.resolveSeq(dimDef.values, io) match {
          case Right(xs) => xs
          case Left(msg) => throw new Exception(msg)
        }
        val values  = valuesC.map(_.doubleValue)
        val dim     = Matrix.Spec.DimConst(name = dimDef.name, units = dimDef.units, valuesC = values)
        specIn.copy(dimensions = dims0 :+ dim)

      case (specIn, Matrix.Op.Reduce(dimRef)) =>
        val dimIdx    = resolveDimIdx(specIn, dimRef)
        val dimIn     = specIn.dimensions(dimIdx)
        val dimOut    = Matrix.Spec.DimConst(name = dimIn.name, units = "", valuesC = Vector(0.0))
        specIn.copy(dimensions = specIn.dimensions.updated(dimIdx, dimOut))
    }

    spec
  }

  private def requestMatrixValueSeq(vr: Matrix)(implicit tx: S#Tx): Matrix.ValueSeq.Value = {
    val m     = findMatrix(vr)
    val factory: LMatrix.ReaderFactory[S] = m.prepareReader(-1)
    val res   = new Matrix.ValueSeq.Value {
      val matrix: LMatrix.Key = factory.key

      // XXX TODO: `reader` is called from non-txn
      // and thus we need to create it early here. This is not
      // really cool, because it means the file is opened if
      // if just the cache is validated. We should perhaps change
      // in LucreMatrix the API to use `TxnLike` (what workspace-addDependent uses)
      // instead of `S#Tx`, so we can insert a cheap single txn here
      val /* def */ reader: Future[LMatrix.Reader] = {
        implicit val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]
        factory.reader()
      }
    }
    res
  }

  private def requestMatrixValueWindow(vr: Matrix, dimRefs: Vec[Dim])(implicit tx: S#Tx): Matrix.ValueWindow.Value = {
    val m       = findMatrix(vr)
    val dimsB   = List.newBuilder[Int]
    val shape   = m.shape
    dimsB.sizeHint(dimRefs.size)
    var _winSize = if (dimRefs.isEmpty) 0L else 1L
    dimRefs.foreach { dimRef =>
      val (m1, dimIdx) = requestDim(dimRef)
      require(m1 == m)
      _winSize *= shape(dimIdx)
      dimsB += dimIdx
    }

    val factory: LMatrix.ReaderFactory[S] = m.prepareReader(-1)
    val res = new Matrix.ValueWindow.Value {
      val matrix: LMatrix.Key = factory.key

      val reader: Future[LMatrix.Reader] = {
        implicit val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]
        factory.reader()
      }

      val winSize: Long = _winSize

      val dims: List[Int] = dimsB.result()
    }
    res
  }

//  private def requestUserValue(req: UserValue)(implicit tx: S#Tx): UserValue.Value = {
//    val key    = req.name
//    val valOpt = for {
//      sonif  <- AuralSonification.find[S]()
//      source <- sonif.controls.get(key)
//    } yield source.value
//
//    UserValue.Value(valOpt)
//  }

}