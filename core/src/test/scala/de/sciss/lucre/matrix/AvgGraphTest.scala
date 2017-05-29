package de.sciss.lucre.matrix

trait AvgGraphTest {
  import de.sciss.fscape._
  import graph._
  import at.iem.sysson.fscape.graph._

  Graph {
    val mIn     = Matrix("anom")
    val dim1    = Dim(mIn, "longitude")
    val dim2    = Dim(mIn, "latitude" )
    val dSz1    = dim1.size
    val dSz2    = dim2.size

    val win     = mIn.valueWindow(dim1, dim2)
    val winSz   = dSz1 * dSz2
    val isOk    = !win.isNaN
    val v       = Gate(win, isOk) * isOk // XXX TODO --- NaN * 0 is not zero

    val tr      = Metro(winSz)
    val sum     = RunningSum(v   , tr)
    val count   = RunningSum(isOk, tr)
    val sumTrunc= ResizeWindow(sum  , size = winSz, start = winSz - 1)
    val cntTrunc= ResizeWindow(count, size = winSz, start = winSz - 1)
    val mOut    = sumTrunc / cntTrunc
    val specIn  = mIn.spec
    val specOut = specIn  .drop(dim1).drop(dim2)
    // MkMatrix("out", specOut, DC(1.23).take(Length(mOut)))
    MkMatrix("out", specOut, mOut)
  }
}
