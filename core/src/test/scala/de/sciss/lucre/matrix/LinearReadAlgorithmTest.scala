package de.sciss.lucre.matrix

import de.sciss.lucre.matrix.impl.ReaderImpl

object LinearReadAlgorithmTest {
  def main(args: Array[String]): Unit = run()

//  def calcIndices(off: Int, shape: Vector[Int]): Vector[Int] = {
//    val modsDivs = shape zip shape.scanRight(1)(_ * _).tail
//    modsDivs.map { case (mod, div) =>
//      (off / div) % mod
//    }
//  }

  def calcIndices(off: Int, shape: Vector[Int]): Vector[Int] = {
    val modsDivs = (shape, shape.scanRight(1)(_ * _).tail, shape.indices).zipped
    modsDivs.map { case (mod, div, idx) =>
      val x = off / div
      if (idx == 0) x else x % mod
    }
  }

  def calcPOI(a: Vector[Int], b: Vector[Int], min: Int): Int = {
    val res = (a.drop(min) zip b.drop(min)).indexWhere { case (ai, bi) => ai != bi }
    if (res < 0) a.size else res + min
  }

//  def calcPOI(a: Vector[Int], b: Vector[Int], shape: Vector[Int], min: Int): Int = {
//    val modsDivs = shape zip shape.scanRight(1)(_ * _).tail
//    val trip = (a, b, shape).zipped.drop(min).toVector
//    val res = trip.indexWhere { case (ai, bi, mod) => (ai % mod) != (bi % mod) }
//    if (res < 0) a.size else res + min
//  }

  def zipToRange(a: Vector[Int], b: Vector[Int]): Vector[Range] =
    (a, b).zipped.map { (ai, bi) =>
      require (ai <= bi, s"zipToRange($a, $b)")
      ai to bi
    }

  def calcOff(a: Vector[Int], shape: Vector[Int]): Int = {
    val divs = shape.scanRight(1)(_ * _).tail
    (a, divs).zipped.map(_ * _).sum
  }

  def indexTrunc(a: Vector[Int], poi: Int, inc: Boolean): Vector[Int] =
    a.zipWithIndex.map { case (ai, i) =>
      if      (i < poi) ai
      else if (i > poi) 0
      else if (inc)     ai + 1
      else              ai
    }

  private[this] val DEBUG = false

  def indexStr(a: Vector[Int]): String = a.mkString("[", ", ", "]")

  def indicesStr(off: Int, len: Int, shape: Vector[Int]): String =
    (off until (off + len)).map(calcIndices(_, shape))
      .map(_.mkString("[", ", ", "]")).mkString("\n")


  def partition(shape: Vector[Int], off: Int, len: Int): List[Vector[Range]] = {
    val rankM = shape.size - 1

    def loop(start: Int, stop: Int, poiMin: Int, dir: Boolean,
             res0: List[Vector[Range]]): List[Vector[Range]] =
      if (start == stop) res0 else {
        assert(start < stop)
        val last = stop - 1

        val s0  = calcIndices(start, shape)
        val s1  = calcIndices(stop , shape)
        val s1m = calcIndices(last , shape)
        val poi = calcPOI(s0, s1m, poiMin)
        val ti  = if (dir) s0 else s1 // m
        val to  = if (dir) s1 else s0
        val st  = if (poi >= rankM) to else indexTrunc(ti, poi, inc = dir)

        val trunc = calcOff(st, shape)
        val split = trunc != (if (dir) stop /* last */ /* stop */ else start)

        if (DEBUG)
          println(f"[${if (dir) "lo" else "hi"}] start = $start%3d, stop = $stop%3d, s0 = ${indexStr(s0)}; s1 = ${indexStr(s1)}; s1m = ${indexStr(s1m)} --> poi = $poi (min $poiMin), trunc = ${indexStr(st)} / $trunc; split $split")

        if (split) {
          if (dir) {
            val res1 = loop(start, trunc, poiMin = poi + 1, dir = true , res0 = res0)
            loop           (trunc, stop , poiMin = 0      , dir = false, res0 = res1)
          } else {
            val s1tm = calcIndices(trunc - 1, shape)
            val res1 = zipToRange(s0, s1tm) :: res0
            if (DEBUG) println(s"read from ${indexStr(s0)} to ${indexStr(s1tm)}")
            loop           (trunc, stop , poiMin = poi + 1, dir = false, res0 = res1)
          }
        } else {
          if (DEBUG) println(s"read from ${indexStr(s0)} to ${indexStr(s1m)}")
          zipToRange(s0, s1m) :: res0
        }
      }

    loop(off, off + len, poiMin = 0, dir = true, res0 = Nil).reverse
  }

  class Variable[A](val data: Vector[A], val shape: Vector[Int]) {
    require(data.size == shape.product)

    def read(sections: Vector[Range]): Vector[A] = {
      require(sections.size == shape.size)
      require(sections.zipWithIndex.forall { case (r, ri) => r.forall(i => i >= 0 && i < shape(ri)) })

      val sz  = if (sections.isEmpty) 0 else (1 /: sections)(_ * _.size)
      val zip = (shape zip sections).reverse
      Vector.tabulate(sz) { i =>
        val (j, _, _) = ((0, 1, 1) /: zip) { case ((res, m, n), (dim, r)) =>
          val add = r((i / n) % r.size) * m
          (res + add, m * dim, n * r.size)
        }
        data(j)
      }
    }
  }

  def sampleRange(in: Range, by: Range): Range = {
    val drop  = by.start
    val stepM = by.step
    require(drop >= 0 && stepM > 0)
    val in1 = in.drop(drop)
    val in2 = if (stepM == 1)
      in1
    else if (in1.isInclusive)  // copy-method is protected
      new Range.Inclusive(start = in1.start, end = in1.end, step = in1.step * stepM)
    else
      new Range(start = in1.start, end = in1.end, step = in1.step * stepM)
    in2.take(by.size)
  }

  class ChunkReader[A](val v: Variable[A], val in: Vector[Range]) {
    require(in.size == v.shape.size)

    private[this] var _pos    = 0
    private[this] val inShape = in.map(_.size)

    def pos : Int = _pos
    val size: Int = inShape.product

    private[this] var _statMaxReads = 0

    def statMaxReads: Int = _statMaxReads

    private[this] val inShapeArr = inShape.toArray

    def read(len: Int): Vector[A] = {
      require (len >= 0 && _pos + len <= size, s"pos = ${_pos}, len = $len; size = $size")

//      val sub = partition(shape = inShape, off = _pos, len = len)
      val sub = {
        var b = List.empty[Vec[Range]]
        ReaderImpl.partition(shape = inShapeArr, start0 = _pos, stop0 = _pos + len) { range =>
          b ::= range
        }
        b.reverse
      }
      if (sub.size > _statMaxReads) _statMaxReads = sub.size
      val res: Vector[A] = sub.flatMap { ranges =>
        val sec = (in, ranges).zipped.map(sampleRange)
        v.read(sec)
      } (collection.breakOut)
      _pos += len
      res
    }
  }

  def run_(): Unit = {
//    val sh = Vector(1, 1, 1, 2)
//    val off = 1
//    val len = 1

//    val sh = Vector(1, 1, 3, 2)
//    val off = 3
//    val len = 3

//    val sh = Vector(1, 2, 2, 2)
//    val off = 5
//    val len = 3

//    val sh = Vector(1, 1, 2, 2)
//    val off = 0
//    val len = 3

    val sh = Vector(2, 3, 4, 5)
    val off = 6
    val len = 21

    println(indicesStr(off, len, sh))
    val res   = partition(sh, off, len)
    val resSz = res.map(_.map(_.size).product).sum
    assert(resSz == len, resSz)
  }

  def run(): Unit = {
    val shape = Vector(5, 6, 7, 8)
    val vSize = shape.product
    val rank  = shape.size
    val v     = new Variable[Int](Vector(1 to vSize: _*), shape)

    lazy val fibs: Stream[Int] = 0 #:: fibs.scanLeft(1)(_ + _)
    val chunkSizes = fibs.drop(2).takeWhile(_ <= vSize).toVector
    println(s"There are ${chunkSizes.size} chunk-sizes")
    var statMaxReads = 0

    def numRanges(n: Int) = n * (n + 1) / 2

    val numIter   = shape.map(numRanges).product
    var iter      = 0
    var lastProg  = 0

    println("_" * 100)

    def loop(sec: Vector[Range]): Unit =
      if (sec.size < rank) {
        val max = shape(sec.size)
        for {
          start <- 0     until max
          stop  <- start until max
        } {
          loop(sec :+ (start to stop))
        }

      } else {
        val secSize = sec.map(_.size).product
        for (chunk <- chunkSizes) {
          val reader = new ChunkReader(v, sec)
          val b = Vector.newBuilder[Int]
          for (pos <- 0 until secSize by chunk) {
            val len   = math.min(secSize - pos, chunk)
            var ok    = false
            try {
              val data = reader.read(len)
              assert(data.size == len, s"data.size ${data.size} != len $len, data $data")
              b ++= data
              ok = true
            } finally {
              if (!ok)
                println(s"FAILED for chunk = $chunk, pos = $pos, len = $len; secSize $secSize; sec = $sec")
            }
          }
          val res = b.result()
          val direct = v.read(sec)
          assert(res.size == secSize, s"res.size = ${res.size}; secSize = $secSize; chunk = $chunk; res = $res; direct = $direct")
          assert(res == direct, s"chunk = $chunk; res = $res; direct = $direct")
          statMaxReads = math.max(statMaxReads, reader.statMaxReads)
        }

        iter += 1
        val prog = (iter * 100) / numIter
        while (lastProg < prog) {
          print('#')
          lastProg += 1
        }
      }

    loop(Vector.empty)
    println()
    println(s"Alright! Maximum reads was $statMaxReads.")
  }
}
