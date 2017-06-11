        [9.796890258789062, 4.949371337890625, 2.3027191162109375, 0.0, 0.8529052734375, -1.7083587646484375, 0.0322113037109375, 4.9380645751953125, 4.80035400390625, 0.7451934814453125, 14.553543090820312, 14.195587158203125]

time 15
lat 2
alt 213

-------------

GenView.Factory for FScape.Output:


        context.acquire[Rendering[S]](_fscape) {
          val ugbCtx = new ContextImpl(_fscape)    // stateless
          RenderingImpl(_fscape, ugbCtx, config, force = false)
        }

As far as I can see, there is no large benefit in caching the rendering, as the rendering is relatively light weight in that it will cache itself the structure of the fscape graph.
That means, we don't need to run context.acquire, however rendering-impl needs a context object; I think it only ever extracts context.{cursor, workspaceHandle} from it.

# RenderingImpl

CacheValue: carries files and output data
in our case, output data will be empty, and files will be the one single cache file, obtained from Cache.instance

we need a variant of RenderingImpl that is more light-weight (we don't need to handle Gen(View))


----

# `Matrix[S]` vs `Matrix.Key`

- ugen graph builder relies on the former (so far)

we have this for reduce-impl:

        def getKey(streamDim: Int)(implicit tx: S#Tx): Matrix.Key = {
          mkReduceReaderFactory(op, inKey = in.getKey(streamDim), inShape = in.shape, dimIdx = indexOfDim, streamDim = streamDim)
        }

the eager creation of `inKey` is a problem then.

eventually we'll need it for `AudioFileCache.acquire(matrixKey)`

Analysis:

- Matrix.Key is both the reader factory and the cache key
- only the cache key needs to be immutably deserializable
- we divide this into Matrix.ReaderFactory and Matrix.Key,
  then the former can be without serialization, and we can
  have a ReaderFactory.Average that holds a `in: stm.Source[S#Tx, Matrix[S]]`.
  And that in turn means we can have a simple variant of
  `UGenGraphBuilderContextImpl` which instead of carrying
  an `FScape[S]` has a simple in-memory map for "attribute" keys.
  And then this means we can reuse most of the existing things
  in place, including `graph.Matrix` and `graph.MatrixOut`.

--------

# Problems

the rt code builds multiple readers, which means multiple instances of RF.Avg are created.
(this should only be a slight performance issue, but not a real problem, because rendering
will be cached, or not?)

`requestVarSpec` ends up with a longitude reader --- why?

das kommt wiederum von `Matrix(blobs).play(Dim(Var("anom"),time).play(UserValue$GE(control,UserValue(speed,6.0))))` ?

- ok, so the multiple invocations are unfortunate but correct

It seems though that the FScape graph is prematurely terminated (for the full 7200 matrix).
Perhaps it's a problem of two graphs running on the same akka actor system?

Or the patch didn't play nicely with the matrix of size 1? In any case, can we shortcut
`prepareDimensionReader` for avg when the dimension equals the avg dimension?

-------------

log:

- four 'new' calls: 3x temp, 1x longitude
- caching seems to work to some degree, as only
  two times the streaming is actually performed, and
  in fact two output files are generated
- so there are two possibilities: the "Output was not provided"
  refers to the two rendered instances, or to the two caches
- the exception would be thrown from the blob-detection offline
  graph?
- note: the exception is only thrown once but printed twice


    --RF-- avg6c763ab1 new(Temperature, AverageKey(Reduce.Key.Transparent(5x30-climatology_2001-05-01_2016-05-01_ta_anom, Temperature, streamDim = -1, section = [0 until 180][0 until 12][17 to 17][180 to 360]),-1,Vector(Range 0 until 180, Range 0 until 1, Range 0 until 1, Range 0 until 181),Vector(Longitude)))
    --RF-- avg6c763ab1 reader(); uState = Complete(accepted: [AttributeKey(out), Dim(Matrix(in),Longitude).size, Matrix(in), Matrix(in).spec.reduce(Dim(Matrix(in),Longitude))], outputs: [])
    --RF-- avg6e8fb317 new(Temperature, AverageKey(Reduce.Key.Transparent(5x30-climatology_2001-05-01_2016-05-01_ta_anom, Temperature, streamDim = -1, section = [0 until 180][0 until 12][17 to 17][180 to 360]),-1,Vector(Range 0 until 180, Range 0 until 1, Range 0 until 1, Range 0 until 181),Vector(Longitude)))
    --RF-- avg5054e04 new(Temperature, AverageKey(Reduce.Key.Transparent(5x30-climatology_2001-05-01_2016-05-01_ta_anom, Temperature, streamDim = -1, section = [0 until 180][0 until 12][17 to 17][180 to 360]),-1,Vector(Range 0 until 180, Range 0 until 1, Range 0 until 1, Range 0 until 181),Vector(Longitude)))
    --RF-- avg7ce858b9 new(Longitude, AverageKey(Reduce.Key.Transparent(5x30-climatology_2001-05-01_2016-05-01_ta_anom, Longitude, streamDim = 0, section = [0 until 12]),0,Vector(Range 0 until 1),Vector(Longitude)))
    --RF-- avg7ce858b9 reader(); uState = Complete(accepted: [AttributeKey(out), Dim(Matrix(in),Longitude).size, Matrix(in), Matrix(in).spec.reduce(Dim(Matrix(in),Longitude))], outputs: [])
    --RF-- avg6c763ab1 runExpanded
    --RF-- avg7ce858b9 runExpanded
    --RF-- avg7ce858b9 runExpanded cache: List(/home/hhrutz/mellite/cache/fscape8287189673420720522.bin)
    --RF-- avg7ce858b9 tKey Reduce.Key.Transparent(fscape8287189673420720522, Longitude, streamDim = 0, section = [0 until 1])
    --RF-- avg6c763ab1 runExpanded cache: List(/home/hhrutz/mellite/cache/fscape1697324117346701235.bin)
    --RF-- avg6c763ab1 tKey Reduce.Key.Transparent(fscape1697324117346701235, Temperature, streamDim = -1, section = [0 until 180][0 until 1][0 until 1][0 until 181])
    total-blobs: 33.0
    mOut.size: 7200
    java.lang.IllegalStateException: Output was not provided
        at de.sciss.fscape.lucre.UGenGraphBuilder$OutputRefImpl.writer(UGenGraphBuilder.scala:524)
        at de.sciss.fscape.lucre.impl.RenderingImpl$.$anonfun$withState$2(RenderingImpl.scala:105)
        at scala.collection.immutable.List.foreach(List.scala:389)
        at de.sciss.fscape.lucre.impl.RenderingImpl$.$anonfun$withState$1(RenderingImpl.scala:102)
        at scala.util.Success.$anonfun$map$1(Try.scala:251)
        at scala.util.Success.map(Try.scala:209)
        at scala.concurrent.Future.$anonfun$map$1(Future.scala:287)
        at scala.concurrent.impl.Promise.liftedTree1$1(Promise.scala:29)
        at scala.concurrent.impl.Promise.$anonfun$transform$1(Promise.scala:29)
        at scala.concurrent.impl.CallbackRunnable.run(Promise.scala:60)
        at scala.concurrent.impl.ExecutionContextImpl$AdaptedForkJoinTask.exec(ExecutionContextImpl.scala:140)
        at java.util.concurrent.ForkJoinTask.doExec(ForkJoinTask.java:289)
        at java.util.concurrent.ForkJoinPool$WorkQueue.runTask(ForkJoinPool.java:1056)
        at java.util.concurrent.ForkJoinPool.runWorker(ForkJoinPool.java:1692)
        at java.util.concurrent.ForkJoinWorkerThread.run(ForkJoinWorkerThread.java:157)
    java.lang.IllegalStateException: Output was not provided
        at de.sciss.fscape.lucre.UGenGraphBuilder$OutputRefImpl.writer(UGenGraphBuilder.scala:524)
        at de.sciss.fscape.lucre.impl.RenderingImpl$.$anonfun$withState$2(RenderingImpl.scala:105)
        at scala.collection.immutable.List.foreach(List.scala:389)
        at de.sciss.fscape.lucre.impl.RenderingImpl$.$anonfun$withState$1(RenderingImpl.scala:102)
        at scala.util.Success.$anonfun$map$1(Try.scala:251)
        at scala.util.Success.map(Try.scala:209)
        at scala.concurrent.Future.$anonfun$map$1(Future.scala:287)
        at scala.concurrent.impl.Promise.liftedTree1$1(Promise.scala:29)
        at scala.concurrent.impl.Promise.$anonfun$transform$1(Promise.scala:29)
        at scala.concurrent.impl.CallbackRunnable.run(Promise.scala:60)
        at scala.concurrent.impl.ExecutionContextImpl$AdaptedForkJoinTask.exec(ExecutionContextImpl.scala:140)
        at java.util.concurrent.ForkJoinTask.doExec(ForkJoinTask.java:289)
        at java.util.concurrent.ForkJoinPool$WorkQueue.runTask(ForkJoinPool.java:1056)
        at java.util.concurrent.ForkJoinPool.runWorker(ForkJoinPool.java:1692)
        at java.util.concurrent.ForkJoinWorkerThread.run(ForkJoinWorkerThread.java:157)

-----

Ok, next. NPE in MatrixOut.AbstractLogic -> onComplete(Success) after writing vars apparently.

---

blank cache
    
    --RF-- avg15baf19c new(Temperature, AverageKey(Reduce.Key.Transparent(5x30-climatology_2001-05-01_2016-05-01_ta_anom, Temperature, streamDim = -1, section = [0 until 180][0 until 12][18 to 18][180 to 360]), streamDim = -1, section = [Range 0 until 180, Range 0 until 1, Range 0 until 1, Range 0 until 181], avgDims = [Longitude]))
    --RF-- avg15baf19c requestOutput(avg-out)
    --RF-- avg15baf19c reader(); uState = Complete(accepted: [Dim(Matrix(in),Longitude).size, Matrix(in), Matrix(in).spec.reduce(Dim(Matrix(in),Longitude))], outputs: [avg-out])
    --RF-- avg12e23c19 new(Temperature, AverageKey(Reduce.Key.Transparent(5x30-climatology_2001-05-01_2016-05-01_ta_anom, Temperature, streamDim = -1, section = [0 until 180][0 until 12][18 to 18][180 to 360]), streamDim = -1, section = [Range 0 until 180, Range 0 until 1, Range 0 until 1, Range 0 until 181], avgDims = [Longitude]))
    --RF-- avg472bf641 new(Temperature, AverageKey(Reduce.Key.Transparent(5x30-climatology_2001-05-01_2016-05-01_ta_anom, Temperature, streamDim = -1, section = [0 until 180][0 until 12][18 to 18][180 to 360]), streamDim = -1, section = [Range 0 until 180, Range 0 until 1, Range 0 until 1, Range 0 until 181], avgDims = [Longitude]))
    --RF-- avg691480e0 new(Longitude, AverageKey(Reduce.Key.Transparent(5x30-climatology_2001-05-01_2016-05-01_ta_anom, Longitude, streamDim = 0, section = [0 until 12]), streamDim = 0, section = [Range 0 until 1], avgDims = [Longitude]))
    --RF-- avg691480e0 requestOutput(avg-out)
    --RF-- avg691480e0 reader(); uState = Complete(accepted: [Dim(Matrix(in),Longitude).size, Matrix(in), Matrix(in).spec.reduce(Dim(Matrix(in),Longitude))], outputs: [avg-out])
    --RF-- avg691480e0 tKey Reduce.Key.Transparent(fscape1057418049092315810, Longitude, streamDim = 0, section = [0 until 1])
    --RF-- avg15baf19c tKey Reduce.Key.Transparent(fscape6682111626858504412, Temperature, streamDim = -1, section = [0 until 180][0 until 1][0 until 1][0 until 181])
    total-blobs: 33.0
    mOut.size: 7200

change space (2.5N to 2.5S) -- notice how tKey for Longitude is (correctly) missing (cached).
    
    --RF-- avg70c64c0e new(Temperature, AverageKey(Reduce.Key.Transparent(5x30-climatology_2001-05-01_2016-05-01_ta_anom, Temperature, streamDim = -1, section = [0 until 180][0 until 12][17 to 17][180 to 360]), streamDim = -1, section = [Range 0 until 180, Range 0 until 1, Range 0 until 1, Range 0 until 181], avgDims = [Longitude]))
    --RF-- avg70c64c0e requestOutput(avg-out)
    --RF-- avg70c64c0e reader(); uState = Complete(accepted: [Dim(Matrix(in),Longitude).size, Matrix(in), Matrix(in).spec.reduce(Dim(Matrix(in),Longitude))], outputs: [avg-out])
    --RF-- avg621d85e new(Temperature, AverageKey(Reduce.Key.Transparent(5x30-climatology_2001-05-01_2016-05-01_ta_anom, Temperature, streamDim = -1, section = [0 until 180][0 until 12][17 to 17][180 to 360]), streamDim = -1, section = [Range 0 until 180, Range 0 until 1, Range 0 until 1, Range 0 until 181], avgDims = [Longitude]))
    --RF-- avg56f464fa new(Temperature, AverageKey(Reduce.Key.Transparent(5x30-climatology_2001-05-01_2016-05-01_ta_anom, Temperature, streamDim = -1, section = [0 until 180][0 until 12][17 to 17][180 to 360]), streamDim = -1, section = [Range 0 until 180, Range 0 until 1, Range 0 until 1, Range 0 until 181], avgDims = [Longitude]))
    --RF-- avg9356f3f new(Longitude, AverageKey(Reduce.Key.Transparent(5x30-climatology_2001-05-01_2016-05-01_ta_anom, Longitude, streamDim = 0, section = [0 until 12]), streamDim = 0, section = [Range 0 until 1], avgDims = [Longitude]))
    --RF-- avg9356f3f requestOutput(avg-out)
    --RF-- avg9356f3f reader(); uState = Complete(accepted: [Dim(Matrix(in),Longitude).size, Matrix(in), Matrix(in).spec.reduce(Dim(Matrix(in),Longitude))], outputs: [avg-out])
    --RF-- avg70c64c0e tKey Reduce.Key.Transparent(fscape8912670812128998722, Temperature, streamDim = -1, section = [0 until 180][0 until 1][0 until 1][0 until 181])
    total-blobs: 24.0
    mOut.size: 7200
    java.lang.IllegalStateException: Output out was not provided
        at de.sciss.fscape.lucre.impl.AbstractOutputRef.writer(AbstractOutputRef.scala:40)
        at de.sciss.fscape.lucre.impl.AbstractOutputRef.writer$(AbstractOutputRef.scala:38)
        at de.sciss.fscape.lucre.UGenGraphBuilder$OutputRefImpl.writer(UGenGraphBuilder.scala:403)
        at de.sciss.fscape.lucre.impl.RenderingImpl$.$anonfun$withState$2(RenderingImpl.scala:112)
        at scala.collection.immutable.List.foreach(List.scala:389)
        at de.sciss.fscape.lucre.impl.RenderingImpl$.$anonfun$withState$1(RenderingImpl.scala:109)
        at scala.util.Success.$anonfun$map$1(Try.scala:251)
        at scala.util.Success.map(Try.scala:209)
        at scala.concurrent.Future.$anonfun$map$1(Future.scala:287)
        at scala.concurrent.impl.Promise.liftedTree1$1(Promise.scala:29)
        at scala.concurrent.impl.Promise.$anonfun$transform$1(Promise.scala:29)
        at scala.concurrent.impl.CallbackRunnable.run(Promise.scala:60)
        at scala.concurrent.impl.ExecutionContextImpl$AdaptedForkJoinTask.exec(ExecutionContextImpl.scala:140)
        at java.util.concurrent.ForkJoinTask.doExec(ForkJoinTask.java:289)
        at java.util.concurrent.ForkJoinPool$WorkQueue.runTask(ForkJoinPool.java:1056)
        at java.util.concurrent.ForkJoinPool.runWorker(ForkJoinPool.java:1692)
        at java.util.concurrent.ForkJoinWorkerThread.run(ForkJoinWorkerThread.java:157)
    java.lang.IllegalStateException: Output out was not provided
        at de.sciss.fscape.lucre.impl.AbstractOutputRef.writer(AbstractOutputRef.scala:40)
        at de.sciss.fscape.lucre.impl.AbstractOutputRef.writer$(AbstractOutputRef.scala:38)
        at de.sciss.fscape.lucre.UGenGraphBuilder$OutputRefImpl.writer(UGenGraphBuilder.scala:403)
        at de.sciss.fscape.lucre.impl.RenderingImpl$.$anonfun$withState$2(RenderingImpl.scala:112)
        at scala.collection.immutable.List.foreach(List.scala:389)
        at de.sciss.fscape.lucre.impl.RenderingImpl$.$anonfun$withState$1(RenderingImpl.scala:109)
        at scala.util.Success.$anonfun$map$1(Try.scala:251)
        at scala.util.Success.map(Try.scala:209)
        at scala.concurrent.Future.$anonfun$map$1(Future.scala:287)
        at scala.concurrent.impl.Promise.liftedTree1$1(Promise.scala:29)
        at scala.concurrent.impl.Promise.$anonfun$transform$1(Promise.scala:29)
        at scala.concurrent.impl.CallbackRunnable.run(Promise.scala:60)
        at scala.concurrent.impl.ExecutionContextImpl$AdaptedForkJoinTask.exec(ExecutionContextImpl.scala:140)
        at java.util.concurrent.ForkJoinTask.doExec(ForkJoinTask.java:289)
        at java.util.concurrent.ForkJoinPool$WorkQueue.runTask(ForkJoinPool.java:1056)
        at java.util.concurrent.ForkJoinPool.runWorker(ForkJoinPool.java:1692)
        at java.util.concurrent.ForkJoinWorkerThread.run(ForkJoinWorkerThread.java:157)
    
