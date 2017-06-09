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

