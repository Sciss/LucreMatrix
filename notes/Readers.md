Matrices are serialised, however, NetCDF backed matrices are 'thin'. Actual data access requires a `DataSource.Resolver` instance.

A reader requires a non-transactional representation, e.g. of the dimensional reductions. This representation is needed for caching purposes. It thus makes sense to define an opaque type `Key` for this representation. It must be possible to serialise these keys, e.g. in the file cache instance.

The API needed:

    trait Matrix {
      def getKey(streamDim: Int): Key
    }
    
    trait Key extends Writable {
      def reader()(implicit r: Resolver): Reader
    }
    
    object Key {
      def read(in: DataInput): Key
    }
    
    trait Resolver {
      def resolve(f: File): NetCdfFile
    }
    