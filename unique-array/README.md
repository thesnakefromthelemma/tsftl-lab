My attempt at
- Reimplementing `Data.Primitive.PrimArray` from the `primitive` package atop `ByteArray#` and `MutableByteArray#` from `GHC.Prim`
- Using the above to give an alternative API to mutable arrays in the `Control.Monad.ST.ST` monad that more strictly enforces uniqueness of references to memory via the universal quantification trick.
- Using the above in turn to give an alternative implementation of linearly mutable arrays more efficient than the one currently in `linear-base`.
- Redoing the above with arrays allocated on the foreign heap via `-XGHCForeignImportPrim`.

Currently incomplete. For motivation see [../add-inplace](https://github.com/thesnakefromthelemma/tsftl-lab/tree/master/add-inplace).