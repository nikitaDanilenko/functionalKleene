> module FunctionalKleene where

> import Control.Arrow               ( first, second )
> import Data.Array                  ( Array, listArray, (!), bounds, range, elems, assocs )
> import Data.Function               ( on )
> import Data.List                   ( groupBy, sortBy, intercalate )
> import Data.Ord                    ( comparing )

> import KleeneAlgebra               ( IdempotentSemiring ( .. ), KleeneAlgebra ( .. ) )
> import qualified DolanClosure as D ( Matrix ( .. ) )
> import RandomMatrix                ( chopUniform, MatLike )

Rows and auxiliary functions
============================

Rows are wrapped in an additional wrapper for a more legible output.

> newtype Row a = Row { unRow :: [(Int, a)] }

A pretty-printing `Show`-instance for vectors.

> instance Show a => Show (Row a) where
>
>  show (Row ixs) = foldr showElem "" ixs
>    where showElem (i, x) ixs = unwords ["(", show i, "|", show x, ")", ixs]

The `Functor` instance maps the given function over every value (i.e. second component) in
the association list.

> instance Functor Row where
>
>  fmap f = Row . map (second f) . unRow

For example we have the following result:

* `fmap chr (Row [(2, 109), (3, 97), (5, 112)]) == Row [(2, 'm'), (3, 'a'), (5, 'p')]`

We use the constant `emptyRow` for abbreviation.

> emptyRow :: Row a
> emptyRow = Row []

This function checks whether the wrapped association list of a vector is empty.

> isEmptyRow :: Row a -> Bool
> isEmptyRow = null . unRow

To avoid manual unwrapping and wrapping back, we provide a filter function for rows.

> filterRow :: (a -> Bool) -> Row a -> Row a
> filterRow p = Row . filter (p . snd) . unRow

Using the above filter function we can easily remove all zero values from a row as follows.

> removeZeroes :: IdempotentSemiring s => Row s -> Row s
> removeZeroes = filterRow (not . isZero)

This function transforms an association list into an array by sorting the indices and taking the
first occurrence of a value at an index.

> toRowFromList :: [(Int, a)] -> Row a
> toRowFromList = Row . map head . groupBy ((==) `on` fst) . sortBy (comparing fst)

Algebraic row functions
=======================

Querying of rows filled with semiring entries at a given position. This function returns `zero`
whenever the index is not in filled in the row.

> (.!.) :: IdempotentSemiring s => Row s -> Int -> s
> Row s .!. k = s .!!. k where
>   []             .!!. _ = zero
>   ((i, v) : ivs) .!!. k | i == k    = v
>                         | i <  k    = ivs .!!. k
>                         | otherwise = zero

A straightforward implementation of scalar multiplication of a row with a given scalar. This version
does not contain any simplifications.

> scale :: IdempotentSemiring s => s -> Row s -> Row s
> scale s = removeZeroes . fmap (s .*.)

A slightly more sophisticated scaling function, which checks the scalar for being `zero` or `one`
before actually mapping over the row. 

> infixr 5 *>
> (*>) :: IdempotentSemiring s => s -> Row s -> Row s
> s *> row | isZero  s = emptyRow
>          | isOne   s = row
>          | otherwise = removeZeroes (fmap (s .*.) row)

This function computes the sum of two rows.

> infixr 4 <+>
> (<+>) :: IdempotentSemiring s => Row s -> Row s -> Row s
> Row v <+> Row w = Row (v <++> w) where
>   []            <++> y                         =  y
>   x             <++> []                        =  x
>   x@((i,v):ivs) <++> y@((j,w):jws) | i  ==  j  = (i, v .+. w) : (ivs <++> jws)
>                                    | i  <   j  = (i, v)       : (ivs <++> y)
>                                    | otherwise = (j, w)       : (x   <++> jws)

> size :: Row s -> Int
> size = length . unRow

Matrices and auxiliary functions
================================

Matrices are wrapped in an additional newtype (contrary to the definition in the paper) to allow a
pretty-printing `Show` instance.

> newtype Mat a  = Mat { matrix :: [Row a] }

For a human-readable output this function prints matrices based on adjacency lists. 

> instance Show a => Show (Mat a) where
>
>   show = intercalate "\n" . map (uncurry f) . zip [0 ..] . matrix where
>     f j r = unwords [show j, ":", show r]

The Kleene closure as a right-fold
==================================

This function returns the indices that are present in a list.

> spine :: [a] -> [Int]
> spine = zipWith const [0 ..]

The Kleene closure itself is computed via the `spine` function and the auxiliary function `tau`.

> kleeneClosure :: KleeneAlgebra k => Mat k -> Mat k
> kleeneClosure a = tau a (spine (matrix a))

The auxiliary computation is a folding of the computation of a new matrix several times.

> tau :: KleeneAlgebra k => Mat k -> [Int] -> Mat k
> tau = foldr newMat

This function computes the next iteration step of the closure computation.

> newMat :: KleeneAlgebra k => Int -> Mat k -> Mat k
> newMat i a = Mat (map (\aj -> (aj .!. i) .*. star (ai .!. i) *> ai <+> aj) matA) where
>   ai   = matA !! i
>   matA = matrix a

The Kleene closure as a left-fold
=================================

This function computes the Kleene closure using a left fold instead of a right-fold.

> kleeneClosureLeft :: KleeneAlgebra k => Mat k -> Mat k
> kleeneClosureLeft a = foldl (flip newMat) a (spine (matrix a))

The Kleene closure based on arrays
==================================

For simplicity of representation we wrap arrays in a wrapper which is peeled of during compilation.

> newtype ArrayMat a = ArrayMat { unArrayMat :: Array (Int, Int) a }

> instance Show a => Show (ArrayMat a) where
>   show (ArrayMat a) = show (Mat (map (Row . zip [0 .. ]) (chopUniform (n + 1) (elems a)))) where
>     n = snd (snd (bounds a))

> kleeneClosureArray :: KleeneAlgebra k => ArrayMat k -> ArrayMat k
> kleeneClosureArray (ArrayMat a) = ArrayMat (foldl newArray a [0 .. n]) where
>
>   newArray arr k         = listArray bnds (map (newValue arr k) positions)
>   bnds                   = bounds a
>   positions              = range bnds
>   n                      = snd (snd bnds)
>
>   newValue arr k (i, j)  = ((arr ! (i, k)) .*. star (arr ! (k, k)) .*. (arr ! (k, j))) .+. arr ! (i, j)

Conversion functions
--------------------

We use a type class to allow simple conversions from pure association lists to a given matrix
representation and to compute the number of existing entries in a matrix, which is to say the number
of non-zero entries. This type class and its functions are designed mostly for simple testing and
debugging purposes.

> class Matrix m where
>
>     fromAssociations :: KleeneAlgebra k => MatLike k -> m k
>     toAssociations   :: KleeneAlgebra k => m k -> MatLike k

Clearly, both matrix versions from above are instances of this type class.

> instance Matrix Mat where
>   fromAssociations = Mat . map (toRowFromList . snd)
>   toAssociations   = zip [0..] . map unRow . matrix

> instance Matrix ArrayMat where
>   fromAssociations ascs = ArrayMat (listArray bnds (fuseWith zero (toPosValues ascs) (range bnds))) 
>     where toPosValues = concatMap (\(i, r) -> map (\(j, v) -> ((i, j), v)) r)
>           bnds        = ((0, 0), (n-1, n-1))
>           n           = length ascs
>   
>   toAssociations = map (second (filter (not . isZero . snd)))             -- remove zeroes in rows
>                  . map (\l@(((i, _), _) : _) -> (i, map (first snd) l ))  -- drop redundant indices
>                  . groupBy ((==) `on` (fst . fst))                        -- preprocess rows
>                  . assocs
>                  . unArrayMat
> 
> fuseWith :: Eq a => b -> [(a, b)] -> [a] -> [b]
> fuseWith zero = fuse where
>   fuse [] rest = map (const zero) rest
>   fuse ps@((p, v) : pvs) (a : azs) | p == a    = v    : fuse pvs azs
>                                    | otherwise = zero : fuse ps azs

The Dolan matrix representation is also an instance of `Matrix`

> instance Matrix D.Matrix where
>   fromAssociations [(_, [(_, x)])] = D.Scalar x
>   fromAssociations rs = D.Matrix (map ((\l -> fuseWith zero l list ) . snd) rs)
>     where list = map fst rs
>   
>   toAssociations (D.Scalar s)  = [(0, [(0, s)])]
>   toAssociations (D.Matrix rs) = zip [0..] (map (filter (not . isZero . snd) . zip [0..]) rs)