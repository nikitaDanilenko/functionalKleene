Random Matrices as Pure Association Lists
=========================================

This module provides a simple generation of different types of random matrices (e.g. triangle or
diagonal matrices). The randomness is provided by the `System.Random` implementation and
is thus repeatable, which makes it very useful for testing.

> module RandomMatrix (
>     randomSquareMatLike,
>     randomDiagonalLike,
>     randomTriangleLike,
>     randomStrictTriangleLike,
> 
>     MatLike,
>     chopUniform,
> 
>     -- * Re-export from "System.Random" to simplify testing
>     mkStdGen,
>     StdGen,
>     Random
>     ) where
> 
> import Control.Arrow         ( first )
> 
> import Data.Maybe            ( mapMaybe )
> 
> import System.Random         ( Random (..), RandomGen, StdGen, split, mkStdGen )
> import System.Random.Shuffle ( shuffle' )
> 
> import KleeneAlgebra         ( Tropical ( .. ), Regular ( .. ), Balance )

Generation of random matrices
-----------------------------

For simplicity only the "pure association lists" of matrices are generated, which are represented
by the stacked association lists. Values of the type `MatLike a` are required to satisfy
the following two conditions:

* Its indices are exactly *0* to *n - 1* for some natural number *n* in precisely this order.
* The value at every index is an association list that is sorted with respect to its indices.

> type MatLike a = [(Int, [(Int, a)])]

This function generates the necessary values. First, it creates a table of the right size and fills
it with the correct number of "non-zero" values at the first positions and with "zeroes" at the
remaining positions. Then it uses the supplied random generator to shuffle the table. Finally, the
table is reduced to an association list.
Any density larger than 1 behaves as 1 and every density smaller than 0 behaves as 0.

> randomMatLikeWith :: (RandomGen g, Random a) =>
>     g                        -- ^ random generator
>  -> Int                      -- ^ number of rows
>  -> Int                      -- ^ number of columns
>  -> (Int -> Int -> Int)      -- ^ function that computes the number of entries
>                              --   in the matrix, i.e. @(*)@
>  -> ([Maybe a] -> MatLike a) -- ^ function that splits the overall list into
>                              --   sublists which then become rows
>  -> Double                   -- ^ density, i.e. percentage of edges, which is
>                              --   a 'Double' value between /0/ and /1/
>  -> (a, a)                   -- ^ Lower\/upper bounds for the random values
>  -> MatLike a
> randomMatLikeWith g rs cs size resize d lu = resize shuffled where
>   shuffled = shuffle' toGo entries g2
>   entries  = size rs cs
>   fill     = floor (fromIntegral entries * d)
>   toGo     =    map Just (take fill (randomRs lu g1)) -- \"interesting\" values
>              ++ replicate (entries - fill) Nothing    -- \"zeroes\"
>   (g1, g2) = split g

This function creates a random matrix by computing the necessary number of entries,
then shuffling them and finally splitting them into uniform chunks which are then
used as rows.

> randomMatLike ::
>   (RandomGen g, Random a) => g            -- ^ random generator
>                           -> Int          -- ^ number of rows
>                           -> Int          -- ^ number of colums
>                           -> Double       -- ^ density (/0 <= d <= 1/)
>                           -> (a, a)       -- ^ lower\/upper bounds
>                           -> MatLike a
> randomMatLike gen rows cols = randomMatLikeWith gen rows cols (*) (resizeWith (chopUniform cols))

A random graph is a random matrix with the same number of rows and columns.

> randomSquareMatLike ::
>   (RandomGen g, Random a) => g            -- ^ random generator
>                           -> Int          -- ^ number of rows and columns
>                           -> Double       -- ^ density (/0 <= d <= 1/)
>                           -> (a, a)       -- ^ lower\/upper bounds
>                           -> MatLike a
> randomSquareMatLike gen size = randomMatLike gen size size

Creates a random diagonal square matrix. Please note that the density is
computed w.r.t. the diagonal and *not* the number of entries altogether. That
is: `randomDiagonalLike (mkStdGen 1234) 10 0.3 (0, 1)` will create a square matrix
with exactly three (not thirty) entries.

> randomDiagonalLike ::
>   (RandomGen g, Random a) => g            -- ^ random generator
>                           -> Int          -- ^ number of rows and columns
>                           -> Double       -- ^ density (/0 <= d <= 1/)
>                           -> (a, a)       -- ^ lower\/upper bounds
>                           -> MatLike a
> randomDiagonalLike gen size = randomMatLikeWith gen size size const resize where
> 
>   resize = zipWith (\i mv -> (i, maybe [] (\v -> [(i, v)]) mv)) [0..]

Creates a random triangle square matrix. As with `randomDiagonalLike` the density
refers to the density of the triangle. That is the number of entries in the matrix will
be `floor (density * size * (size + 1) / 2)`.

> randomTriangleLike ::
>   (RandomGen g, Random a) => g            -- ^ random generator
>                           -> Int          -- ^ number of rows and columns
>                           -> Double       -- ^ density (/0 <= d <= 1/)
>                           -> (a, a)       -- ^ lower\/upper bounds
>                           -> MatLike a
> randomTriangleLike gen size = randomMatLikeWith gen size size f (resizeWith chopTriangle) where
> 
>   f n _ = n * (n + 1) `div` 2

Creates a random strict triangle matrix (no entries at the diagonal). The density refers to the
density of the strict triangle, that is the number of entries is
`floor (density * size * (size - 1) / 2)`.

> randomStrictTriangleLike ::
>   (RandomGen g, Random a) => g            -- ^ random generator
>                           -> Int          -- ^ number of rows and columns
>                           -> Double       -- ^ density (/0 <= d <= 1/)
>                           -> (a, a)       -- ^ lower\/upper bounds
>                           -> MatLike a
> randomStrictTriangleLike gen size = randomMatLikeWith gen size size f (resizeWith chopStrictTriangle)
>   where f n _ = n * (n - 1) `div` 2

Random instances
----------------

`Random` instance for pairs of Random instances that simply generates two values in
sequence.

> instance (Random a, Random b) => Random (a, b) where
> 
>   randomR ((la, lb), (ua, ub)) g = ((x, y), g'') where
>     (x, g')  = randomR (la, ua) g
>     (y, g'') = randomR (lb, ub) g'
>
>   random g = ((x, y), g'') where
>     (x, g')  = random g
>     (y, g'') = random g'

We define a simple instance of `Random` for `Tropical`.
When both bounds are actual weights, we use the inner values to generate a random value and
then wrap it in a `Weight` constructor.
When the first bound is `MinWeight`, we interpret it as `toEnum 0` and use the latter as a lower
bound, i.e. we identify `MinWeight == toEnum 0`. If none of the above applies, we ignore the bounds.
This way you cannot generate a `MaxWeight` value.

> instance (Random a, Ord a, Enum a) => Random (Tropical a) where
>   randomR (Weight l,  Weight u)  = wrapWeight . randomR (l, u)
>   randomR (MinWeight, Weight u)  = wrapWeight . randomR (toEnum 0, u)
>   randomR (Weight _,  MaxWeight) = wrapWeight . random
>   randomR (u, l)                 | u > l     = randomR (l, u)
>                                  | otherwise = wrapWeight . random
>   random  = randomBounded

> wrapWeight :: (Enum a, Eq a) => (a, x) -> (Tropical a, x)
> wrapWeight = first toWeight

> toWeight :: (Enum a, Eq a) => a -> Tropical a
> toWeight x | x == toEnum 0 = MinWeight
>            | otherwise     = Weight x

Our random instance for `Regular` creates only `Letter` values.
While this is a greatly simplified approach, it is a convenient restriction, 
because in the case of applications to graphs,
the edges are usually labelled with exactly such values and not composite ones
(the value `NoWord` typically represents "no edge" rather than an edge with no label,
similarly we omit the case of empty labels, which are represented by `EmptyWord`).
Additionally, we obtain a simple uniform distribution when letter bounds are used.

> instance (Random a, Enum a) => Random (Regular a) where
>   randomR (Letter l, Letter u) g = (Letter pos, g') where
>     (pos, g') = randomR (l, u) g
>   randomR _ g = (Letter pos, g') where
>     (pos, g') = random g
>   
>   random = randomR (NoWord, EmptyWord)

> instance Random Balance where
>   randomR = randomREnum
>   random  = randomBounded

Auxiliary functions
-------------------

Given a function, an integer and a list, this function breaks the list in sublists. The given
function is used to determine the length of the next chunk. For instance:

* `breakWith id    3 "Explanation" == ["Exp","lan","ati","on"]`
* `breakWith (+ 1) 1 "Explanation" == ["E","xp","lan","atio","n"]`

> breakWith :: (Int -> Int) -> Int -> [a] -> [[a]]
> breakWith f = go where
> 
>   go _ [] = []
>   go n xs = take n xs : go (f n) (drop n xs)

Given an integer *n* and a list this function breaks the list into chunks of length *n*. The
last chunk is shorter, iff the length of the given list is not a multiple of *n*.

> chopUniform :: Int -> [a] -> [[a]]
> chopUniform = breakWith id

This function chops a given list into sublists of increasing length beginning with 1. The last
element is shorter than the second to last, if the length of the list is not _n*(n+1)/2_ for
some natural number *n*

The name of the function hints at its use, since one can use the resulting chunks to fill a
lower triangle matrix.

> chopTriangle :: [a] -> [[a]]
> chopTriangle = breakWith (+ 1) 1

This function behaves very similarly to `chopTriangle`, but its first list is empty.
The last element of this list is shorter than the second to last iff the list length is not
_n * (n+1)/2_ for some integer *n*

Again, the name hints at the function's application, namely the construction of a strict lower
triangle matrix.

> chopStrictTriangle :: [a] -> [[a]]
> chopStrictTriangle = breakWith (+1) 0

One particular recurring scheme is to split a list of `Maybe a` values into lists of such values
and then transform these lists into rows. This scheme is captured by the following function.

> resizeWith :: ([Maybe a] -> [[Maybe a]]) -> [Maybe a] -> MatLike a
> resizeWith f = zip [0 .. ] . map toRow . f

This function transforms a list of `Maybe` values into an association list by first
indexing the list and then removing the `Nothing` values. For example,

* `toRow [Just 'h', Nothing, Just 'i'] == [(0, 'h'), (2, 'i')]`

> toRow :: [Maybe a] -> [(Int, a)]
> toRow = mapMaybe (uncurry (fmap . (,))) . zip [0 .. ]

This is a generic way to define the `random` function for bounded values. This implementation is the
same as the one in `System.Random`.

> randomBounded :: (RandomGen g, Random a, Bounded a) => g -> (a, g)
> randomBounded = randomR (minBound, maxBound)

Similarly, the following function is a generic way to define the `randomR` function for types that
are instances of `Enum` by falling back to the `Random` instance of `Int`.

> randomREnum :: (RandomGen g, Random a, Enum a) => (a, a) -> g -> (a, g)
> randomREnum (l, u) g = (toEnum x, g') where
>   (x, g') = randomR (fromEnum l, fromEnum u) g