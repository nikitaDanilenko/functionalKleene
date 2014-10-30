An Implementation of the Star Closure as Described in "Fun with Semirings" by Stephen Dolan
===========================================================================================

This module contains almost exactly the code from the above paper by
Stephen Dolan. To simplify tests, we used our type class for Kleene
algebras instead of the type class for semirings presented in the paper.

We refer to the excellent documentation in the paper for details about
the functions.

``` {.haskell}
module DolanClosure where
```

``` {.haskell}
import Data.List     ( transpose )
import KleeneAlgebra ( IdempotentSemiring ( .. ), KleeneAlgebra ( .. ) )
```

``` {.haskell}
data Matrix a = Scalar a | Matrix [[a]]
```

``` {.haskell}
type BlockMatrix a = (Matrix a, Matrix a, Matrix a, Matrix a)
```

``` {.haskell}
mjoin :: BlockMatrix a -> Matrix a
mjoin (Matrix a, Matrix b, Matrix c, Matrix d) =
  Matrix ((a `hcat` b) ++ (c `hcat` d)) 
      where hcat = zipWith (++)
```

``` {.haskell}
msplit :: Matrix a -> BlockMatrix a
msplit (Matrix (row:rows)) = 
  (Matrix [[first]], Matrix [top], Matrix left, Matrix rest)
     where (first:top) = row
           (left, rest) = unzip (map (\(x:xs) -> ([x],xs)) rows)
```

``` {.haskell}
instance IdempotentSemiring a => IdempotentSemiring (Matrix a) where
  zero = Scalar zero
  one = Scalar one

  Scalar a     .+. Scalar b = Scalar (a .+. b)
  Matrix a     .+. Matrix b = Matrix (zipWith (zipWith (.+.)) a b)
  Scalar s     .+. m        = m .+. Scalar s
  Matrix [[a]] .+. Scalar b = Matrix [[a .+. b]]
  m            .+. s        = mjoin (first .+. s, top, left, rest .+. s)
                              where (first, top, left, rest) = msplit m
  Scalar a  .*. Scalar b = Scalar (a .*. b)
  Scalar a  .*. Matrix b = Matrix (map (map (a .*.)) b)
  Matrix a  .*. Scalar b = Matrix (map (map (.*. b)) a)
  Matrix a  .*. Matrix b = Matrix [[foldl1 (.+.) (zipWith (.*.) row col) | col <- cols] | row <- a]
    where cols = transpose b
    
  isOne (Scalar x) = isOne x
  isOne _          = False
  
  isZero (Scalar x) = isZero x
  isZero _          = False
```

``` {.haskell}
instance KleeneAlgebra a => KleeneAlgebra (Matrix a) where
  star (Matrix [[x]]) = Matrix [[star x]]
  star m = mjoin 
      (first' .+. top' .*. rest' .*. left', top' .*. rest', 
       rest' .*. left',                   rest')
      where (first, top, left, rest) = msplit m
            first'                   = star first
            top'                     = first' .*. top
            left'                    = left .*. first'
            rest'                    = star (rest .+. left' .*. top)
```

``` {.haskell}
kleeneClosureBlock :: KleeneAlgebra k => Matrix k -> Matrix k
kleeneClosureBlock a = a .*. star a

starClosureBlock :: KleeneAlgebra k => Matrix k -> Matrix k
starClosureBlock = star
```
