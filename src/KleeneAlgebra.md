Idempotent Semirings and Kleene Algebras
========================================

This module provides the definition of an idempotent semiring and a
Kleene algebra in terms of type classes and some instances thereof.

``` {.sourceCode .literate .haskell}
module KleeneAlgebra where
```

``` {.sourceCode .literate .haskell}
import Data.Ord ( comparing )
```

``` {.sourceCode .literate .haskell}
infixr 6 .*.
infixr 5 .+.
```

``` {.sourceCode .literate .haskell}
class IdempotentSemiring s where
    (.+.), (.*.)  :: s -> s -> s
    zero, one     :: s
    isZero, isOne :: s -> Bool
```

``` {.sourceCode .literate .haskell}
class IdempotentSemiring k => KleeneAlgebra k where
   star :: k -> k
```

We require that the following laws are satisfied for all `a, b, c :: s`:

-   `(.+.)` is associative, commutative, idempotent and has `zero` as
    the neutral element.
-   `(.*.)` is associative and has `one` as the neutral element.
-   `(.*.)` distributes over `(.+.)` from both sides, i.e.

    `a .*. (b .+. c) == (a .*. b) .+. (a .*. c)`

    `(b .+. c) .*. a == (b .*. a) .+. (c .*. a)`
-   `zero` is annihilating, i.e.

    `a .*. zero == zero == zero .*. a`

-   `isZero x` \<==\> `x == zero`
-   `isOne  x` \<==\> `x == one`

The latter laws use an equality on their right-hand side, which is meant
to be abstract, but to coincide with a concrete equality in case it
exists.

Boolean values form a semiring.

``` {.sourceCode .literate .haskell}
instance IdempotentSemiring Bool where
  (.+.) = (||)
  (.*.) = (&&)
  zero  = False
  one   = True
  isZero = not
  isOne  = id
```

``` {.sourceCode .literate .haskell}
instance KleeneAlgebra Bool where
  star = const one
```

Another classical semiring is the tropical semiring that can be used to
compute lightest paths. This semiring is usually defined on the
non-negative real numbers with infinity, but it can be easily
generalised.

``` {.sourceCode .literate .haskell}
data Tropical w = MinWeight | MaxWeight | Weight { weight :: w }
    deriving Eq
```

``` {.sourceCode .literate .haskell}
instance Show a => Show (Tropical a) where
  show MinWeight = "Min"
  show MaxWeight = "Max"
  show (Weight w) = unwords ["Weight", show w]
```

``` {.sourceCode .literate .haskell}
instance Ord w => Ord (Tropical w) where
    compare MinWeight  _           = LT
    compare _          MinWeight   = GT
    compare MaxWeight  _           = GT
    compare _          MaxWeight   = LT
    compare w          w'          = comparing weight w w'
```

``` {.sourceCode .literate .haskell}
instance Bounded (Tropical w) where
  minBound = MinWeight
  maxBound = MaxWeight
```

`Tropical w` forms a semiring if `w` has an `Ord` and a `Monoid`
instance that satisfy `` forall x, y :: w . x <= x `mappend` y ``. For
simplicity, we use only numerical monoids with the implicit condition
that only values greater or equal to zero are allowed as weights.

This condition is *not* satisfied in case of the additive monoid of most
numerical values, because adding negative numbers makes values smaller.

Assuming the expansion property, we can define an `Enum` instance for
`Tropical`. The `Int` representation uses `[-2, .., maxBound]`, where
(-2) is mapped to `MinWeight`, `maxBound` is mapped to `MaxWeight` and
every value in between is mapped to the predecessor of the given
representation on the inner values.

``` {.sourceCode .literate .haskell}
instance Enum w => Enum (Tropical w) where
  toEnum (-2) = MinWeight
  toEnum x    | x > -2 && x < maxBound = Weight (toEnum (x + 1))
              | x == maxBound          = MaxWeight
              | otherwise              = error "Not a valid weight."
  
  fromEnum MinWeight  = -2
  fromEnum MaxWeight  = maxBound
  fromEnum (Weight w) = fromEnum w - 1
```

Assuming the expansion property again, one can define an order on the
tropical semiring.

``` {.sourceCode .literate .haskell}
instance (Ord w, Num w) => IdempotentSemiring (Tropical w) where
    (.+.) = min
    zero  = MaxWeight
    one   = MinWeight

    MaxWeight .*. _         = MaxWeight
    _         .*. MaxWeight = MaxWeight
    MinWeight .*. x         = x
    x         .*. MinWeight = x
    Weight w  .*. Weight w' = Weight (w + w')
    isZero MaxWeight = True
    isZero _         = False
    
    isOne MinWeight  = True
    isOne _          = False
```

Additionally, this semiring is also a Kleene algebra and its star
operation is again the function `const one`

``` {.sourceCode .literate .haskell}
instance (Ord w, Num w) => KleeneAlgebra (Tropical w) where
  star = const one
```

Given two idempotent semiring their direct product is an idempotent
semiring as well, where all operations and constants are taken
component-wise.

``` {.sourceCode .literate .haskell}
instance (IdempotentSemiring a, IdempotentSemiring b) => IdempotentSemiring (a, b) where
  (xl, yl) .+. (xr, yr) = (xl .+. xr, yl .+. yr)
  (xl, yl) .*. (xr, yr) = (xl .*. xr, yl .*. yr)
  zero                  = (zero, zero)
  one                   = (one, one)
  isZero (x, y)         = isZero x && isZero y
  isOne (x, y)          = isOne x && isOne y
```

Similarly, the direct product of two Kleene algebras is again a Kleene
algebra.

``` {.sourceCode .literate .haskell}
instance (KleeneAlgebra a, KleeneAlgebra b) => KleeneAlgebra (a, b) where
  star (x, y) = (star x, star y)
```
