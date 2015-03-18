Idempotent Semirings and Kleene Algebras
========================================

This module provides the definition of an idempotent semiring and a
Kleene algebra in terms of type classes and some instances thereof.

``` {.haskell}
module KleeneAlgebra where
```

``` {.haskell}
import Data.Ord ( comparing )
```

``` {.haskell}
infixr 6 .*.
infixr 5 .+.
```

``` {.haskell}
class IdempotentSemiring s where
    (.+.), (.*.)  :: s -> s -> s
    zero, one     :: s
    isZero, isOne :: s -> Bool
```

``` {.haskell}
class IdempotentSemiring k => KleeneAlgebra k where
   star :: k -> k
```

``` {.haskell}
class IdempotentSemiring k => KAT k where
  sparseTests :: [k]
  isSimple    :: k -> Bool
  compute     :: k -> k
```

``` {.haskell}
katStar :: KAT k_ => k_ -> k_
katStar a = one .+. katPlus a
```

``` {.haskell}
katPlus :: KAT k_ => k_ -> k_
katPlus a = tau a sparseTests
```

``` {.haskell}
tau :: KAT k_ => k_ -> [k_] -> k_
tau a  []        = a
tau a  (t : ts)  = x .*. katStar (t .*. x) where x = tau a ts
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

``` {.haskell}
instance IdempotentSemiring Bool where
  (.+.) = (||)
  (.*.) = (&&)
  zero  = False
  one   = True
  isZero = not
  isOne  = id
```

``` {.haskell}
instance KleeneAlgebra Bool where
  star = const one
```

Another classical semiring is the tropical semiring that can be used to
compute lightest paths. This semiring is usually defined on the
non-negative real numbers with infinity, but it can be easily
generalised.

``` {.haskell}
data Tropical w = MinWeight | MaxWeight | Weight { weight :: w }
    deriving Eq
```

``` {.haskell}
instance Show a => Show (Tropical a) where
  show MinWeight = "Min"
  show MaxWeight = "Max"
  show (Weight w) = unwords ["Weight", show w]
```

``` {.haskell}
instance Ord w => Ord (Tropical w) where
    compare MinWeight  _           = LT
    compare _          MinWeight   = GT
    compare MaxWeight  _           = GT
    compare _          MaxWeight   = LT
    compare w          w'          = comparing weight w w'
```

``` {.haskell}
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

``` {.haskell}
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

``` {.haskell}
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

``` {.haskell}
instance (Ord w, Num w) => KleeneAlgebra (Tropical w) where
  star = const one
```

One particularly important idempotent semiring is the free semiring of
regular expressions over an alphabet. First, we provide the necessary
datatype.

``` {.haskell}
data Regular a = Letter a
               | NoWord
               | EmptyWord
               | Binary BinOp (Regular a) (Regular a)
               | Star (Regular a)
```

There are two binary constructors for regular expressions, namely the
alternative (addition) and the composition (multiplication).

``` {.haskell}
data BinOp = Alternative | Composition
```

A simple `Show` instance for the binary operations on regular
expressions.

``` {.haskell}
instance Show BinOp where
  show Alternative = "|"
  show Composition = "x"
```

We avoid unnecessary brackets by propagating the previously defined
precedences of the operations.

``` {.haskell}
precedenceOf :: BinOp -> Int
precedenceOf Alternative = 5
precedenceOf Composition = 6
```

This auxiliary function is used to decide whether we need brackets
around the regular expression.

``` {.haskell}
isComposite :: Regular a -> Bool
isComposite NoWord     = False
isComposite EmptyWord  = False
isComposite (Letter _) = False
isComposite _          = True
```

Constants which we use to show the constants and the star operation of
the `Regular` datatype.

``` {.haskell}
noWordString, emptyWordString, starString :: ShowS
noWordString    = showChar '_'
emptyWordString = showChar '-'
starString      = showChar '*'
```

We provide a `Show` instance for regular expressions that avoids
unnecessary brackets. For instance, the associativity of the addition is
reflected in the fact that `r .+. (s .+. t)` is displayed as
`r | s | t`.

``` {.haskell}
instance Show a => Show (Regular a) where
  showsPrec _ (Letter a)     = shows a
  showsPrec _ NoWord         = noWordString
  showsPrec _ EmptyWord      = emptyWordString
  showsPrec p (Binary b r s) = showParen (p > p') 
                                   (showsPrec p' r . shows b . showsPrec p' s)
                                where p' = precedenceOf b
  showsPrec p (Star r)       = showParen (isComposite r) (showsPrec p r) . starString
```

Regular expressions are a special type of semiring, namely the free
semiring. However, one has to take into account that equality of regular
expressions is actually symbol-wise equality and thus does not satisfy
the required semiring laws. This is simply remedied by defining two
regular expressions to be equal iff they describe the same language. In
theory one can define a function `language :: Regular a -> Set [a]`
(where `Set` is some sort of set representation) and then define an `Eq`
instance for regular expressions according to the above definition.
Unfortunately, this definition would not terminate as soon as the
regular expression contained the star operation of a non-zero and
non-one element. Since we don't require any of the algebraic rules in
the implementation, we omit this definition. Still, need to implement
some algebraic rules concerning the constants to (greatly!) improve the
performance in case of the Kleene closure computation of a matrix. In
particular, we deal with recognising `star one = one` and
`star zero = one` as well as the neutrality of `one` with respect to
`(.*.)`, the neutrality of `zero` with respect to `(.+.)` and the
annihilation property of `zero` with respect to `(.*.)`.

``` {.haskell}
instance IdempotentSemiring (Regular a) where
  zero  = NoWord
  
  isZero NoWord = True
  isZero _      = False
  
  one   = EmptyWord
  
  isOne EmptyWord = True
  isOne _         = False
  
  NoWord .+. r      = r
  r      .+. NoWord = r
  r      .+. s      = Binary Alternative r s
  
  NoWord    .*. _         = NoWord
  _         .*. NoWord    = NoWord
  EmptyWord .*. r         = r
  r         .*. EmptyWord = r
  r         .*. s         = Binary Composition r s
```

``` {.haskell}
instance KleeneAlgebra (Regular a) where
  star r         = Star r
```

The balance semiring (described in detail in: Vladimir Bagatelj,
"Semirings for Social Network Analysis", Journal of Mathematical
Sociology, pp. 53 -- 68) can be used to check the types of paths in a
graph. A signed graph /(G = (V, E), sign)/ is a graph equipped with a
function /sign : E -\> {P, N}/ where /P/ stands for /positive/ and /N/
stands for /negative/. A walk in a graph is called positive (resp.
negative) iff it contains an even (resp. odd) number of negative arcs. A
semiwalk is a walk in the symmetric closure of the graph. A graph is
called /balanced/ iff there is a partition of /V/ in two sets /A, B/
such that for all edges /e/ we have \* / sign e == P / ==\> /e in A x A/
or /e in B x B/ \* / sign e == N / ==\> /e in A x B/ or /e in B x A/
That is: every positive edge connects vertices of the same partition set
and every negative edge connects vertices of different partition sets
(cf. the above mentioned source for more detail). Balanced graphs can be
characterised as follows: a graph is balanced iff every closed semiwalk
is positive. This is where the balance semiring can be used. We define
the possible values of a path as follows.

``` {.haskell}
data Balance = NoPath | Positive | Negative | Mixed
  deriving ( Eq, Enum )
```

Here `NoPath` denotes that there is no path between two vertices,
`Positive` means a positive path, `Negative` means a negative path and
`Mixed` denotes a path that contains at least one positive and one
negative edge.

``` {.haskell}
instance Show Balance where
  show NoPath   = "0"
  show Positive = "+"
  show Negative = "-"
  show Mixed    = "~"
```

We can define a semiring instance for `Balance`. The basic idea is to
think about the composition of paths as multiplication and the
alternative of paths as addition.

``` {.haskell}
instance IdempotentSemiring Balance where
  zero = NoPath
  
  isZero NoPath = True
  isZero _      = False
  
  one  = Positive
  
  isOne Positive = True
  isOne _        = False
  
  NoPath .+. x      = x                  -- nothing or something is something
  x      .+. NoPath = x                  -- something or nothing is something
  Mixed  .+. _      = Mixed              -- if there are mixed paths, all choices are mixed
  _      .+. Mixed  = Mixed              
  x      .+. y      | x == y    = x      -- choose the first of two equal values
                    | otherwise = Mixed  -- if two values are different, there are mixed choices
   
  NoPath .*. _      = NoPath
  _      .*. NoPath = NoPath
  Mixed  .*. _      = Mixed
  _      .*. Mixed  = Mixed
  x      .*. y      | x == y    = Positive 
                    | otherwise = Negative
```

The star closure operation can be defined as follows:

``` {.haskell}
instance KleeneAlgebra Balance where
  star NoPath = Positive
  star Positive = Positive
  star _        = Mixed
```

The star closures of `NoPath, Negative` and `Mixed` can be easily
computed using the fixpoint equation `star x = one .+. x .*. star x`.
The equation `star Positive = Positive` is true because of the geometric
series representation of the star closure.

Given two idempotent semiring their direct product is an idempotent
semiring as well, where all operations and constants are taken
component-wise.

``` {.haskell}
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

``` {.haskell}
instance (KleeneAlgebra a, KleeneAlgebra b) => KleeneAlgebra (a, b) where
  star (x, y) = (star x, star y)
```
