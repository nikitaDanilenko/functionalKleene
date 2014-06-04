~~~{.haskell}
module FunctionalKleene where
~~~

~~~{.haskell}
class IdempotentSemiring s where
    (.+.), (.*.)  :: s -> s_ -> s_
    zero, one     :: s
    isZero, isOne :: s -> Bool
~~~

> class IdempotentSemiring k_ => KleeneAlgebra k_ where
>    star :: k_ -> k_

> type Arc a_ = (Int, a_)
> type Row a_ = [Arc a_]
> type Mat a_ = [Row a_]

(.!.) :: IdempotentSemiring s_ => Row s_ -> Int -> s_
[]             .!. _ = zero
((i, v) : ivs) .!. k | i == k    = v
                     | i <  k    = ivs .!. k
                     | otherwise = zero

scale :: IdempotentSemiring s_ => s_ -> Row s_ -> Row s_
scale s = filter (not . isZero . snd) . map (\ (i, v) -> (i, s .*. v))

(*>) :: IdempotentSemiring s_ => s_ -> Row s_ -> Row s_
s *> row | isZero  s = []
         | isOne   s = row
         | otherwise = filter (not . isZero . snd) (map (\(i, v) -> (i, s .*. v)) row)

(<+>) :: IdempotentSemiring s_ => Row s_ -> Row s_ -> Row s_
[]            <+> y                         =  y
x             <+> []                        =  x
x@((i,v):ivs) <+> y@((j,w):jws) | i  ==  j  = (i, v .+. w) : (ivs <+> jws)
                                | i  <   j  = (i, v)       : (ivs <+> y)
                                | otherwise = (j, w)       : (x   <+> jws)

shape :: [a_] -> [Int]
shape = zipWith const [0 ..]

kleeneClosure :: KleeneAlgebra k_ => Mat k_ -> Mat k_
kleeneClosure a = tau a (shape a)

tau :: KleeneAlgebra k_ => Mat k_ -> [Int] -> Mat k_
tau = foldr newMat

newMat :: KleeneAlgebra k_ => Int -> Mat k_ -> Mat k_
newMat k a = map (\aj -> (aj .!. k) .*. star (ak .!. k) *> ak <+> aj) a
  where ak = a !! k