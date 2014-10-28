> module Main ( main ) where

> import Control.DeepSeq    ( NFData ( .. ), deepseq )
> import System.Environment ( getArgs )
> import DolanClosure       ( Matrix ( .. ), kleeneClosureBlock )
> import FunctionalKleene   ( fromAssociations, kleeneClosure, kleeneClosureArray, kleeneClosureLeft,
>                             Row ( .. ), Mat ( .. ), ArrayMat ( .. ) )
> import RandomMatrix       ( randomSquareMatLike, mkStdGen, MatLike )
> import KleeneAlgebra      ( Tropical ( .. ), KleeneAlgebra )

Some instances of deepseq'able data.

> instance NFData a => NFData (Row a) where
>   rnf = rnf . unRow

> instance NFData a => NFData (Mat a) where
>   rnf = rnf . matrix

> instance NFData a => NFData (ArrayMat a) where
>   rnf = rnf . unArrayMat

> instance NFData a => NFData (Tropical a) where
>   rnf MinWeight  = ()
>   rnf MaxWeight  = ()
>   rnf (Weight w) = rnf w

> instance NFData a => NFData (Matrix a) where
>   rnf (Matrix rs) = rnf rs
>   rnf (Scalar s)  = rnf s

This function applies a closure function to a given matrix. The closure function is specified in
terms of a string. 

> mkFunction :: (KleeneAlgebra k, NFData k) => String -> MatLike k -> IO ()
> mkFunction "left"  matLike = kleeneClosureLeft  (fromAssociations matLike) `deepseq` return ()
> mkFunction "right" matLike = kleeneClosure      (fromAssociations matLike) `deepseq` return ()
> mkFunction "block" matLike = kleeneClosureBlock (fromAssociations matLike) `deepseq` return ()
> mkFunction _       matLike = kleeneClosureArray (fromAssociations matLike) `deepseq` return ()

This message is displayed, if the input does not match the required criteria.
        
> message :: String
> message = unlines ["Required arguments:", "\tsize :: Int", "\tdensity :: Double",
>                    "\trandomGenerator :: Int",
>                    "\ttype ::= t (tropical) | txb (pair of tropical and Boolean) | b (boolean)",
>                    "\tfunction ::= left | right | array | block"]

The (compiled!) main function expects a size, a density, a random generator, a type of Kleene algebra
and the closure function. It then creates a random matrix with the given size and the given density
from the given random generator and applies the closure function to the result.
Since this constitutes exactly one test, we ran the compiled program via an external script
called runTestsNew.sh

> main :: IO ()
> main =
>   do args <- getArgs
>      case args of
>        (s : d : r : ty : f : _) ->
>          let size = read s :: Int
>              dens = read d :: Double
>              gen  = mkStdGen (read r)
>          in case ty of
>               "t"   -> mkFunction f (randomSquareMatLike gen size dens (MinWeight, MaxWeight)                 :: MatLike (Tropical Int))
>               "txb" -> mkFunction f (randomSquareMatLike gen size dens ((MinWeight, False), (MaxWeight,True)) :: MatLike (Tropical Int, Bool))
>               _     -> mkFunction f (randomSquareMatLike gen size dens (True, True)                           :: MatLike Bool)
>        _ -> putStr message

              