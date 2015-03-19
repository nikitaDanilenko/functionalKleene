> {-# Language BangPatterns #-}

> module Main ( main ) where

> import Control.DeepSeq    ( NFData ( .. ), deepseq )
> import System.Environment ( getArgs )
> import DolanClosure       ( Matrix ( .. ), kleeneClosureBlock, starClosureBlock )
> import FunctionalKleene   ( fromAssociations, toAssociations,
>                             kleeneClosure, kleeneClosureArray, kleeneClosureLeft,
>                             Row ( .. ), Mat ( .. ), ArrayMat ( .. ) )
> import RandomMatrix       ( randomSquareMatLike, mkStdGen, MatLike )
> import KleeneAlgebra      ( Tropical ( .. ), Regular ( .. ), Balance ( Positive, Negative ),
>                             KleeneAlgebra ( star ) )

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

> instance NFData a => NFData (Regular a) where
>   rnf (Letter a)     = rnf a
>   rnf NoWord         = ()
>   rnf EmptyWord      = ()
>   rnf (Binary _ r s) = rnf r `seq` rnf s
>   rnf (Star r)       = rnf r

> instance NFData Balance where
>   rnf !_ = ()

> instance NFData a => NFData (Matrix a) where
>   rnf (Matrix rs) = rnf rs
>   rnf (Scalar s)  = rnf s

This function applies a closure function to a given matrix. The closure function is specified in
terms of a string. 

> mkFunction :: (KleeneAlgebra k, NFData k) => String -> MatLike k -> IO ()
> mkFunction "left"   matLike = kleeneClosureLeft  (fromAssociations matLike) `deepseq` return ()
> mkFunction "right"  matLike = kleeneClosure      (fromAssociations matLike) `deepseq` return ()
> mkFunction "block+" matLike = kleeneClosureBlock (fromAssociations matLike) `deepseq` return ()
> mkFunction "block*" matLike = starClosureBlock   (fromAssociations matLike) `deepseq` return ()           
> mkFunction "array"  matLike = kleeneClosureArray (fromAssociations matLike) `deepseq` return ()
> mkFunction _       _        = putStr message

This message is displayed, if the input does not match the required criteria.
        
> message :: String
> message = unlines ["Required arguments:", "\tsize :: Int", "\tdensity :: Double",
>                    "\trandomGenerator :: Int",
>                    "\ttype ::= t (tropical) "                       ++ 
>                             "| txb (pair of tropical and Boolean) " ++
>                             "| b (boolean) "                        ++
>                             "| r (regular expressions over lowercase letters) " ++
>                             "| bal (balance semiring)",
>                    "\tfunction ::= left | right | array | block+ | block*"]

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
>               "t"   -> mkFunction f (randomSquareMatLike gen size dens (Weight 1, Weight maxInt)                  :: MatLike (Tropical Integer))
>               "txb" -> mkFunction f (randomSquareMatLike gen size dens ((Weight 1, False), (Weight maxInt, True)) :: MatLike (Tropical Integer, Bool))
>               "r"   -> mkFunction f (randomSquareMatLike gen size dens (Letter 'a', Letter 'z')                   :: MatLike (Regular Char))
>               "bal" -> mkFunction f (randomSquareMatLike gen size dens (Positive, Negative)                       :: MatLike Balance)
>               _     -> mkFunction f (randomSquareMatLike gen size dens (True, True)                               :: MatLike Bool)
>        _ -> putStr message

> maxInt :: Integer
> maxInt = toEnum (maxBound :: Int)