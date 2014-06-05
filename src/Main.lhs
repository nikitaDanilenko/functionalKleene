> module Main ( main ) where

> import Control.DeepSeq    ( NFData ( .. ), deepseq )
> import System.Environment ( getArgs )
> import FunctionalKleene   ( fromAssociations, kleeneClosure, kleeneClosureArray, kleeneClosureLeft,
>                             Row ( .. ), Mat ( .. ), ArrayMat ( .. ) )
> import RandomMatrix       ( randomSquareMatLike, mkStdGen, MatLike )
> import KleeneAlgebra      ( Tropical ( .. ), KleeneAlgebra )

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
    
> mkFunction :: (KleeneAlgebra k, NFData k) => String -> MatLike k -> IO ()
> mkFunction "left"  matLike = kleeneClosureLeft  (fromAssociations matLike) `deepseq` return ()
> mkFunction "right" matLike = kleeneClosure      (fromAssociations matLike) `deepseq` return ()
> mkFunction _       matLike = kleeneClosureArray (fromAssociations matLike) `deepseq` return ()
        
> message :: String
> message = unlines ["Required arguments:", "\tsize :: Int", "\tdensity :: Double",
>                    "\trandomGenerator :: Int",
>                    "\ttype ::= t (tropical) | txb (pair of tropical and Boolean) | b (boolean)",
>                    "\tfunction ::= left | right | array"]

> main :: IO ()
> main =
>   do args <- getArgs
>      case args of
>        (s : d : r : ty : f : _) ->
>          let size = read s :: Int
>              dens = read d :: Double
>              gen  = mkStdGen (read r)
>          in case ty of
>               "t"   -> mkFunction f (randomSquareMatLike gen size dens (MinWeight, MaxWeight) :: MatLike (Tropical Int))
>               "txb" -> mkFunction f (randomSquareMatLike gen size dens ((MinWeight, False), (MaxWeight,True)) :: MatLike (Tropical Int, Bool))
>               _     -> mkFunction f (randomSquareMatLike gen size dens (True, True) :: MatLike Bool)
>        _ -> putStr message

              