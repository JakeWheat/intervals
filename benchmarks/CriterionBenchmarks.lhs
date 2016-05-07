
> {-# LANGUAGE ScopedTypeVariables,StandaloneDeriving,DeriveGeneric #-}
> import Criterion.Main
> import Intervals
> import ArbitraryIntervals
> -- import qualified Test.QuickCheck.Random as QR

> -- import Test.QuickCheck.Gen as QG
> --import Data.List (intercalate)

> import Control.DeepSeq(force,NFData(..))
> import Control.Exception(evaluate)
> import Control.DeepSeq.Generics(genericRnf)

> instance NFData Interval where
>     rnf = genericRnf
> instance NFData IntervalSet where
>     rnf = genericRnf

> main :: IO ()
> main = do
>     -- generate a repeatable set of arbitrary values using a seed
>     let x :: [IntervalSet]
>         x = unGen arbitrary (mkQCGen 10) 50 -- 500
>     x1 <- evaluate $ force x
>     defaultMain
>         [bgroup "pack-simple"
>          $ let mb a b = bench ("pack-simple-" ++ show a)
>                         $ nf packIntervalSet b
>            in zipWith mb [(0::Int)..] x1
>         ,bgroup "pack-v2"
>          $ let mb a b = bench ("pack-v2-" ++ show a)
>                         $ nf packIntervalSet' b
>            in zipWith mb [(0::Int)..] x1
>         ,bgroup "model-u_minus"
>          $ let mb a (b,c) = bench ("model-u_minus-" ++ show a)
>                           $ nf (modelUMinusIntervalSet b) c
>            in zipWith mb [(0::Int)..] [ (b,c) | b <- x1, c <- x1 ]
>         ,bgroup "u_minus-v2"
>          $ let mb a (b,c) = bench ("u_minus-v2-" ++ show a)
>                           $ nf (uMinusIntervalSetv2 b) c
>            in zipWith mb [(0::Int)..] [ (b,c) | b <- x1, c <- x1 ]
>         ,bgroup "u_minus-v3"
>          $ let mb a (b,c) = bench ("u_minus-v3-" ++ show a)
>                           $ nf (uMinusIntervalSetv3 b) c
>            in zipWith mb [(0::Int)..] [ (b,c) | b <- x1, c <- x1 ]
>         ,bgroup "u_minus-v4"
>          $ let mb a (b,c) = bench ("u_minus-v4-" ++ show a)
>                           $ nf (uMinusIntervalSetv4 b) c
>            in zipWith mb [(0::Int)..] [ (b,c) | b <- x1, c <- x1 ]
>         ,bgroup "u_minus-v5"
>          $ let mb a (b,c) = bench ("u_minus-v5-" ++ show a)
>                           $ nf (uMinusIntervalSetv5 b) c
>            in zipWith mb [(0::Int)..] [ (b,c) | b <- x1, c <- x1 ]
>         ]
