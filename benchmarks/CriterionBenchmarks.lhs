
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
>         [bgroup "bench"
>             [bgroup "pack-simple"
>              $ let mb a b = bench ("pack-simple-" ++ show a)
>                             $ nf packIntervalSet b
>                in zipWith mb [(0::Int)..] x1
>             ,bgroup "pack-v2"
>              $ let mb a b = bench ("pack-v2-" ++ show a)
>                             $ nf packIntervalSet' b
>                in zipWith mb [(0::Int)..] x1
>             ]
>         ]
