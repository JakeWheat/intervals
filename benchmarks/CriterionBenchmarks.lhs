
> {-# LANGUAGE ScopedTypeVariables,StandaloneDeriving,DeriveGeneric #-}
> import Criterion.Main
> import Intervals
> import ArbitraryIntervals

> import Data.List (intercalate)

> import Control.DeepSeq(force,NFData(..))
> import Control.Exception(evaluate)
> import Control.DeepSeq.Generics(genericRnf)

> import Test.QuickCheck.Gen (resize)

> instance NFData Interval where
>     rnf = genericRnf
> instance NFData IntervalSet where
>     rnf = genericRnf

> runMany :: [(a -> b)] -> [a] -> [b]
> runMany fs as =
>     zipWith ($) fs as


> doUMinusTests :: NFData c =>
>                  String
>               -> (a -> b -> c)
>               -> [(a, b)]
>               -> Benchmark
> doUMinusTests nm f ps = bench nm $
>     let (as,bs) = unzip ps
>         fs = zipWith ($) (repeat f) as
>     in nf (runMany fs) bs

> doPack :: NFData a =>
>           String
>        -> (b -> a)
>        -> [b]
>        -> Benchmark
> doPack nm f as = bench nm $ nf (runMany (repeat f)) as

> main :: IO ()
> main = do
>     -- generate a repeatable set of arbitrary values using a seed
>     let x :: [IntervalSet]
>         x = unGen arbitrary (mkQCGen 0) 52
>     x1 <- evaluate $ force x
>     putStrLn $ "number of example datas: " ++ show (length x1)
>              ++ "(" ++ intercalate "," (map (show . isLength) x1) ++ ")"
>     let isInteresting a b = not $ intervalSetsEquivalent a
>                             (a `uMinusIntervalSetv6` b)
>         x1Pairs = filter (uncurry isInteresting)
>                   [(a,b) | a <- x1, b <- x1]
>     putStrLn $ "number of interesting data pairs: " ++ show (length x1Pairs)
>     defaultMain $
>         [doPack "pack-model" packIntervalSetModel x1
>         ,doPack "pack-v2" packIntervalSetv2 x1
>         ,doUMinusTests "model-u_minus" modelUMinusIntervalSet x1Pairs
>         --,doUMinusTests "u_minus-v2" uMinusIntervalSetv2 x1Pairs
>         ,doUMinusTests "u_minus-v3" uMinusIntervalSetv3 x1Pairs
>         ,doUMinusTests "u_minus-v4" uMinusIntervalSetv4 x1Pairs
>         ,doUMinusTests "u_minus-v5" uMinusIntervalSetv5 x1Pairs
>         ,doUMinusTests "u_minus-v6" uMinusIntervalSetv6 x1Pairs
>         ]
