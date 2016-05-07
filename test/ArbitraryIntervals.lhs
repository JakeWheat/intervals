
> module ArbitraryIntervals
>        ({-Q.generate
>        ,Q.sample'
>        ,Q.sample
>        ,Q.arbitrary
>        ,-}QG.unGen
>        ,Q.arbitrary
>        ,QR.mkQCGen
>        ) where

> import Test.QuickCheck.Gen as QG
> import qualified Test.Tasty.QuickCheck as Q
> import Intervals
> import qualified Test.QuickCheck.Random as QR

> instance Q.Arbitrary Interval where
>   arbitrary = do
>     x <- Q.arbitrary
>     y <- Q.arbitrary
>     return $ if x > y
>              then I y x
>              else I x y

> instance Q.Arbitrary IntervalSet where
>   arbitrary = do
>     l <- Q.arbitrary
>     return $ makeIntervalSet l

