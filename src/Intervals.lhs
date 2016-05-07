
> {-# LANGUAGE DeriveGeneric #-}
> module Intervals
>        (Interval(..)
>        ,makeInterval
>        ,IntervalSet
>        ,makeIntervalSet
>        ,packIntervalSet
>        ,packIntervalSet'
>        ,unpackIntervalSet
>        ,modelUMinusIntervalSet
>        ,uMinusIntervalSet
>        ,uMinusIntervalSetv2
>        ,uMinusIntervalSetv3
>        ,uMinusIntervalSetv4
>        ,uMinusIntervalSetv5
>        ) where

> import Data.List (nub,sort,(\\))
> import GHC.Generics (Generic)

> data Interval = I Integer Integer
>                 deriving (Ord, Eq,Show,Generic)

what about representing unit intervals with a different ctor?

> makeInterval :: Integer -> Integer -> Either String Interval
> makeInterval b e =
>     if e < b
>     then Left $ "interval end is less than start: " ++ show (b,e)
>     else Right $ I b e

maybe change interval set to vector or something for speed?

> data IntervalSet = IS [Interval]
>                    deriving (Eq,Show,Generic)


does sort and unique:

> makeIntervalSet :: [Interval] -> IntervalSet
> makeIntervalSet is = IS $ sort $ nub is


> packIntervalSet :: IntervalSet -> IntervalSet
> packIntervalSet tis' =
>   let (IS tis) = unpackIntervalSet tis'
>   in makeIntervalSet $ combineIs tis
>   where
>     combineIs [] = []
>     combineIs i@[_] = i
>     combineIs ((I a b):is@((I c d):is1))
>         | b + 1 == c = combineIs ((I a d):is1)
>         | otherwise = (I a b):combineIs is

'optimised' version of pack which doesn't go via unpack

> packIntervalSet' :: IntervalSet -> IntervalSet
> packIntervalSet' (IS tis) =
>   makeIntervalSet $ combineIs tis
>   where
>     combineIs [] = []
>     combineIs i@[_] = i
>     combineIs ((I a b):is@((I c d):is1))
>         | merges a b c d = combineIs ((I (min a c) (max b d)):is1)
>         | otherwise = (I a b):combineIs is
>     -- todo: split this function out and test it
>     merges a b c d =
>         let m1 x y z = (x <= z && y >= z) || (y + 1 == z)
>         in m1 a b c || m1 c d a

> unpackIntervalSet :: IntervalSet -> IntervalSet
> unpackIntervalSet (IS is) =
>     makeIntervalSet $ concatMap unpackInterval is
>   where
>     unpackInterval (I a b) = [I c c | c <- [a..b]]

> modelUMinusIntervalSet :: IntervalSet -> IntervalSet -> IntervalSet
> modelUMinusIntervalSet is0 is1 =
>     let (IS iu0) = unpackIntervalSet is0
>         (IS iu1) = unpackIntervalSet is1
>         resu = iu0 \\ iu1
>     in packIntervalSet' $ makeIntervalSet resu


u_minus for interval sets using stream implementation, still uses
unpack but not as much (the current implementation uses unpack way
more than the original at the moment)

algo:

consider the head of the first set: i0
drop all the elements from i1s head which are strictly less than i0
take all the elements from the head of i1s which aren't strictly
greater than i0
unpack i0, subtract each of the matching i1s without unpacking them
then:
take another i0 and loop

> uMinusIntervalSetv2 :: IntervalSet -> IntervalSet -> IntervalSet
> uMinusIntervalSetv2 (IS is0) (IS is1) =
>     packIntervalSet' $ makeIntervalSet $ f is0 is1
>   where
>     f [] _ = []
>     f i0s [] = i0s
>     f (i0:i0s) i1s =
>         let keepI1s = dropWhile (`strictlyLessThan` i0) i1s
>             matchingI1s = takeWhile (not . flip strictlyGreaterThan i0) keepI1s
>         in subtractInterval i0 matchingI1s ++ f i0s keepI1s
>     strictlyLessThan (I _ b) (I c _) = b < c
>     strictlyGreaterThan (I a _) (I _ d) = d < a
>     subtractInterval :: Interval -> [Interval] -> [Interval]
>     subtractInterval (I a b) is =
>         let (IS a1) = unpackIntervalSet $ IS [I a b]
>             (IS a2) = unpackIntervalSet $ IS is
>         in a1 \\ a2

todo: subtract is very poor
fix 1: only unpack the first arg
fix 2: don't unpack the second arg


> uMinusIntervalSetv3 :: IntervalSet -> IntervalSet -> IntervalSet
> uMinusIntervalSetv3 (IS is0) is1' =
>     let IS is1 = packIntervalSet' is1'
>     in packIntervalSet' $ makeIntervalSet $ f is0 is1
>   where
>     f [] _ = []
>     f i0s [] = i0s
>     f (i0:i0s) i1s =
>         let keepI1s = dropWhile (`strictlyLessThan` i0) i1s
>             matchingI1s = takeWhile (not . flip strictlyGreaterThan i0) keepI1s
>         in subtractInterval i0 matchingI1s ++ f i0s keepI1s
>     strictlyLessThan (I _ b) (I c _) = b < c
>     strictlyGreaterThan (I a _) (I _ d) = d < a
>     subtractInterval :: Interval -> [Interval] -> [Interval]
>     subtractInterval (I a b) is =
>         let (IS a1) = unpackIntervalSet $ IS [I a b]
>             (IS a2) = unpackIntervalSet $ IS is
>         in a1 \\ a2

> uMinusIntervalSetv4 :: IntervalSet -> IntervalSet -> IntervalSet
> uMinusIntervalSetv4 is0' is1' =
>     let IS is0 = packIntervalSet' is0'
>         IS is1 = packIntervalSet' is1'
>     in packIntervalSet' $ makeIntervalSet $ f is0 is1
>   where
>     f [] _ = []
>     f i0s [] = i0s
>     f (i0:i0s) i1s =
>         let keepI1s = dropWhile (`strictlyLessThan` i0) i1s
>             matchingI1s = takeWhile (not . flip strictlyGreaterThan i0) keepI1s
>         in subtractInterval i0 matchingI1s ++ f i0s keepI1s
>     strictlyLessThan (I _ b) (I c _) = b < c
>     strictlyGreaterThan (I a _) (I _ d) = d < a
>     subtractInterval :: Interval -> [Interval] -> [Interval]
>     subtractInterval (I a b) is =
>         let (IS a1) = unpackIntervalSet $ IS [I a b]
>             (IS a2) = unpackIntervalSet $ IS is
>         in a1 \\ a2

new algorithm still using packing and unpacking:

> uMinusIntervalSetv5 :: IntervalSet -> IntervalSet -> IntervalSet
> uMinusIntervalSetv5 is0' is1' =
>     let IS is0 = packIntervalSet' is0'
>         IS is1 = packIntervalSet' is1'
>         is1Unpacked = flip map is1
>                       $ \x -> (x,unpackInterval x)
>     in packIntervalSet' $ makeIntervalSet $ f is0 is1Unpacked
>   where
>     unpackInterval :: Interval -> [Interval]
>     unpackInterval (I a b) = map (\i -> I i i) [a..b]
>     f [] _ = []
>     f i0s [] = i0s
>     f (i0:i0s) i1s =
>         let keepI1s = dropWhile ((`strictlyLessThan` i0) . fst) i1s
>             matchingI1s = takeWhile (not . flip strictlyGreaterThan i0 . fst) keepI1s
>         in subtractInterval i0 matchingI1s ++ f i0s keepI1s
>     strictlyLessThan (I _ b) (I c _) = b < c
>     strictlyGreaterThan (I a _) (I _ d) = d < a
>     subtractInterval :: Interval -> [(Interval,[Interval])] -> [Interval]
>     subtractInterval (I a b) is =
>         let (IS a1) = unpackIntervalSet $ IS [I a b]
>         in a1 \\ concatMap snd is




> {-uMinusIntervalSetv3 :: IntervalSet -> IntervalSet -> IntervalSet
> uMinusIntervalSetv3 (IS is0) (IS is1) =
>     packIntervalSet' $ makeIntervalSet $ f is0 is1
>   where
>     f [] _ = []
>     f i0s [] = i0s
>     f (i0:i0s) i1s =
>         let keepI1s = dropWhile (`strictlyLessThan` i0) i1s
>             matchingI1s = takeWhile (not . flip strictlyGreaterThan i0) keepI1s
>         in subtractInterval i0 matchingI1s ++ f i0s keepI1s
>     strictlyLessThan (I _ b) (I c _) = b < c
>     strictlyGreaterThan (I a _) (I _ d) = d < a
>     subtractInterval :: Interval -> [Interval] -> [Interval]
>     subtractInterval (I a b) [] = [I a b]
>     subtractInterval i@(I a b) ((I c d):is)
>         | d < a || b < c = subtractInterval i is
>          -- overlaps completely
>         | c <= a &&  d >= b -> []
>          -- overlaps start
>         | c <= a &&  d >= b -> []
>          -- overlaps end
>          -- overlaps middle
>     
>     subtractInterval (I a b) is =
>         let (IS a1) = unpackIntervalSet $ IS [I a b]
>             (IS a2) = unpackIntervalSet $ IS is
>         in a1 \\ a2 -}


> uMinusIntervalSet :: IntervalSet -> IntervalSet -> IntervalSet
> uMinusIntervalSet = uMinusIntervalSetv2

now: packRelation, unpackRelation, modelUMinusRelation, uMinusRelation
these are the above with a payload for each entry

create separate modules, with model and optimised implementations


then:
self times using slow join

do a few example checks
see if can do the generator test


self times using streaming implementation
can test against the generator
and against the model implementation

use criterion to check optimisations
implement the algorithm in postgres and compare times



some optimisation ideas:

use a variation of vector or list of vectors to represent interval
  sets
benchmark running pack on already packed
benchmark running isPacked function
make sure we don't repeat packing
get ghc profiling and code coverage working

