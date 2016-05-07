
> {-# LANGUAGE DeriveGeneric,MultiWayIf #-}
> module Intervals
>        (-- * basic intervals
>         Interval(..)
>        ,makeInterval
>         -- * simple sets of intervals
>        ,IntervalSet
>        ,makeIntervalSet
>        ,isLength
>         -- * packing and unpacking sets of intervals
>        ,packIntervalSetModel
>        ,packIntervalSetv2
>        ,packIntervalSet
>        ,unpackIntervalSet
>        ,intervalSetsEquivalent
>         -- * u_minus on interval sets
>        ,uminusIntervalSetModel
>        ,uminusIntervalSet
>        ,uminusIntervalSetv2
>        ,uminusIntervalSetv3
>        ,uminusIntervalSetv4
>        ,uminusIntervalSetv5
>        ,uminusIntervalSetv6
>         -- * relations
>        ,Relation
>        ,makeRelation
>        ,relationCount
>         -- * packing and unpacking on relations
>        ,unpackRelation
>        ,packRelationv2
>        ,packRelation
>        ,relationsEquivalent
>         -- * u_minus for relations
>        -- ,uminusModel
>        ,uminusv2
>        ,uminus
>        ) where

> import Data.List (nub,sort,(\\)) --, sortBy)
> --import Data.Ord (comparing)
> import GHC.Generics (Generic)
> import Control.Arrow (second)

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

> isLength :: IntervalSet -> Int
> isLength (IS is) = length is

does sort and unique:

> makeIntervalSet :: [Interval] -> IntervalSet
> makeIntervalSet is = IS $ sort $ nub is


> packIntervalSetModel :: IntervalSet -> IntervalSet
> packIntervalSetModel tis' =
>     let (IS tis) = unpackIntervalSet tis'
>     in makeIntervalSet $ combineIs tis
>   where
>     combineIs [] = []
>     combineIs i@[_] = i
>     combineIs (i@(I a b):is@((I c d):is1))
>         | b + 1 == c = combineIs ((I a d):is1)
>         | otherwise = i:combineIs is

'optimised' version of pack which doesn't go via unpack

> packIntervalSetv2 :: IntervalSet -> IntervalSet
> packIntervalSetv2 (IS tis) =
>     makeIntervalSet $ combineIs tis
>   where
>     combineIs [] = []
>     combineIs i@[_] = i
>     combineIs (i@(I a b):is@((I c d):is1))
>         | merges a b c d = combineIs ((I (min a c) (max b d)):is1)
>         | otherwise = i:combineIs is
>     -- todo: split this function out and test it
>     merges a b c _d =
>         let m1 x y z = (x <= z && y >= z) || (y + 1 == z)
>         in m1 a b c --  || m1 c d a


> packIntervalSet :: IntervalSet -> IntervalSet
> packIntervalSet = packIntervalSetv2

todo: some trivial tests for intervalSetsEquivalent

> intervalSetsEquivalent :: IntervalSet -> IntervalSet -> Bool
> intervalSetsEquivalent a b = packIntervalSet a == packIntervalSet b

> unpackIntervalSet :: IntervalSet -> IntervalSet
> unpackIntervalSet (IS is) =
>     makeIntervalSet $ concatMap unpackInterval is
>   where
>     unpackInterval (I a b) = [I c c | c <- [a..b]]

> uminusIntervalSetModel :: IntervalSet -> IntervalSet -> IntervalSet
> uminusIntervalSetModel is0 is1 =
>     let (IS iu0) = unpackIntervalSet is0
>         (IS iu1) = unpackIntervalSet is1
>         resu = iu0 \\ iu1
>     in packIntervalSet $ makeIntervalSet resu



series of u_minus implementations. Not that interesting. The v6 below
is _almost_ the starting point of a naive implementation.

algo:

consider the head of the first set: i0
drop all the elements from i1s head which are strictly less than i0
take all the elements from the head of i1s which aren't strictly
greater than i0
unpack i0, subtract each of the matching i1s without unpacking them
then:
take another i0 and loop

> uminusIntervalSetv2 :: IntervalSet -> IntervalSet -> IntervalSet
> uminusIntervalSetv2 (IS is0) (IS is1) =
>     packIntervalSet $ makeIntervalSet $ f is0 is1
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


> uminusIntervalSetv3 :: IntervalSet -> IntervalSet -> IntervalSet
> uminusIntervalSetv3 (IS is0) is1' =
>     let IS is1 = packIntervalSet is1'
>     in packIntervalSet $ makeIntervalSet $ f is0 is1
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

> uminusIntervalSetv4 :: IntervalSet -> IntervalSet -> IntervalSet
> uminusIntervalSetv4 is0' is1' =
>     let IS is0 = packIntervalSet is0'
>         IS is1 = packIntervalSet is1'
>     in packIntervalSet $ makeIntervalSet $ f is0 is1
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

> uminusIntervalSetv5 :: IntervalSet -> IntervalSet -> IntervalSet
> uminusIntervalSetv5 is0' is1' =
>     let IS is0 = packIntervalSet is0'
>         IS is1 = packIntervalSet is1'
>         is1Unpacked = flip map is1
>                       $ \x -> (x,unpackInterval x)
>     in packIntervalSet $ makeIntervalSet $ f is0 is1Unpacked
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


finally without unpacking

> uminusIntervalSetv6 :: IntervalSet -> IntervalSet -> IntervalSet
> uminusIntervalSetv6 is0' is1' =
>     let IS is0 = packIntervalSet is0'
>         IS is1 = packIntervalSet is1'
>     in packIntervalSet $ makeIntervalSet $ f is0 is1
>   where
>     f [] _ = []
>     f i0s [] = i0s
>     f (i0:i0s) i1s =
>         -- this uses a kind of sort merge approach
>         -- maybe it can be optimised better?
>         -- because the intervals are packed,
>         -- I think we never need to look at a interval twice
>         -- which we do here because of keepI1s and matchingI1s
>         let keepI1s = dropWhile (`strictlyLessThan` i0) i1s
>             matchingI1s = takeWhile (not . flip strictlyGreaterThan i0) keepI1s
>         in subtractInterval i0 matchingI1s ++ f i0s keepI1s
>     strictlyLessThan (I _ b) (I c _) = b < c
>     strictlyGreaterThan (I a _) (I _ d) = d < a
>     -- this uses the fact that the second list of intervals is
>     -- packed and sorted
>     subtractInterval :: Interval -> [Interval] -> [Interval]
>     subtractInterval i [] = [i]
>     subtractInterval (I a b) ((I c d):is) =
>         -- check for 2 impossible conditions first
>         -- can remove these when working and trust the testing enough
>         if -- interval 2 is completely before interval 1
>            {-  | d < a -> subtractInterval i is
>            -- interval 2 is completely after interval 1
>            | b < c -> [i] -}
>              -- interval 2 covers interval one completely
>            | c <= a && d >= b -> []
>              -- interval 2 overlaps the start of interval one
>            | c <= a && d < b-> subtractInterval (I (d + 1) b) is
>              -- interval 2 overlaps the end of interval one
>            | c > a && d >= b -> [I a (c - 1)]
>              -- interval 2 appears in the middle of interval one
>            | a < c && b > d -> I a (c - 1)
>                                : subtractInterval (I (d + 1) b) is

Next steps for this function are in the todo. want to eliminate the
double passing in f, and then optimise the memory layouts of the
data. The core algorithmic idea is probably good, possibly a hash
based instead of sorting could work?

> uminusIntervalSet :: IntervalSet -> IntervalSet -> IntervalSet
> uminusIntervalSet = uminusIntervalSetv6

------------------------------

not really a relation, but an interval set with a payload per row

it's sorted on the intervals and doesn't support duplicate entries

> data Relation a = Relation [(a,IntervalSet)]
>                   deriving (Eq,Show,Generic)

> makeRelation :: Eq a => [(Interval,a)] -> Relation a
> makeRelation es =
>     let es' = partitionEntries es
>     in Relation $ map (second makeIntervalSet) es'

> partitionEntries :: Eq a => [(Interval,a)] -> [(a,[Interval])]
> partitionEntries = undefined

> relationCount :: Relation a -> Int
> relationCount (Relation es) = sum $ map (isLength . snd) es


TODO: code below is all wrong. Start by partitioning by the payload
values, then just delegate to the set versions. Might be able to do a
optimised unpack (but this operation is only really needed for
testing).

> unpackRelation :: Eq a => Relation a -> Relation a
> unpackRelation (Relation es) =
>     Relation $ map (second unpackIntervalSet) es


> packRelationv2 :: Eq a => Relation a -> Relation a
> packRelationv2 (Relation es) =
>     Relation $ map (second packIntervalSet) es


> packRelation :: Eq a => Relation a -> Relation a
> packRelation = packRelationv2

> relationsEquivalent :: Eq a => Relation a -> Relation a -> Bool
> relationsEquivalent a b = packRelation a == packRelation b

> uminusv2 :: Eq a => Relation a -> Relation a -> Relation a
> uminusv2 (Relation r0) (Relation r1) =
>     -- for each value in r0
>     -- find the corresponding value in r1
>     -- and subtract
>     -- if none found, then this op is id
>     -- this process can probably be optimised easily if there
>     -- are a lot of distinct values
>     Relation $ map (flip minusMatching r1) r0
>   where
>     minusMatching (a,es0) [] = (a,es0)
>     minusMatching (a,es0) ((b,es1):_)
>       | a == b  = (a,uminusIntervalSet es0 es1)
>     minusMatching e0 (_:es1) = minusMatching e0 es1

> uminus :: Eq a => Relation a -> Relation a -> Relation a
> uminus = uminusv2
