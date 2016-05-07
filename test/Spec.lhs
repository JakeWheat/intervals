


plan:
start with interval type
then add sets of intervals
then add pack and unpack for these (for a single column)
then do pack and unpack with a payload
then implement u_minus via pack and unpack
then implement faster u_minus using sort without unpack
see if this can be used in sql
ultimately, want to process all the layered events
maybe start with unpack implementation
then move to a sort/streaming implementation

> import Data.List (nub,sort,(\\))


> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as H
> import qualified Test.Tasty.QuickCheck as Q


> data Interval = I Integer Integer
>                 deriving (Ord, Eq,Show)

> --instance Ord Interval where
> --  (I a b) `compare` (I c d) = (a,b) `compare` (c,d)

what about representing unit intervals with a different ctor?

> makeInterval :: Integer -> Integer -> Either String Interval
> makeInterval b e =
>     if e < b
>     then Left $ "interval end is less than start: " ++ show (b,e)
>     else Right $ I b e

tests:
a few basic examples

> intervalExamples :: Test
> intervalExamples = Group "interval-examples"
>     [IntervalExample 1 1 $ Just $ I 1 1
>     ,IntervalExample 1 2 $ Just $ I 1 2
>     ,IntervalExample (-1) 2 $ Just $ I (-1) 2
>     ,IntervalExample 1 0 Nothing
>     ]

> data Test = Group String [Test]
>           | IntervalExample Integer Integer (Maybe Interval)
>             -- checks the two interval sets are the same
>             -- after pack and unpack
>           | IntervalSetEquivalentExample [Interval] [Interval]
>           | IntervalSetPackExample [Interval] [Interval]
>           | IntervalSetUnpackExample [Interval] [Interval]
>           | UMinusISExample [Interval] [Interval] [Interval]

maybe change interval set to vector or something for speed?

> data IntervalSet = IS [Interval]
>                    deriving (Eq,Show)


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

todo:
a few basic examples

> intervalSetExamples :: Test
> intervalSetExamples = Group "intervalSet-examples"
>     [IntervalSetPackExample [] []
>     ,IntervalSetPackExample [I 1 1] [I 1 1]
>     ,IntervalSetPackExample [I 1 1, I 3 3] [I 1 1, I 3 3]
>     ,IntervalSetPackExample [I 1 1, I 2 3] [I 1 3]

>     ,IntervalSetUnpackExample [] []
>     ,IntervalSetUnpackExample [I 1 1] [I 1 1]
>     ,IntervalSetUnpackExample [I 1 2,I 3 4] [I 1 1, I 2 2, I 3 3, I 4 4]

>     ,IntervalSetEquivalentExample [] []
>     ,IntervalSetEquivalentExample [I 1 1] [I 1 1]
>     ,IntervalSetEquivalentExample [I 1 2] [I 1 1, I 1 2]
>     ,IntervalSetEquivalentExample [I 1 3] [I 1 2, I 2 3]

>     ,IntervalSetEquivalentExample [I 1 2, I 5 6] [I 5 6, I 1 2]

>     ]


check an interval list, and then same list in random order are the
same after makeintervalset

some quickchecking:
generate some intervalsets
check that pack . unpack = unpack
   unpack . pack = pack
   unpack . unpack = unpack
   pack . pack = pack

> instance Q.Arbitrary Interval where
>   arbitrary = do
>     x <- Q.arbitrary
>     y <- Q.arbitrary
>     return $ if x > y
>              then I y x
>              else I x y


> intervalSetsPackAndUnpack :: T.TestTree
> intervalSetsPackAndUnpack = T.testGroup "intervalSetsPackAndUnpack"
>     [intervalSetsPackUnpackEqUnpack packIntervalSet
>     ,intervalSetsUnpackPackEqPack packIntervalSet
>     ,intervalSetsUnpackUnpackEqUnpack
>     ,intervalSetsPackPackEqPack packIntervalSet
>     ,intervalSetsPackUnpackEqUnpack packIntervalSet'
>     ,intervalSetsUnpackPackEqPack packIntervalSet'
>     ,intervalSetsPackPackEqPack packIntervalSet'
>     ]

> intervalSetsPackUnpackEqUnpack :: (IntervalSet -> IntervalSet) -> T.TestTree
> intervalSetsPackUnpackEqUnpack pack =
>     Q.testProperty  "unpack . pack = unpack" $
>     \l -> let is = makeIntervalSet l
>           in (unpackIntervalSet . pack) is
>               == unpackIntervalSet is

> intervalSetsUnpackPackEqPack :: (IntervalSet -> IntervalSet) -> T.TestTree
> intervalSetsUnpackPackEqPack pack =
>     Q.testProperty  "pack . unpack = pack" $
>     \l -> let is = makeIntervalSet l
>           in (pack . unpackIntervalSet) is
>               == pack is

> intervalSetsUnpackUnpackEqUnpack :: T.TestTree
> intervalSetsUnpackUnpackEqUnpack =
>     Q.testProperty  "unpack . unpack = unpack" $
>     \l -> let is = makeIntervalSet l
>           in (unpackIntervalSet . unpackIntervalSet) is
>               == unpackIntervalSet is

> intervalSetsPackPackEqPack :: (IntervalSet -> IntervalSet) -> T.TestTree
> intervalSetsPackPackEqPack pack =
>     Q.testProperty  "pack . pack = pack" $
>     \l -> let is = makeIntervalSet l
>           in (pack . pack) is
>               == pack is



u_minus for interval sets using unpack

is0 u_minus is0

> modelUMinusIntervalSet :: IntervalSet -> IntervalSet -> IntervalSet
> modelUMinusIntervalSet is0 is1 =
>     let (IS iu0) = unpackIntervalSet is0
>         (IS iu1) = unpackIntervalSet is1
>         resu = iu0 \\ iu1
>     in packIntervalSet' $ makeIntervalSet resu

> uMinusISExamples :: Test
> uMinusISExamples = Group "UMinusISExamples"
>     [UMinusISExample [] [] []
>     ,UMinusISExample [] [I 1 1] []
>     ,UMinusISExample [I 1 1] [I 1 1] []
>     ,UMinusISExample [I 2 2] [I 1 1] [I 2 2]
>     ,UMinusISExample [I 1 2] [I 1 1] [I 2 2]
>     ,UMinusISExample [I 1 3] [I 2 2] [I 1 1, I 3 3]
>     ]

u_minus for interval sets using stream implementation


> {-uMinusIntervalSet :: IntervalSet -> IntervalSet -> IntervalSet
> uMinusIntervalSet (IS is0) (IS is1) =
>   packIntervalSet'
>   where
>     in packIntervalSet $ makeIntervalSet resu-}


quickcheck this against the model


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



> makeTasty :: Test -> T.TestTree
> makeTasty (Group nm ts) =  T.testGroup nm $ map makeTasty ts

> makeTasty (IntervalExample b e r) =
>     H.testCase ("interval-example: " ++ show (b,e,r)) $ do
>     case makeInterval b e of
>         Right r' -> H.assertEqual "" r (Just r')
>         g@(Left {}) -> case r of
>                        Nothing -> return ()
>                        Just i -> H.assertEqual "" (Right i) g


> makeTasty (IntervalSetPackExample i0 i1) =
>   H.testCase "IntervalSetPackExample" $
>   H.assertEqual "" (makeIntervalSet i1) $ packIntervalSet $ makeIntervalSet i0

> makeTasty (IntervalSetUnpackExample i0 i1) =
>   H.testCase "IntervalSetUnpackExample" $
>   H.assertEqual "" (makeIntervalSet i1) $ unpackIntervalSet $ makeIntervalSet i0

> makeTasty (IntervalSetEquivalentExample i0' i1') =
>     -- todo: add names
>     H.testCase "intervalSet-example" $ do
>     let i0 = makeIntervalSet i0'
>         i1 = makeIntervalSet i1'
>         ip0 = packIntervalSet i0
>         ip1 = packIntervalSet i1
>         iu0 = unpackIntervalSet i0
>         iu1 = unpackIntervalSet i1
>     H.assertEqual "" ip0 ip1
>     H.assertEqual "" iu0 iu1

> makeTasty (UMinusISExample a b c) =
>     H.testCase "uMinusISExample" $ do
>     H.assertEqual "" (makeIntervalSet c)
>         (modelUMinusIntervalSet (makeIntervalSet a)
>                                 (makeIntervalSet b))



> allTests :: Test
> allTests = Group "hunit tests"
>     [intervalExamples
>     ,intervalSetExamples
>     ,uMinusISExamples]

> main :: IO ()
> main = T.defaultMain $
>        (T.testGroup "tests" [makeTasty allTests
>                             ,intervalSetsPackAndUnpack])

