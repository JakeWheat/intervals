
> module Main where

> import Intervals
> import ArbitraryIntervals ()
> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as H
> import qualified Test.Tasty.QuickCheck as Q

> data Test = Group String [Test]
>           | IntervalExample Integer Integer (Maybe Interval)
>             -- checks the two interval sets are the same
>             -- after pack and unpack
>           | IntervalSetEquivalentExample [Interval] [Interval]
>           | IntervalSetPackExample [Interval] [Interval]
>           | IntervalSetUnpackExample [Interval] [Interval]
>           | UMinusISExample [Interval] [Interval] [Interval]


> intervalExamples :: Test
> intervalExamples = Group "interval-examples"
>     [IntervalExample 1 1 $ Just $ I 1 1
>     ,IntervalExample 1 2 $ Just $ I 1 2
>     ,IntervalExample (-1) 2 $ Just $ I (-1) 2
>     ,IntervalExample 1 0 Nothing
>     ]

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

TODO:
check an interval list, and then same list in random order are the
same after makeintervalset

some quickchecking:
generate some intervalsets
check that pack . unpack = unpack
   unpack . pack = pack
   unpack . unpack = unpack
   pack . pack = pack

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
>     \is -> (unpackIntervalSet . pack) is
>            == unpackIntervalSet is

> intervalSetsUnpackPackEqPack :: (IntervalSet -> IntervalSet) -> T.TestTree
> intervalSetsUnpackPackEqPack pack =
>     Q.testProperty  "pack . unpack = pack" $
>     \is -> (pack . unpackIntervalSet) is
>            == pack is

> intervalSetsUnpackUnpackEqUnpack :: T.TestTree
> intervalSetsUnpackUnpackEqUnpack =
>     Q.testProperty  "unpack . unpack = unpack" $
>     \is -> (unpackIntervalSet . unpackIntervalSet) is
>            == unpackIntervalSet is

> intervalSetsPackPackEqPack :: (IntervalSet -> IntervalSet) -> T.TestTree
> intervalSetsPackPackEqPack pack =
>     Q.testProperty  "pack . pack = pack" $
>     \is -> (pack . pack) is == pack is

> uMinusISExamples :: Test
> uMinusISExamples = Group "UMinusISExamples"
>     [UMinusISExample [] [] []
>     ,UMinusISExample [] [I 1 1] []
>     ,UMinusISExample [I 1 1] [I 1 1] []
>     ,UMinusISExample [I 2 2] [I 1 1] [I 2 2]
>     ,UMinusISExample [I 1 2] [I 1 1] [I 2 2]
>     ,UMinusISExample [I 1 3] [I 2 2] [I 1 1, I 3 3]
>     ]

quickcheck this against the model

> minusAEqMinusB :: String
>                -> (IntervalSet -> IntervalSet -> IntervalSet)
>                -> (IntervalSet -> IntervalSet -> IntervalSet)
>                -> T.TestTree
> minusAEqMinusB nm f0 f1 =
>     Q.testProperty nm $ \i0 i1 -> (i0 `f0` i1) == (i0 `f1` i1)

> uMinusv2EqModelUMinus :: T.TestTree
> uMinusv2EqModelUMinus =
>     minusAEqMinusB "u_minus v2 == model u_minus"
>         modelUMinusIntervalSet uMinusIntervalSetv2

> uMinusv3EqModelUMinus :: T.TestTree
> uMinusv3EqModelUMinus =
>     minusAEqMinusB "u_minus v3 == model u_minus"
>         modelUMinusIntervalSet uMinusIntervalSetv3

> uMinusv4EqModelUMinus :: T.TestTree
> uMinusv4EqModelUMinus =
>     minusAEqMinusB "u_minus v4 == model u_minus"
>         modelUMinusIntervalSet uMinusIntervalSetv4

> uMinusv5EqModelUMinus :: T.TestTree
> uMinusv5EqModelUMinus =
>     minusAEqMinusB "u_minus v5 == model u_minus"
>         modelUMinusIntervalSet uMinusIntervalSetv5

> uMinusv6EqModelUMinus :: T.TestTree
> uMinusv6EqModelUMinus =
>     minusAEqMinusB "u_minus v6 == model u_minus"
>         modelUMinusIntervalSet uMinusIntervalSetv6




---------------------------------------

testing boilerplate


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
>                             ,intervalSetsPackAndUnpack
>                             ,uMinusv2EqModelUMinus
>                             ,uMinusv3EqModelUMinus
>                             ,uMinusv4EqModelUMinus
>                             ,uMinusv5EqModelUMinus
>                             ,uMinusv6EqModelUMinus])

