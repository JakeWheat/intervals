


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

> import Data.List (nub,sort)


> import qualified Test.Tasty as T


> data Interval = I Integer Integer
>                 deriving (Eq,Show,Ord)

> makeInterval :: Integer -> Integer -> Either String Interval
> makeInterval b e =
>     if e < b
>     then Left $ "interval end is less than start: " ++ show (b,e)
>     else Right $ I b e

tests:
a few basic examples

> intervalExamples :: [(Integer,Integer, Maybe Interval)]
> intervalExamples = [(1,1, Just $ I 1 1)
>                    ,(1,2, Just $ I 1 2)
>                    ,(-1,2, Just $ I (-1) 2)
>                    ,(1,0, Nothing)]

maybe change interval set to vector or something for speed?

> data IntervalSet = IS [Interval]
>                    deriving (Eq,Show)


does sort and unique:

> makeIntervalSet :: [Interval] -> IntervalSet
> makeIntervalSet is = IS $ nub $ sort is

> packIntervalSet :: IntervalSet -> IntervalSet
> packIntervalSet = undefined

> unpackIntervalSet :: IntervalSet -> IntervalSet
> unpackIntervalSet = undefined

todo:
a few basic examples
some quickchecking


u_minus for interval sets using unpack

> modelUMinusIntervalSet :: IntervalSet -> IntervalSet -> IntervalSet
> modelUMinusIntervalSet = undefined

do some example tests


u_minus for interval sets using stream implementation

quickcheck this against the model


now: packRelation, unpackRelation, modelUMinusRelation, uMinusRelation
these are the above with a payload for each entry


then:
self times using slow join

do a few example checks
see if can do the generator test


self times using streaming implementation
can test against the generator
and against the model implementation




> main :: IO ()
> main = T.defaultMain $ T.testGroup "tests" []

