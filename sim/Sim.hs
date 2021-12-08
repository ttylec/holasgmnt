{-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module Sim where

import Data.Random
import Data.Random.Distribution.Exponential (exponential)
import Data.Random.Distribution.Beta (beta)
import Data.Sequence ( Seq (Empty), (<|), ViewR ((:>), EmptyR) )
import qualified Data.Sequence as Seq
import Data.List (sort, unfoldr, foldl')
import Data.List (sortBy)
import Data.Ord (comparing)


data CustomerType = Yellow | Red | Blue deriving (Show)

newtype Arriving = Arriving (CustomerType, Double)

data Customer = Customer {
    arriving :: Double
  , processing :: Double
  } deriving Show

-- processingTime :: CustomerType -> RVar Double
-- processingTime Yellow = (\x -> x*200*0.03333) <$> beta 2 5
-- processingTime Red = (\x -> x*200*0.16) <$> beta 2 2
-- processingTime Blue = (\x -> x*200*0.2) <$> beta 5 1

processingTime :: CustomerType -> RVar Double
processingTime Yellow = (*200) <$> beta 2 5
processingTime Red = (*200) <$> beta 2 2
processingTime Blue = (*200) <$> beta 5 1

-- processingTime :: CustomerType -> RVar Double
-- processingTime Yellow = betaF 2 5 <$> uniform 0 1
-- processingTime Red = betaF 2 2 <$> uniform 0 1
-- processingTime Blue = betaF 5 1 <$> uniform 0 1

betaF :: Floating a => a -> a -> a -> a
betaF alpha beta x = 200 * x**(alpha-1) * (1-x)**(beta-1)

customerStream :: CustomerType -> Double -> RVar [Customer]
customerStream c end = loop 0
  where
    loop t0
      | t0 > end = pure []
      | otherwise = do
          dt <- exponential 100
          p <- processingTime c
          let t = t0+dt
          rest <- loop t
          pure $ (Customer {arriving=t,processing=p}):rest


data Event a = Event { eventTime :: Double, event :: a } deriving (Eq, Show)

instance Eq a => Ord (Event a) where
  compare = comparing eventTime

waitingTime :: Customer -> Double -> Double
waitingTime Customer{arriving} started = started - arriving

served :: (Customer -> Double -> a) -> [Customer] -> [Event a]
served f cs = unfoldr go (0, cs)
  where
    go (_, []) = Nothing
    go (last, c:cs) =
      let event =  Event finished (f c started)
          started = max last (arriving c)
          finished = started + processing c
      in Just (event, (finished, cs))

data EventType = NewCustomer Customer | ServedCustomer Customer Double deriving (Show)


allEvents :: [Customer] -> [Event EventType]
allEvents cs = allEvents' (arrivals cs) (served ServedCustomer cs)
  where
    arrivals = fmap (\c -> Event (arriving c) (NewCustomer c))
    allEvents' :: [Event EventType] -> [Event EventType] -> [Event EventType]
    allEvents' [] ss = ss
    allEvents' as [] = as
    allEvents' (a:as) (s:ss)
      | eventTime a < eventTime s = a:allEvents' as (s:ss)
      | otherwise = s:allEvents' (a:as) ss


queueAtArrival :: [Event EventType] -> [Int]
queueAtArrival = snd . foldl' go (0, []) . fmap event
  where
    go (l, ls) (NewCustomer _) = (l+1, l:ls)
    go (l, ls) (ServedCustomer _ _ ) = (l-1, ls)