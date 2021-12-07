-- {-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module Sim where

import Data.Random
import Data.Random.Distribution.Exponential (exponential)
import Data.Random.Distribution.Beta (beta)
import Data.Sequence ( Seq (Empty), (<|), ViewR ((:>), EmptyR) )
import qualified Data.Sequence as Seq


data CustomerType = Yellow | Red | Blue deriving (Show)

newtype Arriving = Arriving (CustomerType, Double)

data Customer = Customer {
    arriving :: Double
  , processing :: Double
  -- , ctype :: CustomerType
  } deriving Show

processingTime :: CustomerType -> RVar Double
processingTime Yellow = beta 2 5
processingTime Red = beta 2 2
processingTime Blue = beta 5 1

customerStream :: CustomerType -> Double -> RVar [Customer]
customerStream c end = loop 0
  where
    loop t0
      | t0 > end = pure []
      | otherwise = do
          dt <- exponential 200
          p <- processingTime c
          let t = t0+dt
          rest <- loop t
          pure $ (Customer {arriving=t,processing=p}):rest


newtype CustomerQueue = CustomerQueue (Seq Customer, Double) deriving (Show)

newtype Queued a = Queued (a, Double) deriving (Show)

emptyQueue :: CustomerQueue
emptyQueue = CustomerQueue (Empty, 0)

data DeskStatus = EmptyDesk | Processing CustomerQueue | Finished (Queued Customer) CustomerQueue

processCustomers :: CustomerQueue -> [Customer] -> [Queued Customer]
processCustomers (CustomerQueue (qs, started)) [] = case Seq.viewr qs of
  EmptyR -> []
  seq :> cus -> queued cus started:processCustomers (CustomerQueue (seq, processing cus + started)) []
processCustomers q (c:cs) = case deskStatus q (arriving c) of
  EmptyDesk -> processCustomers (CustomerQueue (c <| Empty, arriving c)) cs
  Processing (CustomerQueue (qs, started)) -> processCustomers (CustomerQueue (c <| qs, started)) cs
  Finished c' q' -> c':processCustomers q' (c:cs)

deskStatus :: CustomerQueue -> Double -> DeskStatus
deskStatus  q@(CustomerQueue (qs, started)) t = case Seq.viewr qs of
  EmptyR -> EmptyDesk
  seq :> cus -> if t < processing cus + started
    then Processing q
    else Finished (queued cus started) (CustomerQueue (seq, processing cus + started))

queued :: Customer -> Double -> Queued Customer
queued cus started = Queued (cus, started - arriving cus)

clist :: [Customer]
clist = [Customer 0 100, Customer 10 50, Customer 50 500, Customer 120 20]