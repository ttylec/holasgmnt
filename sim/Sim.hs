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
  } deriving Show

processingTime :: CustomerType -> RVar Double
processingTime Yellow = (*200) <$> beta 2 5
processingTime Red = (*200) <$> beta 2 2
processingTime Blue = (*200) <$> beta 5 1

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

data DeskStatus a = EmptyDesk | Processing CustomerQueue | Finished a CustomerQueue

processCustomers :: CustomerQueue -> [Customer] -> [Queued Customer]
processCustomers (CustomerQueue (qs, started)) [] = case Seq.viewr qs of
  EmptyR -> []
  seq :> cus -> queued cus started:processCustomers (CustomerQueue (seq, processing cus + started)) []
processCustomers q (c:cs) = case deskStatus q (arriving c) of
  EmptyDesk -> processCustomers (CustomerQueue (c <| Empty, arriving c)) cs
  Processing (CustomerQueue (qs, started)) -> processCustomers (CustomerQueue (c <| qs, started)) cs
  Finished c' q' -> c':processCustomers q' (c:cs)

deskStatus :: CustomerQueue -> Double -> DeskStatus (Queued Customer)
deskStatus  q@(CustomerQueue (qs, started)) t = case Seq.viewr qs of
  EmptyR -> EmptyDesk
  seq :> cus -> if t < processing cus + started
    then Processing q
    else Finished (queued cus started) (CustomerQueue (seq, processing cus + started))

processCustomersWith :: (CustomerQueue -> Customer -> a) -> CustomerQueue -> [Customer] -> [a]
processCustomersWith f q@(CustomerQueue (qs, started)) [] = case Seq.viewr qs of
  EmptyR -> []
  seq :> cus -> f q cus:processCustomersWith f (CustomerQueue (seq, processing cus + started)) []
processCustomersWith f q (c:cs) = case deskStatusWith f q (arriving c) of
  EmptyDesk -> processCustomersWith f (CustomerQueue (c <| Empty, arriving c)) cs
  Processing (CustomerQueue (qs, started)) -> processCustomersWith f (CustomerQueue (c <| qs, started)) cs
  Finished c' q' -> c':processCustomersWith f q' (c:cs)

deskStatusWith :: (CustomerQueue -> Customer -> a) -> CustomerQueue -> Double -> DeskStatus a
deskStatusWith f q@(CustomerQueue (qs, started)) t = case Seq.viewr qs of
  EmptyR -> EmptyDesk
  seq :> cus -> if t < processing cus + started
    then Processing q
    else Finished (f q cus) (CustomerQueue (seq, processing cus + started))


queued :: Customer -> Double -> Queued Customer
queued cus started = Queued (cus, started - arriving cus)

queued' :: CustomerQueue -> Customer -> Queued Customer
queued' (CustomerQueue (_, started)) cus = Queued (cus, started - arriving cus)

clist :: [Customer]
clist = [Customer 0 100, Customer 10 50, Customer 50 500, Customer 120 20]

data EventType = NewInQueue Customer | Processed (Queued Customer) deriving (Show)

data Event = Event { event :: EventType, at :: Double, status :: CustomerQueue } deriving (Show)

newInQueue :: CustomerQueue -> Customer -> Event
newInQueue q cus = Event { event = NewInQueue cus, at = arriving cus, status = q}

processed :: CustomerQueue -> Customer -> Event
processed q@(CustomerQueue (_, started)) cus = Event
  { event = Processed (queued cus started)
  , at = started + processing cus
  , status = q}

processingEvents :: CustomerQueue -> [Customer] -> [Event]
processingEvents q@(CustomerQueue (qs, started)) [] = case Seq.viewr qs of
  EmptyR -> []
  seq :> cus -> processed q cus:processingEvents (CustomerQueue (seq, processing cus + started)) []
processingEvents q (c:cs) = case status q (arriving c) of
  EmptyDesk -> newInQueue q c:processingEvents (CustomerQueue (c <| Empty, arriving c)) cs
  Processing (CustomerQueue (qs, started)) -> newInQueue q c:processingEvents (CustomerQueue (c <| qs, started)) cs
  Finished c' q' -> processed q c':processingEvents q' (c:cs)
  where
    status :: CustomerQueue -> Double -> DeskStatus Customer
    status q@(CustomerQueue (qs, started)) t = case Seq.viewr qs of
      EmptyR -> EmptyDesk
      seq :> cus -> if t < processing cus + started
        then Processing q
        else Finished cus (CustomerQueue (seq, processing cus + started))