{-# LANGUAGE NamedFieldPuns #-}
module Sim where

import Data.Random ( uniform, RVar, sample )
import Data.Random.Distribution.Exponential (exponential)
import Data.Random.Distribution.Beta (beta)
import Data.List (unfoldr, foldl', minimumBy)
import Data.Ord (comparing)
import Control.Monad.Loops (unfoldrM)
import Control.Monad (replicateM)
import qualified Data.Vector as V
import Statistics.Sample (mean)

{-

Solution will be separated into two parts:
- generation of random list of customers
- computing statistics (so that we can use pure functions)

The first iteration of computing statistics was over-engineered
and can be found in the SimOld.hs file. After trying to make
that more readable, I realised that there is much cleaner
solution implemented here.

We start with defining some useful types.
-}

-- | Represents type of the customer
data CustomerType = Yellow | Red | Blue deriving (Show)

-- | Represents a customer
data Customer = Customer {
  -- | arrival time of the customer (absolute units)
    arriving :: Double
  -- | time that it takes to process that customer when get to the service desk
  -- in relative units.
  , processing :: Double
  } deriving (Eq, Ord, Show)

{-

If we would aim for additional type-safety we should
use different types for the 'arriving' and 'processing'
fields. The first one is in absolute clock units, like
the 'UTCTime' type. The second is relative, so would
correspond to 'DiffTime'.

-}

{-

For generating random numbers I use the random-fu package.
It may be not the fastest library, but offers very clean
interface.

The core function is generation of the stream of customers.
Problem specification was a bit unprecise, but as I understand,
provided function F(t) is a cumulative distribution function,
so the value should be interpreted as:

Value of F(t) at time t, where t is the time since last customer
arrived, is probability that the next customer arrived by that time.
Provided function F(t) is cumulative distribution of exponential
probability distribution (which can be easily obtained by differentiation,
or just by recognizing the form). As the result, the time difference when
the next customer arrives I draw from the exponenetion distribution.

-}

-- | Stream of customers of specific type.
customerStream :: CustomerType -> Double -> RVar [Customer]
customerStream ctype end_time = unfoldrM gen 0
  where
    gen t0
      | t0 > end_time = pure Nothing
      | otherwise = do
          dt <- exponential 100
          p <- processingTime ctype
          let t = t0 + dt
          pure $ Just (Customer {arriving=t, processing=p}, t)

{-

Generation of processing time for each customer is more tricky:
problem description first desribe as coming from rescaled beta distribution,
i.e.:

-}

-- | Processing time for a customer type
processingTime :: CustomerType -> RVar Double
processingTime Yellow = (*200) <$> beta 2 5
processingTime Red = (*200) <$> beta 2 2
processingTime Blue = (*200) <$> beta 5 1

{-

However, the method described in the problem statement
of getting the number, that corresponds to following code:

-}

betaF :: Floating a => a -> a -> a -> a
betaF alpha beta x = 200 * x**(alpha-1) * (1-x)**(beta-1)

processingTime' :: CustomerType -> RVar Double
processingTime' Yellow = betaF 2 5 <$> uniform 0 1
processingTime' Red = betaF 2 2 <$> uniform 0 1
processingTime' Blue = betaF 5 1 <$> uniform 0 1

{-

is not the correct method of sampling from beta
(or any) distribution.

This can be easily seen by generating valeus using both
methods and then comparing histograms to the density function
of beta probability distribution. One can use following
simple function to generate a csv file that can be conveniently
plotted using jupyter notebook/python:

-}

samplingBeta :: IO ()
samplingBeta = do
  xs <- sample (replicateM 1000 $ beta 2 5) :: IO [Double]
  xs' <- sample (replicateM 1000 (betaF 2 5 <$> uniform 0 1)) :: IO [Double]
  putStr . unlines . fmap (\(x, x') -> show x <> ", " <> show x') $ zip xs xs'

{-

Finally, it is not clear whether the scaling factor
200 includes normalization factor of beta distribution or
not. If it does, then we should use following definition:

-}

processingTime'' :: CustomerType -> RVar Double
processingTime'' Yellow = (\x -> x*200*0.03333) <$> beta 2 5
processingTime'' Red = (\x -> x*200*0.16) <$> beta 2 2
processingTime'' Blue = (\x -> x*200*0.2) <$> beta 5 1

{-

but this in numerical simulations result in different order
of magnitude between mean time between customer arriving and
processing time. So we stick with 'processingTime'.

-}

{-

Then we proceed to define helper functions for computing
statistics.

We will compute statistics based on the stream of 'Event's
than can happen (and are of interest for us, thus the
parameter 'a')

-}

-- | Represents one event occuring at 'eventTime'. The payload
-- of the 'Event' depends on the particular problem:
-- this can be waiting time for the customer that had been served,
-- or 'CustomerEvent' defined later.
data Event a = Event { eventTime :: Double, event :: a } deriving (Eq, Show)

-- | We order events by the their time.
instance Eq a => Ord (Event a) where
  compare = comparing eventTime

-- | Event that desribe either new customer being added to the queue
-- or a customer that had been "processed"
data CustomerEvent = NewCustomer Customer | ServedCustomer Customer Double deriving (Eq, Show)

-- | Walks through the (sorted) list of customers
-- and creates event with payload generated by function 'f'
-- whenever 'Customer' is "done" by the teller.
--
-- The first argument for function 'f' is that customer
-- the second argument is a timestamp when the customer
-- got to the teller's desk.
served :: (Customer -> Double -> a) -> [Customer] -> [Event a]
served f cs = unfoldr go (0, cs)
  where
    go (_, []) = Nothing
    go (last, c:cs) =
      let event =  Event finished (f c started)
          started = max last (arriving c)
          finished = started + processing c
      in Just (event, (finished, cs))

-- | Time that customer spent in the queue.
waitingTime :: Customer -> Double -> Double
waitingTime Customer{arriving} started = started - arriving

{-

We have sufficient elements to write solution for the
first task.

-}

-- | For a given (sorted) list of 'Customer's return
-- the mean and maximum processing time.
--
-- Note that
-- > served waitingTime clist
--
-- Is a list of events where the payload is a waiting time the queue.
meanMaxProcessingTime :: [Customer] -> (Double, Double)
meanMaxProcessingTime clist =
    let times = V.fromList . fmap event $ served waitingTime clist
    in (mean times, V.maximum times)

day :: Double
day = 3600*8

{- | Solution to task 1:

Given only yellow customers, what are the average and maximum customer waiting times?
-}
task1 :: IO ()
task1 = do
    clist <- sample (customerStream Yellow (1000*day))

    let (mu, max) = meanMaxProcessingTime clist
    putStrLn $ "Mean waiting time: " <> show mu
    putStrLn $ "Max waiting time: " <> show max


{-

In order to solve task 2 we need to compute length of
the queue. To do so, we need to have a stream of events
for both arriving and leaving clients.

-}

-- | Construct stream of all events
--
-- We could simply concatenate and sort 'arrivals cs' and 'served ...'
-- lists, but that would increase complexity. We can "merge as we go"
-- with explicit recursion.
allEvents :: [Customer] -> [Event CustomerEvent]
allEvents cs = allEvents' (arrivals cs) (served ServedCustomer cs)
  where
    arrivals = fmap (\c -> Event (arriving c) (NewCustomer c))
    allEvents' [] ss = ss
    allEvents' as [] = as
    allEvents' (a:as) (s:ss)
      | a < s = a:allEvents' as (s:ss)
      | otherwise = s:allEvents' (a:as) ss


-- | Folds list of events to produce list of queue lenghts when the
-- new customer arrives.
queueAtArrival :: [Event CustomerEvent] -> [Int]
queueAtArrival = snd . foldl' go (0, []) . fmap event
  where
    -- we increase queue length (first element of the pair)
    -- when the new customer arrives. Second element of the tuple
    -- is our "result": when that customer arrived, the queue
    -- had length 'l'
    go (l, ls) (NewCustomer _) = (l+1, l:ls)
    -- when customer is leaving, queue become shorter.
    -- Nothing to append to the result.
    go (l, ls) (ServedCustomer _ _ ) = (l-1, ls)

{-|

Now we can easily define solution for task 2:

Given only red customers, what are the average and maximum queue lengths in-front of the teller?

-}
task2 :: IO ()
task2 = do
    clist <- sample (customerStream Red (1000*day))

    let queue_sizes = V.fromList . queueAtArrival $ allEvents clist
        mean_size = mean $ V.map fromIntegral queue_sizes
    putStrLn $ "Mean customer queue for arriving customers: " <> show mean_size
    putStrLn $ "Maximum queue length: " <> show (V.maximum queue_sizes)


{-

However, obtained results for a given parameters lie outside
domain of the problem (avg. length of queue has order of magnitude of ~1000).

This is due to the fact, that mean processing time for 'Red'
customers is 100s, which is mean time between new arrivals.
As a result, it is quite probable that there will be a "traffic jam"
at teller station, which when onset is hard to unload.

One can make plots with csv data generated by the `sizes.hs` script.

So far we run simulation in what could be called continuous mode.
It assumed that a steady state can be obtained within time span of
a one work day. This may be true for the 'Yellow' type but certainly is false
for the 'Red'.

Consequently, the proper way to run simulation is to make
single run for no longer that one day (8 hours) and repeat for many days.
-}

task2' :: IO ()
task2' = do
  (mean_queues, max_queues) <- unzip <$> sample (replicateM 300 oneDay)
  putStrLn $ "Mean customer queue: " <> show (mean . V.fromList $ mean_queues)
  putStrLn $ "Maximum queue length: " <> show (maximum max_queues)
  where
    oneDay :: RVar (Double, Int)
    oneDay = do
      clist <- sample (customerStream Red day)
      let queue_sizes = V.fromList . queueAtArrival $ allEvents clist
          mean_size = mean $ V.map fromIntegral queue_sizes
      pure (mean_size, V.maximum queue_sizes)


{-

Finally, the task 3:

Which type of customer(yellow, red or blue) gives the gives the closest value between the average and maximum customer waiting times?

-}
task3 :: IO ()
task3 = do
  ans <- minimumBy (comparing fst) <$> mapM diff [Yellow, Red, Blue]
  putStrLn ("Min difference: " <> show ans)
  where
    diff ctype = do
      (mu, max) <- meanMaxProcessingTime <$> sample (customerStream ctype (1000*day))
      pure (abs (max - mu), ctype)

{-

Similarly, result may be different if take into account that working day has a fixed
length.

-}
minMaxDiffDay :: CustomerType -> RVar Double
minMaxDiffDay ctype = do
  (mu, max) <- meanMaxProcessingTime <$> sample (customerStream ctype day)
  pure $ abs (max - mu)

task3' :: IO ()
task3' = do
  ans <- minimumBy (comparing fst) <$> sample (mapM meanDiff [Yellow, Red, Blue])
  putStrLn ("Min difference: " <> show ans)
  where
    diffs ct = replicateM 300 $ minMaxDiffDay ct
    meanDiff ct = (,) <$> (mean . V.fromList <$> diffs ct) <*> pure ct

-- | Helper function to verify that we properly sample arrival times.
arrivalDiffs :: [Customer] -> [Double]
arrivalDiffs cs = fmap (\(c, c') -> arriving c' - arriving c) $ zip cs (tail cs)