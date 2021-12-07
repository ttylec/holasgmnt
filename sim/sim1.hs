module Main where

import Sim
import Statistics.Sample
import qualified Data.Vector as V
import Data.Random (sample)

hour = 3600 :: Double
day = 3600*8 :: Double

main :: IO ()
main = do
    clist <- sample $ customerStream Yellow  (5*day)
    let pcs = processCustomers emptyQueue clist
    print . mean . V.fromList . fmap (\(Queued (_, a)) -> a) $ pcs
