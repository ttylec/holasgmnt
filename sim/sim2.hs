module Main where

import Sim
import Statistics.Sample
import qualified Data.Vector as V
import Data.Random (sample)
import Data.Maybe (catMaybes, mapMaybe)

hour = 3600 :: Double
day = 3600*8 :: Double

isProcessed :: EventType -> Bool
isProcessed (Processed _) = True
isProcessed _ = False

time :: Event -> Maybe Double
time e@Event{event=Processed (Queued (_, t))} = Just t
time _ = Nothing

main :: IO ()
main = do
    clist <- sample $ customerStream Yellow  (5*day)
    let pcs = processingEvents emptyQueue clist
    print . mean . V.fromList $ mapMaybe time pcs

    let pcs' = processCustomers emptyQueue clist
    print . mean . V.fromList . fmap (\(Queued (_, a)) -> a) $ pcs'
