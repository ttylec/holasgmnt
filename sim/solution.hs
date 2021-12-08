module Main where

import Sim

main :: IO ()
main = do
    putStrLn "Task 1: Given only yellow customers, what are the average and maximum customer waiting times?"
    task1
    putStrLn "Task 2: Given only red customers, what are the average and maximum queue lengths in-front of the teller?"
    task2
    putStrLn "Task 2 correction: Given only red customers, what are the average and maximum queue lengths in-front of the teller?"
    task2'
    putStrLn "Task 3: Which type of customer(yellow, red or blue) gives the gives the closest value between the average and maximum customer waiting times?"
    task3
    putStrLn "Task 3 correction: Which type of customer(yellow, red or blue) gives the gives the closest value between the average and maximum customer waiting times?"
    task3'