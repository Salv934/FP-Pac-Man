module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
    putStrLn "Choose difficulty:"
    putStrLn "1. Easy"
    putStrLn "2. Normal"
    putStrLn "3. Hard"

    choice <- getLine

    let selectedDiff = case choice of
            "1" -> Easy
            "3" -> Hard
            _ -> Normal

    playIO (InWindow "Counter" (800, 600) (100, 100)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              (initialDiffState selectedDiff)     -- diffuculty setting
              view             -- View function
              input            -- Event function
              step             -- Step function

