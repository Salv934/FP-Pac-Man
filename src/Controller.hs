-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Prelude hiding (Left, Right)
import Model

import Graphics.Gloss
import Graphics.Gloss hiding (Down)
import Graphics.Gloss.Interface.IO.Game
    (Event(..), Key(..), SpecialKey(..))
import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do randomNumber <- randomIO
       let newNumber = abs randomNumber `mod` 10
       return $ gstate { infoToShow = ShowANumber newNumber
                 , elapsedTime = 0
                 }
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

{- inputKey (EventKey (Char c) _ _ _) gstate
  = -- If the user presses a character key, show that one
    gstate { infoToShow = ShowAChar c }
inputKey _ gstate = gstate -- Otherwise keep the same
-}

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyDown) Gloss.Down _ _) gstate
  = movePacman 0 (-20) gstate

inputKey (EventKey (SpecialKey KeyUp) Gloss.Down _ _) gstate
  = movePacman 0 20 gstate

inputKey (EventKey (SpecialKey KeyRight) Gloss.Down _ _) gstate
  = movePacman 20 0 gstate

inputKey (EventKey (SpecialKey KeyLeft) Gloss.Down _ _) gstate
  = movePacman (-20) 0 gstate


inputKey (EventKey (Char 'w') Gloss.Down _ _) gstate
  = movePacman 0 20 gstate

inputKey (EventKey (Char 'a') Gloss.Down _ _) gstate
  = movePacman (-20) 0 gstate

inputKey (EventKey (Char 's') Gloss.Down _ _) gstate
  = movePacman 0 (-20) gstate

inputKey (EventKey (Char 'd') Gloss.Down _ _) gstate
  = movePacman 20 0 gstate

{-inputKey (EventKey (Char c) _ _ _) gstate
  = -- If the user presses a character key, show that one
    gstate { infoToShow = ShowAChar c }
    -}
  

-- other events
inputKey _ gstate = gstate


movePacman :: Float -> Float -> GameState -> GameState
movePacman dx dy gstate 
  = let p = pacman gstate
        (x, y) = pacmanPos p
    in gstate { 
    pacman = p 
         {pacmanPos = (x + dx, y + dy)}
    }