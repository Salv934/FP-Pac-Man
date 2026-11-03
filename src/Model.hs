-- | This module contains the data types
--   which represent the state of the game
module Model where

import Prelude hiding (Left, Right)
import Graphics.Gloss (Point)

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data Pacman = Pacman
     { pacmanPos :: Point,
       pacmanDir :: Direction,
       pacmanNextDir :: Direction
     }

Data Ghost = Ghost
     { ghostPos :: Point
     }

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , pacman      :: Pacman
                 , ghosts      :: Ghost
                 }

initialState :: GameState
initialState = GameState {
    ShowNothing 0
    , elapsedTime = 0
    , pacman = Pacman {
        pacmanPos = (-50, 0)
        , pacmanDir = Right
        , pacmanNextDir = Right
        }
    , ghosts = Ghost {
        ghostPos = (50, 0)
        }
    }