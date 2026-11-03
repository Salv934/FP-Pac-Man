-- | This module contains the data types
--   which represent the state of the game
module Model where

import Prelude hiding (Left, Right)
import Graphics.Gloss (Point, Color, red, rose, cyan, orange, green)

data Direction = Up | Down | Left| Right
    deriving (Eq, Show)

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

data Ghost = Ghost
     { ghostPos :: Point
     , ghostColor :: Color
     }

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , pacman      :: Pacman
                 , ghosts      :: [Ghost]
                 }

initialState :: GameState
initialState = GameState {
    infoToShow = ShowNothing
    , elapsedTime = 0
    , pacman = Pacman {
        pacmanPos = (-50, 0)
        , pacmanDir = Right
        , pacmanNextDir = Right
        }
    , ghosts = 
        [Ghost (50, 0) red          -- Blinky
        , Ghost (75, -30) rose      -- Pinky
        , Ghost (100, 0) cyan       -- Inky
        , Ghost (75, 30) orange     -- Clyde
        , Ghost (-100, 0) green     -- Jeffrey
        ]
    }