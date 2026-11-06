-- | This module contains the data types
--   which represent the state of the game
module Model where

import Prelude hiding (Left, Right)
import Graphics.Gloss (Point, Color, red, rose, cyan, orange, green)

data Difficulty = Easy | Normal | Hard
    deriving (Eq, Show)

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
     , ghostBehavior :: GhostBehavior
     , ghostState :: GhostState
     }

data GhostBehavior = Blinky | Pinky | Inky | Clyde | Jeffrey deriving (Eq, Show)

data GhostState = AtGhostRoom | Scatter | Chasing | Fleeing | FleeingPeriodEnding | Eaten | ReturningToGhostRoom deriving(Eq, Show)

type GhostRespawns = (GhostBehavior, Float)

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , pacman      :: Pacman
                 , ghosts      :: [Ghost]
                 , ghostRespawns :: [GhostRespawns]
                 , powerPelletTimer :: Float
                 , gameStarted :: Bool
                 , isPaused :: Bool
                 , lives :: Int
                 , difficulty :: Difficulty
                 }

createGhost :: GhostBehavior -> Ghost
createGhost behavior = Ghost{
        ghostPos =spawnPos behavior
    ,   ghostColor = collorGhost behavior
    ,   ghostBehavior = behavior
    ,   ghostState = Chasing
}

spawnPos :: GhostBehavior -> Point
spawnPos Blinky = (50, 0)
spawnPos Pinky = (75,-30)
spawnPos Inky = (100, 0)
spawnPos Clyde = (75, 30)
spawnPos Jeffrey = (-100, 0)

collorGhost :: GhostBehavior -> Color
collorGhost Blinky = red
collorGhost Pinky = rose
collorGhost Inky = cyan
collorGhost Clyde = orange
collorGhost Jeffrey = green

initialGhost :: [Ghost]
initialGhost =[
        createGhost Blinky
    ,   createGhost Pinky
    ,   createGhost Inky
    ,   createGhost Clyde
    ,   createGhost Jeffrey
    ]


initialState :: GameState
initialState = GameState {
    infoToShow = ShowNothing
    , elapsedTime = 0
    , pacman = Pacman {
        pacmanPos = (-50, 0)
        , pacmanDir = Right
        , pacmanNextDir = Right
        }
    , ghosts = initialGhost
    ,    ghostRespawns = []
    ,   powerPelletTimer = 0
    , gameStarted = False 
    , isPaused = False 
    , lives = 3
    , difficulty = Normal

    }

initialDiffState :: Difficulty -> GameState
initialDiffState selectedDiff = 
    initialState
        {difficulty = selectedDiff, ghosts = selectedGhosts}
  where
    selectedGhosts =
        case selectedDiff of
            Easy -> map createGhost [Blinky, Pinky]
            Normal -> map createGhost [Blinky, Pinky, Inky, Clyde]
            Hard -> map createGhost [Blinky, Pinky, Inky, Clyde, Jeffrey] 