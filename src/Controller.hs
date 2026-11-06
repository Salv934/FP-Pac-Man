-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Prelude hiding (Left, Right)
import Model

import Graphics.Gloss (Point)
import Graphics.Gloss.Interface.IO.Game
    (Event(..), Key(..), SpecialKey(..))
import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import System.Random

import Data.List (find, partition, sortOn)

screenWidth :: Float
screenWidth = 800
screenHeight :: Float
screenHeight = 600

wrapPoint :: Point -> Point
wrapPoint (x, y) 
    = (coordinate (screenWidth / 2) x, coordinate(screenHeight / 2) y)
  where
    coordinate limit value
        | value > limit = -limit
        | value < -limit = limit
        | otherwise = value


distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 -y1)^2)

moveTo :: Float -> Float -> Point -> Point -> Point
moveTo speed secs (x,y) (tx,ty)
    | d < 0.001 =(x,y)
    | otherwise = (x + dx * fact, y +dy *fact )
  where
    dx = tx -x
    dy = ty -y 
    d = distance (x,y) (tx,ty)
    fact = min 1 (speed * secs /d)

ahead :: Float -> Pacman -> Point
ahead tiles p = 
    let (x,y) = pacmanPos p
        d = tiles * 20
    in case pacmanDir p of
        Up -> (x, y + d)
        Down -> (x,y -d)
        Left -> (x - d, y)
        Right -> (x + d, y)


ghostLoc :: GhostBehavior -> [Ghost] -> Point -> Point
ghostLoc behavior active fallback =
    case find ((==behavior) . ghostBehavior) active of
        Just g -> ghostPos g
        Nothing -> fallback


scatterSpot :: GhostBehavior -> Point
scatterSpot Blinky = ( 250, 200)
scatterSpot Pinky = (-250, 200)
scatterSpot Inky = (250, -200)
scatterSpot Clyde = (-250, -200)
scatterSpot Jeffrey = (0,200)


chasePac :: Pacman -> [Ghost] -> Ghost -> Point
chasePac p active g =
    case ghostBehavior g of
        Blinky -> pacmanPos p
        Pinky -> ahead 4 p 
        Inky -> let (ax, ay) = ahead 2 p 
                    (bx, by) = ghostLoc Blinky active (0,0)
                in (2* ax - bx, 2 * ay - by)
        Clyde -> if distance (ghostPos g) (pacmanPos p) > 8 * 20
                 then pacmanPos p
                 else scatterSpot Clyde
        Jeffrey -> let (px, py) = pacmanPos p
                       (bx, by) = ghostLoc Blinky active (px, py)
                       (kx, ky) = ghostLoc Pinky active (px, py)
                       (ax, ay) = ahead 4 p 

                       targetB = (2* px -bx, 2 * py - by)
                       targetP = ((ax +kx) / 2, (ay  + ky)/2)

                       current = ghostPos g 
                   in if distance current targetB <= distance current targetP
                       then targetB
                       else targetP
                

ghostSpeed :: Ghost -> Float
ghostSpeed g =
    let normalSpeed =
              if ghostBehavior g == Jeffrey
              then 50
              else 40
    in if isFrightend g
       then normalSpeed * 0.70
       else normalSpeed

isFrightend :: Ghost -> Bool
isFrightend g = ghostState g == Fleeing || ghostState g == FleeingPeriodEnding

fleeFromPac :: Pacman -> Ghost -> Point
fleeFromPac p g =
    let (gx, gy) = ghostPos g
        (px, py) = pacmanPos p
        dx = gx -px 
        dy = gy - py
    in if abs dx + abs dy < 0.001
       then (gx + 100, gy)
       else (gx+dx, gy +dy)


ghostTar :: Pacman -> [Ghost] -> Ghost -> Point
ghostTar p active g =
    case ghostState g of
    AtGhostRoom -> ghostPos g 
    Scatter -> scatterSpot (ghostBehavior g)
    Chasing -> chasePac p active g 
    Fleeing -> fleeFromPac p g 
    FleeingPeriodEnding -> fleeFromPac p g 
    Eaten -> spawnPos (ghostBehavior g)
    ReturningToGhostRoom -> spawnPos (ghostBehavior g)


moveGhost :: Float -> Pacman -> [Ghost] -> Ghost -> Ghost
moveGhost secs p active g =
    let target = ghostTar p active g 
        nextPos = moveTo
             (ghostSpeed g)
             secs
             (ghostPos g)
             target
    in g {ghostPos = wrapPoint nextPos}


powerPelletDur :: Float
powerPelletDur = 10

activatePowerPellet :: GameState -> GameState
activatePowerPellet gstate =
    gstate {powerPelletTimer = powerPelletDur, ghosts = map frighten (ghosts gstate)}
  where
    frighten g = g {ghostState = Fleeing}


updateGhostState :: Float -> Ghost -> Ghost
updateGhostState remaining g 
    | remaining > 2 =
        if isFrightend g 
        then g {ghostState = Fleeing}
        else g 
    | remaining > 0 =
        if isFrightend g 
        then g {ghostState = FleeingPeriodEnding}
        else g 
    | isFrightend g = g { ghostState = Chasing}
    | otherwise = g 


eatGhost :: GhostBehavior -> GameState -> GameState
eatGhost behavior gstate =
    let (eaten, survivor) = partition(\g -> ghostBehavior  g == behavior && isFrightend g)
                                     (ghosts gstate)
        respawnG = [(ghostBehavior g, 10) | g <- eaten]
    in gstate{ ghosts = survivor, ghostRespawns = ghostRespawns gstate ++ respawnG}   


eatClosestGhost :: GameState -> GameState
eatClosestGhost gstate =
    let fright = filter isFrightend (ghosts gstate)
        nearest = sortOn(distance (pacmanPos (pacman gstate)) . ghostPos) fright
    in case nearest of 
        [] -> gstate
        (g:_) -> eatGhost (ghostBehavior g) gstate


-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return finalState
  where
    movedPacman = currMovePacman secs (pacman gstate)
    remaining = max 0 (powerPelletTimer gstate - secs)
    currGhost = map (updateGhostState remaining) (ghosts gstate)
    movedGhosts = map(moveGhost secs (movedPacman) currGhost) currGhost
    (eaten, survivor) = partition (\g -> isFrightend g && distance (pacmanPos (movedPacman))(ghostPos g) < 25) movedGhosts
    respawnG = [(ghostBehavior g, 10) | g <- eaten]
    waiting = [(b, t -secs) | (b,t) <- ghostRespawns gstate ++ respawnG]
    stillWaiting = [(b,t) | (b,t) <- waiting, t > 0]
    ready = [b | (b,t) <- waiting, t <= 0]
    respawned = [let g = createGhost b
                 in if remaining > 0
                    then g { ghostState =
                               if remaining <= 2
                               then FleeingPeriodEnding
                               else Fleeing}
                    else g | b <- ready]
    finalState = gstate {elapsedTime = elapsedTime gstate + secs
                        ,pacman = movedPacman
                        ,powerPelletTimer = remaining
                        ,ghosts = survivor ++ respawned
                        ,ghostRespawns = stillWaiting}
    



{- step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do randomNumber <- randomIO
       let newNumber = abs randomNumber `mod` 10
       return $ gstate { infoToShow = ShowANumber newNumber
                 , elapsedTime = 0
                 }
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs } -}




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
  = movePacman Down gstate

inputKey (EventKey (SpecialKey KeyUp) Gloss.Down _ _) gstate
  = movePacman Up gstate

inputKey (EventKey (SpecialKey KeyRight) Gloss.Down _ _) gstate
  = movePacman Right gstate

inputKey (EventKey (SpecialKey KeyLeft) Gloss.Down _ _) gstate
  = movePacman  Left gstate


inputKey (EventKey (Char 'w') Gloss.Down _ _) gstate
  = movePacman Up gstate

inputKey (EventKey (Char 'a') Gloss.Down _ _) gstate
  = movePacman Left gstate

inputKey (EventKey (Char 's') Gloss.Down _ _) gstate
  = movePacman Down gstate

inputKey (EventKey (Char 'd') Gloss.Down _ _) gstate
  = movePacman Right gstate

inputKey (EventKey(Char 'k') Gloss.Down _ _) gstate
  = eatClosestGhost gstate

inputKey (EventKey(Char 'l') Gloss.Down _ _) gstate
  = activatePowerPellet gstate


{-inputKey (EventKey (Char c) _ _ _) gstate
  = -- If the user presses a character key, show that one
    gstate { infoToShow = ShowAChar c }
    -}
  

-- other events
inputKey _ gstate = gstate

pacmanSpeed :: Float
pacmanSpeed = 120

movePacman :: Direction -> GameState -> GameState
movePacman dir gstate 
  = let p = pacman gstate
    in gstate { 
    pacman = p 
         {
          pacmanDir = dir 
         , pacmanNextDir = dir}
    }

currMovePacman :: Float -> Pacman -> Pacman
currMovePacman secs p 
  = let (x, y) = pacmanPos p
        amount = pacmanSpeed * secs
        newPos =
            case pacmanDir p of
                Up -> (x, y + amount)
                Down -> (x, y - amount)
                Left -> (x - amount, y)
                Right -> (x + amount, y)
    in p { pacmanPos = wrapPoint newPos}