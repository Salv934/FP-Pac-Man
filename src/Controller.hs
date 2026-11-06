-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Prelude hiding (Left, Right)
import Model

import Graphics.Gloss (Point)
import Graphics.Gloss.Interface.IO.Game
    (Event(..), Key(..), SpecialKey(..))
import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import Maze

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


pacmanSpawn :: Point
pacmanSpawn = pacmanStart

ghostCollision :: Pacman -> Ghost -> Bool
ghostCollision p g = distance (pacmanPos p) (ghostPos g) < 25



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

releaseGhost :: GameState -> IO GameState
releaseGhost gstate
    | null (ghostWaiting gstate) = return gstate
    | not releaseDue = return gstate
    | otherwise = do
        let waiting = ghostWaiting gstate
        index <- randomRIO (0, length waiting - 1)
        let behavior = waiting !! index
            remaining = take index waiting ++ drop (index + 1) waiting
        return gstate
            { ghosts = ghosts gstate ++ [createGhost behavior]
            , ghostWaiting = remaining
            , ghostReleaseTimer = 0
            , ghostReleaseScore = score gstate
            , firstGhostPending = False
            }
  where
    releaseDue = firstGhostPending gstate || ghostReleaseTimer gstate >= 5 || score gstate - ghostReleaseScore gstate >= 300


-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
    | not (gameStarted gstate) = return gstate | isPaused gstate = return gstate | lives gstate <= 0 = return gstate 
    | otherwise = do
        releasedState <- releaseGhost gstate
        return (advanceGame secs releasedState)

advanceGame :: Float -> GameState -> GameState
advanceGame secs gstate = clearedState
  where
    movedPacman = currMovePacmanMaze (maze gstate) secs (pacman gstate)
    (collectedMaze, gainedPoints, powpelEaten) = collectPellet (maze gstate) (pacmanPos movedPacman )
    remaining = 
        if powpelEaten
        then powerPelletDur
        else max 0 (powerPelletTimer gstate - secs)
    currGhost = map (updateGhostState remaining . frightenOnPickup) (ghosts gstate)
    frightenOnPickup g =
        if powpelEaten
        then g {ghostState = Fleeing}
        else g 
    movedGhosts = map(moveGhostMaze(maze gstate) secs movedPacman currGhost) currGhost
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
    danger = filter (\g -> not (isFrightend g) && ghostCollision movedPacman g) movedGhosts
    hit = not (null danger)
    updatePacman =
        if hit 
        then (movedPacman) {pacmanPos = pacmanSpawn}
        else movedPacman
    updateGhost =
        if hit 
        then []
        else survivor ++ respawned
    updateLives =
        if hit 
        then max 0 (lives gstate - 1)
        else lives gstate
    levelCleared = not hit && remainingPellets  collectedMaze == 0
    nextLevel = level gstate + 1
    nextStage =
        (initialDiffState (difficulty gstate))
            { level = nextLevel
            , maze = mazeLevel nextLevel
            , lives = updateLives
            , score = score gstate + gainedPoints}
            , ghostReleaseScore = score gstate + gainedPoints
    finalState = gstate {elapsedTime = elapsedTime gstate + secs
                        ,pacman = updatePacman
                        ,powerPelletTimer = if hit then 0 else remaining
                        , ghosts = updateGhost
                        ,ghostRespawns = if hit then [] else stillWaiting
                        ,lives = updateLives
                        , gameStarted = if hit then False else gameStarted gstate
                        , maze = collectedMaze
                        , score = score gstate + gainedPoints
                        , ghostReleaseScore =
                            if hit
                            then score gstate + gainedPoints
                            else ghostReleaseScore gstate
                        , ghostWaiting =
                            if hit
                            then map ghostBehavior (ghostsForDifficulty (difficulty gstate))
                            else ghostWaiting gstate
                        , firstGhostPending =
                            if hit then True else firstGhostPending gstate
                            clearedState =
                                if levelCleared
                                then nextStage
                                else finalState}
    clearedState =
        if levelCleared
        then nextStage
        else finalState
    
    
    



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

inputKey :: Event -> GameState ->GameState

inputKey (EventKey (Char 'r') Gloss.Down _ _) gstate | lives gstate <= 0 = initialDiffState (difficulty gstate)

inputKey _ gstate | lives gstate <= 0 = gstate

{- inputKey (EventKey (Char c) _ _ _) gstate
  = -- If the user presses a character key, show that one
    gstate { infoToShow = ShowAChar c }
inputKey _ gstate = gstate -- Otherwise keep the same
-}

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

inputKey (EventKey(Char 'p') Gloss.Down _ _) gstate
  = gstate { isPaused = not (isPaused gstate)}


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
    in gstate 
    { gameStarted = True
    , pacman = p 
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

neighbor :: Direction -> Cell -> Cell
neighbor dir (x, y)=
    case dir of
        Up -> (x, y - 1)
        Down -> (x, y + 1)
        Left -> (x - 1, y)
        Right -> (x + 1, y)

opposite :: Direction -> Direction -> Bool
opposite Up Down = True
opposite Down Up = True
opposite Left Right = True
opposite Right Left = True
opposite _ _ = False

moveInDirection :: Direction -> Float -> Point -> Point
moveInDirection dir amount (x, y) =
    case dir of
        Up    -> (x, y + amount)
        Down  -> (x, y - amount)
        Left  -> (x - amount, y)
        Right -> (x + amount, y)

currMovePacmanMaze :: Maze -> Float -> Pacman -> Pacman
currMovePacmanMaze board secs p =
    let pos = pacmanPos p
        cell = pointCell pos
        center = tileCenter cell
        amount = pacmanSpeed * secs
        nearCenter = distance pos center <= amount + 0.5
        desired = pacmanNextDir p
        current = pacmanDir p
        mayTurn =
            desired /= current &&
            nearCenter &&
            isOpen board (neighbor desired cell)
        reverseNow = opposite desired current
        dir =
            if mayTurn || reverseNow
            then desired
            else current
        startPos = if mayTurn then center else pos
        next = neighbor dir (pointCell startPos)
        proposed = moveInDirection dir amount startPos
        safePos =
            if isOpen board next || not nearCenter
            then
                if isOpen board (pointCell proposed)
                then proposed
                else center
            else center
    in p
        { pacmanPos = safePos , pacmanDir = dir}

moveGhostMaze
    :: Maze
    -> Float
    -> Pacman
    -> [Ghost]
    -> Ghost
    -> Ghost

moveGhostMaze board secs p active g =
    let pos = ghostPos g
        oldWaypoint = ghostTargetTile g
        reached = distance pos oldWaypoint < 0.01
        target = ghostTar p active g
        waypoint = if reached
            then
                tileCenter $
                    nextGhostCell
                        board
                        (pointCell pos)
                        (pointCell target)
            else oldWaypoint
        nextPos =
            moveTo
                (ghostSpeed g)
                secs
                pos
                waypoint
    in g
        { ghostPos = nextPos, ghostTargetTile = waypoint}