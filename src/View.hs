-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Maze
import Prelude hiding (Left, Right)

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures
	[ drawMaze (maze gstate)
  ,  pictures (map drawGhost (ghosts gstate))
  , drawPacman (pacman gstate)
  , drawInfo (infoToShow gstate)
  , drawHUD gstate
  , drawLives (lives gstate)
  , drawPausedOnScreen gstate
  , drawGameOver gstate
	]

drawInfo :: InfoToShow -> Picture
drawInfo info = case info of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])

drawPacman :: Pacman -> Picture
drawPacman p =
  let (x, y) = pacmanPos p
  in translate x y $
	 animatePac p

animatePac :: Pacman -> Picture
animatePac p =
    let phase = pacMouth p 
        open = 1- 4 * abs (phase -0.25)
        angle = 60 * open
        rotation =
            case pacmanDir p of
                Right -> 0
                Down    -> 90
                Left  -> 180
                Up  -> 270
    in rotate rotation $ color yellow $ arcSolid (angle / 2) (360 - angle / 2) 12

drawGhost :: Ghost -> Picture
drawGhost g =
  let (x, y) = ghostPos g
  in translate x y $
	 color (getGhostColor g) $
		polygon [(-10, -8), (10, -8), (0, 11)]

drawPausedOnScreen :: GameState -> Picture
drawPausedOnScreen gstate
  | isPaused gstate = 
      pictures 
        [color (makeColor 0 0 0 0.6) $ polygon [(-400, -300), (400, -300), (400, 300), (-400, 300)]
        , translate (-140) 20 $ 
            scale 0.5 0.5 $ 
            color white $ 
            text "PAUSED"
        ]
  | otherwise = blank

getGhostColor :: Ghost -> Color
getGhostColor g | ghostState g == Fleeing = blue | ghostState g == FleeingPeriodEnding = white 
    | otherwise =
        case ghostBehavior g of
            Blinky -> red
            Pinky -> rose
            Inky -> cyan
            Clyde -> orange
            Jeffrey -> green

drawLives :: Int -> Picture
drawLives amount = pictures [translate (-375 + fromIntegral i * 25) (-275) $
            color yellow $ circleSolid 8 | i <- [0 .. amount - 1]]

drawGameOver :: GameState -> Picture
drawGameOver gstate
  | lives gstate <= 0 =
      pictures[ color (makeColor 0 0 0 0.8) $ polygon [(-400, -300), (400, -300),
                     (400, 300), (-400, 300)]
        , translate (-170) 30 $ scale 0.5 0.5 $ color red $ text "GAME OVER"
        , translate (-135) (-40) $ scale 0.2 0.2 $ color white $ text "Press R to restart"
        ]

  | otherwise = blank

drawMaze :: Maze -> Picture
drawMaze board =
    pictures
        [ drawTile (tileCenter cell) tile | (cell, tile) <- allTiles board]
  where
    drawTile (x, y) tile = translate x y $
            case tile of
                Wall ->
                    pictures
                        [ color (makeColor 0.02 0.09 0.38 1) $ rectangleSolid tileSize tileSize
                        , color (makeColor 0.2 0.4 1 1) $ rectangleWire tileSize tileSize]
                Pellet -> color (makeColor 1 0.84 0.55 1) $ circleSolid 2.5
                PowerPellet -> color (makeColor 1 0.84 0.55 1) $ circleSolid 6
                Cherry -> color red $ circleSolid 5
                Freeze -> color cyan $ rectangleSolid 12 12
                Floor -> blank

drawHUD :: GameState -> Picture
drawHUD gstate =
    pictures
        [ translate (-270) 262 $
            scale 0.13 0.13 $
            color white $
            text ("LEVEL " ++ show (level gstate))
        , translate (-65) 262 $
            scale 0.13 0.13 $
            color white $
            text ("SCORE " ++ show (score gstate))]
