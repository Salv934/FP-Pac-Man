-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures
	[ pictures (map drawGhost (ghosts gstate))
  , drawPacman (pacman gstate)
  , drawInfo (infoToShow gstate)
  , drawPausedOnScreen gstate
  , drawLives (lives gstate)
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
	 color yellow (circleSolid 15)

drawGhost :: Ghost -> Picture
drawGhost g =
  let (x, y) = ghostPos g
  in translate x y $
	 color (getGhostColor g) $
		polygon [(-15, -12), (15, -12), (0, 16)]

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
