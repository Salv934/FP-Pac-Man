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

getGhostColor :: Ghost -> Color
getGhostColor g | ghostState g == Fleeing = blue | ghostState g == FleeingPeriodEnding = white 
    | otherwise =
        case ghostBehavior g of
            Blinky -> red
            Pinky -> rose
            Inky -> cyan
            Clyde -> orange
            Jeffrey -> green
