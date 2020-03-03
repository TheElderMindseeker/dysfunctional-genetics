module Main where

import Genetics

import Graphics.Gloss

-- Cat bodyWidth bodyHeight headRadius tailType color
data Cat = Cat Float Float Float Tail Color

data Tail = Short | Long deriving Eq

window :: Display
window = InWindow "Genetics Demo" (640, 480) (10, 10)

background :: Color
background = white

catBody :: Cat -> Picture
catBody (Cat bodyWidth bodyHeight _ _ catColor) = pictures [body, translate 0 (bodyHeight / 2) $ back]
  where
    body = pictures [color catColor $ rectangleSolid bodyWidth bodyHeight, 
      color black $ rectangleWire bodyWidth bodyHeight]
    back = pictures [color catColor $ arcSolid 0 180 (bodyWidth / 2), 
      color black $ arc 0 180 (bodyWidth / 2)]

catHead :: Cat -> Picture
catHead (Cat bodyWidth _ headRadius _ catColor) = pictures [translate 0 headRadius $ ears, head]
  where
    head = pictures [color catColor $ circleSolid headRadius, 
      color black $ thickCircle headRadius 1]
    
    -- No mirroring in gloss, this way is faster
    rightEarPath = [(0, 0), (0, 15), (15, 0), (0, 0)]
    leftEarPath = [(0, 0), (0, 15), (-15, 0), (0, 0)]

    rightEar = rotate 45 $ translate (headRadius / 2) 0 
      $ pictures [color catColor $ polygon rightEarPath, color black $ line rightEarPath]
    leftEar = rotate (-45) $ translate (-(headRadius / 2)) 0 
      $ pictures [color catColor $ polygon leftEarPath, color black $ line leftEarPath]
    ears = pictures [leftEar, rightEar]

catTail :: Cat -> Picture
catTail (Cat _ _ _ tailType catColor)
  | tailType == Short = pictures [color catColor $ rectangleSolid 35 10, color black $ rectangleWire 35 10]
  | tailType == Long = pictures [color catColor $ rectangleSolid 60 10, color black $ rectangleWire 60 10]

drawCat :: Cat -> Picture
drawCat (Cat bodyWidth bodyHeight headRadius tailType catColor) = 
  pictures [translate (bodyWidth / 2) 0 $ catTail cat,
  catBody cat, 
  translate (-bodyWidth / 2) (bodyHeight / 2) $ catHead cat]
  where
    cat = (Cat bodyWidth bodyHeight headRadius tailType catColor)

whiteCat :: Picture
whiteCat = drawCat (Cat 60 30 20 Long white)

blackCat :: Picture
blackCat = drawCat (Cat 60 30 20 Short (greyN 0.5))

main :: IO ()
main = display window background (pictures [translate (-100) 100 $ whiteCat, translate 100 100 $ blackCat])
