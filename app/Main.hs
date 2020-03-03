module Main where

import Genetics

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

data DemoState = DemoState Genotype Genotype

-- Cat bodyWidth bodyHeight headRadius tailType color cat genotype
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

tailGene :: Gene -> Tail
tailGene gene
  | gene == Dominant || gene == Mixed = Long
  | gene == Recessive = Short

colorGene :: Gene -> Color
colorGene gene
  | gene == Dominant || gene == Mixed = white
  | gene == Recessive = greyN 0.5

catFromGenotype :: Genotype -> Cat
catFromGenotype genotype = (Cat 60 30 20 (tailGene (genotype!!0)) (colorGene (genotype!!1)))

parents :: DemoState
parents = DemoState [Mixed, Mixed] [Mixed, Mixed]

drawChildren :: [Ratio] -> Picture
drawChildren [] = blank
drawChildren (ratio:ratios) = pictures 
  [translate (-50) 100 $ drawGenotype genotype,
  drawCat (catFromGenotype genotype), 
  translate 150 0 $ drawChildren ratios]
  where
    genotype = fst ratio

drawGenotype :: Genotype -> Picture
drawGenotype genotype = scale 0.1 0.1 $ text (show genotype)

drawDemo :: DemoState -> Picture
drawDemo (DemoState parent1 parent2) = pictures 
  [translate (-150) 200 $ drawGenotype parent1,
  translate (-100) 100 $ drawCat cat1,
  translate 50 200 $ drawGenotype parent2, 
  translate 100 100 $ drawCat cat2, 
  translate childrenOffset (-200) $ drawChildren children]
  where
    cat1 = catFromGenotype parent1
    cat2 = catFromGenotype parent2
    children = firstGeneration parent1 parent2
    childrenOffset = fromIntegral ((-100) * (((length children) + 4) `div` 2))

-- | Try update an element at a given position in a list.
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt position update list = concat [unchanged, changed]
  where
    (unchanged, toChange) = splitAt position list
    changed = 
      if null toChange then toChange
      else update (head toChange) : tail toChange

handleDemo :: Event -> DemoState -> DemoState
handleDemo (EventKey (Char '1') _ _ _) (DemoState parent1 parent2) = (DemoState (updateAt 0 (\x -> Dominant) parent1) parent2)
handleDemo (EventKey (Char '2') _ _ _) (DemoState parent1 parent2) = (DemoState (updateAt 0 (\x -> Mixed) parent1) parent2)
handleDemo (EventKey (Char '3') _ _ _) (DemoState parent1 parent2) = (DemoState (updateAt 0 (\x -> Recessive) parent1) parent2)
handleDemo (EventKey (Char '4') _ _ _) (DemoState parent1 parent2) = (DemoState (updateAt 1 (\x -> Dominant) parent1) parent2)
handleDemo (EventKey (Char '5') _ _ _) (DemoState parent1 parent2) = (DemoState (updateAt 2 (\x -> Mixed) parent1) parent2)
handleDemo (EventKey (Char '6') _ _ _) (DemoState parent1 parent2) = (DemoState (updateAt 3 (\x -> Recessive) parent1) parent2)
handleDemo (EventKey (Char '7') _ _ _) (DemoState parent1 parent2) = (DemoState parent1 (updateAt 0 (\x -> Dominant) parent2))
handleDemo (EventKey (Char '8') _ _ _) (DemoState parent1 parent2) = (DemoState parent1 (updateAt 0 (\x -> Mixed) parent2))
handleDemo (EventKey (Char '9') _ _ _) (DemoState parent1 parent2) = (DemoState parent1 (updateAt 0 (\x -> Recessive) parent2))
handleDemo (EventKey (Char '0') _ _ _) (DemoState parent1 parent2) = (DemoState parent1 (updateAt 1 (\x -> Dominant) parent2))
handleDemo (EventKey (Char '-') _ _ _) (DemoState parent1 parent2) = (DemoState parent1 (updateAt 2 (\x -> Mixed) parent2))
handleDemo (EventKey (Char '=') _ _ _) (DemoState parent1 parent2) = (DemoState parent1 (updateAt 3 (\x -> Recessive) parent2))
handleDemo event state = state

updateDemo :: Float -> DemoState -> DemoState
updateDemo delta state = state


main :: IO ()
main = play window background 60 parents drawDemo handleDemo updateDemo