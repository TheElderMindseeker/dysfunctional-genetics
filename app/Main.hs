module Main where

import Genetics

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

data DemoState = DemoState Genotype Genotype

-- | Data structrures
-- | Description of the cats
-- Cat bodyWidth bodyHeight headRadius tailType color
data Cat = Cat Float Float Float Tail Color

-- | Tail enumeration
data Tail = Short | Long deriving Eq

-- | Required Gloss structures
-- | Window to render to
window :: Display
window = InWindow "Genetics Demo" (640, 480) (10, 10)

-- | Background color of the window
background :: Color
background = white

-- | Cat drawing functions - draws a cat with black outline
-- | Body drawing
catBody :: Cat -> Picture
catBody (Cat bodyWidth bodyHeight _ _ catColor) = pictures [body, 
  translate 0 (bodyHeight / 2) $ back]
  where
    body = pictures [color catColor $ rectangleSolid bodyWidth bodyHeight, 
      color black $ rectangleWire bodyWidth bodyHeight]
    back = pictures [color catColor $ arcSolid 0 180 (bodyWidth / 2), 
      color black $ arc 0 180 (bodyWidth / 2)]

-- | Head drawing
catHead :: Cat -> Picture
catHead (Cat bodyWidth _ headRadius _ catColor) = pictures 
  [translate 0 headRadius $ ears, head]
  where
    head = pictures [color catColor $ circleSolid headRadius, 
      color black $ thickCircle headRadius 1]
    
    -- No mirroring in gloss, so doing it this way
    rightEarPath = [(0, 0), (0, 15), (15, 0), (0, 0)]
    leftEarPath = [(0, 0), (0, 15), (-15, 0), (0, 0)]

    rightEar = rotate 45 $ translate (headRadius / 2) 0 
      $ pictures [color catColor $ polygon rightEarPath, color black $ line rightEarPath]
    leftEar = rotate (-45) $ translate (-(headRadius / 2)) 0 
      $ pictures [color catColor $ polygon leftEarPath, color black $ line leftEarPath]
    ears = pictures [leftEar, rightEar]

-- | Tail drawing
catTail :: Cat -> Picture
catTail (Cat _ _ _ tailType catColor)
  | tailType == Short = pictures [color catColor $ rectangleSolid 35 10, color black $ rectangleWire 35 10]
  | tailType == Long = pictures [color catColor $ rectangleSolid 60 10, color black $ rectangleWire 60 10]

-- | Complete cat drawing
drawCat :: Cat -> Picture
drawCat (Cat bodyWidth bodyHeight headRadius tailType catColor) = 
  pictures [translate (bodyWidth / 2) 0 $ catTail cat,
  catBody cat, 
  translate (-bodyWidth / 2) (bodyHeight / 2) $ catHead cat]
  where
    cat = (Cat bodyWidth bodyHeight headRadius tailType catColor)

-- | Mapping from genes to tail shapes
tailGene :: Gene -> Tail
tailGene gene
  | gene == Dominant || gene == Mixed = Long
  | gene == Recessive = Short

-- | Mapping from genes to cat color
colorGene :: Gene -> Color
colorGene gene
  | gene == Dominant || gene == Mixed = white
  | gene == Recessive = greyN 0.5

-- | Constructs a cat from genotype
catFromGenotype :: Genotype -> Cat
catFromGenotype genotype = (Cat 60 30 20 (tailGene (genotype!!0)) (colorGene (genotype!!1)))

-- | Interaction controls
-- | Initial interaction state
parents :: DemoState
parents = DemoState [Mixed, Mixed] [Mixed, Mixed]

-- | Helper function to draw child cats in a row
drawChildren :: [Ratio] -> Picture
drawChildren [] = blank
drawChildren (ratio:ratios) = pictures 
  [translate (-50) 100 $ drawGenotype genotype,
  drawCat (catFromGenotype genotype), 
  translate 150 0 $ drawChildren ratios]
  where
    genotype = fst ratio

-- | Helper function to display the genotype
drawGenotype :: Genotype -> Picture
drawGenotype genotype = scale 0.1 0.1 $ text (show genotype)

-- | Main drawing function - draws 2 parents and their children
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

-- | Helper function to try to update an element at a given position in a list.
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt position update list = concat [unchanged, changed]
  where
    (unchanged, toChange) = splitAt position list
    changed = 
      if null toChange then toChange
      else update (head toChange) : tail toChange

-- | Keypress handling
-- | 1-3 control tail gene of cat 1
-- | 4-6 control color gene of cat 1
-- | 7-9 control tail gene of cat 2
-- | 0, -, = control color gene of cat 2
handleDemo :: Event -> DemoState -> DemoState
handleDemo (EventKey (Char '1') _ _ _) (DemoState parent1 parent2) = (DemoState (updateAt 0 (\x -> Dominant) parent1) parent2)
handleDemo (EventKey (Char '2') _ _ _) (DemoState parent1 parent2) = (DemoState (updateAt 0 (\x -> Mixed) parent1) parent2)
handleDemo (EventKey (Char '3') _ _ _) (DemoState parent1 parent2) = (DemoState (updateAt 0 (\x -> Recessive) parent1) parent2)
handleDemo (EventKey (Char '4') _ _ _) (DemoState parent1 parent2) = (DemoState (updateAt 1 (\x -> Dominant) parent1) parent2)
handleDemo (EventKey (Char '5') _ _ _) (DemoState parent1 parent2) = (DemoState (updateAt 1 (\x -> Mixed) parent1) parent2)
handleDemo (EventKey (Char '6') _ _ _) (DemoState parent1 parent2) = (DemoState (updateAt 1 (\x -> Recessive) parent1) parent2)
handleDemo (EventKey (Char '7') _ _ _) (DemoState parent1 parent2) = (DemoState parent1 (updateAt 0 (\x -> Dominant) parent2))
handleDemo (EventKey (Char '8') _ _ _) (DemoState parent1 parent2) = (DemoState parent1 (updateAt 0 (\x -> Mixed) parent2))
handleDemo (EventKey (Char '9') _ _ _) (DemoState parent1 parent2) = (DemoState parent1 (updateAt 0 (\x -> Recessive) parent2))
handleDemo (EventKey (Char '0') _ _ _) (DemoState parent1 parent2) = (DemoState parent1 (updateAt 1 (\x -> Dominant) parent2))
handleDemo (EventKey (Char '-') _ _ _) (DemoState parent1 parent2) = (DemoState parent1 (updateAt 1 (\x -> Mixed) parent2))
handleDemo (EventKey (Char '=') _ _ _) (DemoState parent1 parent2) = (DemoState parent1 (updateAt 1 (\x -> Recessive) parent2))
handleDemo event state = state

-- | Update function
updateDemo :: Float -> DemoState -> DemoState
updateDemo delta state = state


-- | Main application
main :: IO ()
main = play window background 60 parents drawDemo handleDemo updateDemo