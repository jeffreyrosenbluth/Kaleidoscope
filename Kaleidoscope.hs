{-# LANGUAGE NoMonomorphismRestriction #-}

import           Control.Monad                 (replicateM)
import           Control.Monad.Random
import           Data.Colour.Palette.ColorSet
import           Data.List                     (zipWith, zipWith3)
import           Diagrams.Prelude
import           Diagrams.Backend.Rasterific.CmdLine
import           System.Random

type Dia = Diagram B R2

iterateIdx :: Integral i => (i -> a -> a) -> a -> [a]
iterateIdx f t = go f t 0
  where
    go f t i = let t' = f i t in t' : go f t' (i + 1)

kaleidoscope :: Dia -> Double -> Dia
kaleidoscope d y = appends hex hexs
  where
    hexs   = zip dirs (replicate 6 hex)
    dirs   = iterate (rotateBy (1/6)) (rotateBy (1/12) unitX)
    hex    = mconcat . take 6 $ iterateIdx next tri
    tri    = alignBR $ mkTriangle (translateY (-y) d)
    next i = reflectAbout (0 ^& 0) (rotateBy (- fromIntegral i / 6) unitX)

mkTriangle :: Dia -> Dia
mkTriangle = clipped (triangle 1) # lw none

confettiScope :: Int -> Int -> Double -> Dia
confettiScope n r y
  = kaleidoscope (mkConfetti n (mkStdGen r)) y
          # centerXY <> (circle 2.75 # fc black)
          # pad 1.1

sizeValue :: (RandomGen g) => Rand g Double
sizeValue = getRandomR (0.05, 0.25)

coordValue :: (RandomGen g) => Rand g Double
coordValue = getRandomR (-0.5, 0.5)

confetti :: Int -> Rand StdGen Dia
confetti n = do
  ss <- replicateM n sizeValue   -- radius
  cs <- replicateM n getRandom   -- color index
  as <- replicateM n getRandom   -- opacity
  xs <- replicateM n coordValue  -- x coordinate
  ys <- replicateM n coordValue  -- y coordinate
  let mkCirc :: Double -> Int -> Double -> Dia
      mkCirc s c a = circle s # fc (webColors c) # opacity a
      pos = zipWith mkP2 xs ys
      conf = zipWith3 mkCirc ss cs as
  return $ position (zip pos conf)

mkConfetti :: Int -> (StdGen -> Dia)
mkConfetti n = evalRand $ confetti n

main = mainWith $ confettiScope 50 0 0

dias = map (confettiScope 50 0) [-0.5,-0.49..0.5]

gif :: [(Diagram B R2, Int)]
gif = zip (dias ++ reverse dias) (repeat 5)
