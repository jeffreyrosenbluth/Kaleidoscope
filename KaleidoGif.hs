{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import           Control.Monad                 (replicateM)
import           Control.Monad.Random
import           Data.Colour.Palette.ColorSet
import           Data.List                     (zipWith, zipWith3)
import           Diagrams.Prelude
import           Diagrams.Backend.Rasterific.CmdLine
import           System.Random

sizeValue :: (RandomGen g) => Rand g Double
sizeValue = getRandomR (0.05, 0.25)

coordValue :: (RandomGen g) => Rand g Double
coordValue = getRandomR (-0.5, 0.5)

confetti :: Int -> Rand StdGen (Diagram B)
confetti n = do
  ss <- replicateM n sizeValue   -- radius
  cs <- replicateM n getRandom   -- color index
  as <- replicateM n getRandom   -- opacity
  xs <- replicateM n coordValue  -- x coordinate
  ys <- replicateM n coordValue  -- y coordinate
  let mkCirc :: Double -> Int -> Double -> Diagram B
      mkCirc s c a = circle s # fc (webColors c)
                              # opacity a # lw none
      pos  = zipWith mkP2 xs ys
      conf = zipWith3 mkCirc ss cs as
  return $ atPoints pos conf

mkConfetti :: Int -> (StdGen -> Diagram B)
mkConfetti n = evalRand $ confetti n

isoceles :: (TrailLike t, V t ~ V2) => Int -> t
isoceles n = polygon
  (def & polyType   .~ PolySides [a1 @@ turn, a2 @@ turn] [1,1]
       & polyOrient .~ OrientH )
  where
  a1 = 1/2 - (1 / fromIntegral n)
  a2 = 1/2 - 1/2 * a1

mkTriangle :: Int -> Diagram B -> Diagram B
mkTriangle n = clipped tri . lw none
  where
  tri = isoceles n # rotateBy (-1/4 - 1 / (2 * fromIntegral n))

iterateIdx :: Integral i => (i -> a -> a) -> a -> [a]
iterateIdx f t = go f t 0
  where
    go f t i = let t' = f i t
               in  t': go f t' (i + 1)

-- | Creat a kaleidoscope by clipping a triangle from d, using a polygon with n
--   sides.
kaleidoscope :: Diagram B -> Int -> Double -> Diagram B
kaleidoscope d n y = mconcat . take n $ iterateIdx next tri
  where
    tri    = alignBR $ mkTriangle n (translateY (-y) d)
    next t = reflectAbout
             (0 ^& 0)
             (rotateBy (-fromIntegral t / fromIntegral n) xDir)

-- | n - pieces of confetti, r - seed to prng, poly - number of sides to polygon
--   (same as number of triangles used to make the circle), y - offset of confetti.
confettiScope :: Int -> Int -> Int -> Double -> Diagram B
confettiScope n r poly y
  = kaleidoscope (mkConfetti n (mkStdGen r)) poly y
          # centerXY <> (circle 1 # fc black)
          # frame 0.1

-- | Make a gif with n pieces of confetti, seed r, poly sides, with numFrames frames.
gif :: Int -> Int -> Int -> Int -> [(Diagram B, Int)]
gif n r poly numFrames = zip (dias ++ reverse dias) (repeat 5)
  where
    dias = map (confettiScope n r poly) [-0.5,(inc - 0.5)..0.5]
    inc = 1 / fromIntegral numFrames

main :: IO ()
main = do
  putStrLn "How may pieces of confetti d0 you want:"
  confettiStr <- getLine
  putStrLn "Enter a seed (integer) for the random number generator:"
  seedStr <- getLine
  putStrLn "How many trianges do you want to use:"
  triStr <- getLine
  putStrLn "How many frames for the GIF:"
  frameStr <- getLine
  let numConfetti = read confettiStr :: Int
      seed = read seedStr :: Int
      numTri = read triStr :: Int
      frames = read frameStr :: Int
  putStrLn "Building the GIF..."
  mainWith $ gif numConfetti seed numTri frames
