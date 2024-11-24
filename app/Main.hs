{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.Environment
import System.Random

windowWidth, windowHeight, framesPerSecond :: Int
windowWidth = 600
windowHeight = 600
framesPerSecond = 120

boidSize, boidSpeed, boidAcceleration, boidRange, boidView :: Float
boidSize = 10
boidSpeed = 200
boidAcceleration = 200
boidRange = 50
boidView = 270

main :: IO ()
main = do
  (screenWidth, screenHeight) <- getScreenSize
  let windowLeft = div (screenWidth - windowWidth) 2
  let windowTop  = div (screenHeight - windowHeight) 2
  let window = InWindow "Boids" (windowWidth, windowHeight) (windowLeft, windowTop)
  world <- initialWorld
  play window black framesPerSecond world render handleInput step

data Vec2 = Vec2 Float Float

vzero :: Vec2
vzero = Vec2 0 0

vmap :: (Float -> Float) -> Vec2 -> Vec2
vmap f (Vec2 x y) = Vec2 (f x) (f y)

vmap2 :: (Float -> Float -> Float) -> Vec2 -> Vec2 -> Vec2
vmap2 f (Vec2 x y) (Vec2 x' y') = Vec2 (f x x') (f y y')

vapply :: (Float -> Float) -> (Float -> Float) -> Vec2 -> Vec2
vapply f g (Vec2 x y) = Vec2 (f x) (g y)

vaverage :: [Vec2] -> Vec2
vaverage xs = (1 / (fromIntegral $ length xs)) *^ (sum xs)

instance Num Vec2 where
  (+) = vmap2 (+)
  (-) = vmap2 (-)
  (*) = vmap2 (*)
  abs = vmap abs
  signum = vmap signum
  fromInteger x = Vec2 (fromInteger x) (fromInteger x)

(*^) :: Float -> Vec2 -> Vec2
(*^) c = vmap (*c)

angle :: Vec2 -> Float
angle (Vec2 x y) = atan2 y x

norm :: Vec2 -> Float
norm (Vec2 x y) = sqrt $ x ** 2 + y ** 2

normalize :: Vec2 -> Vec2
normalize v = if (norm v) < 1e-6 then v else vmap (/ (norm v)) v

data Boid = Boid
  { position :: Vec2
  , velocity :: Vec2
  , acceleration :: Vec2
  }

data World = World
  { boids :: [Boid]
  , rng :: StdGen
  }

initialWorld :: IO World
initialWorld =  do
  rng <- initStdGen
  return World { boids = [], rng }

withBoids :: [Boid] -> World -> World
withBoids boids' world = World { boids = boids', rng = rng world }

render :: World -> Picture
render = Pictures . (map renderBoid) . boids

renderBoid :: Boid -> Picture
renderBoid boid =
  let (Vec2 x y) = position boid in
  Color white $
  Translate x y $
  Scale boidSize boidSize $
  Rotate (-(angle $ velocity boid) * 180 / pi) $
  Polygon [(0, -1), (2, 0), (0, 1)]

handleInput :: Event -> World -> World
handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) world =
  World { boids = boid : (boids world), rng = rng' }
  where (boid, rng') = spawnBoid (rng world) x y
handleInput _ world = world

spawnBoid :: StdGen -> Float -> Float -> (Boid, StdGen)
spawnBoid rng' x y = (Boid (Vec2 x y) (boidSpeed *^ (Vec2 (cos angle') (sin angle'))) vzero, rng'')
                    where (angle', rng'') = uniformR (0, 2 * pi) rng'

step :: Float -> World -> World
step dt world = withBoids (map (\boid -> stepBoid dt (close boid) boid) $ boids world) world
  where close boid = filter (isClose boid) $ boids world
        isClose boid boid' = let delta = position boid - position boid' in
          norm delta < boidRange && abs ((angle delta) - (angle $ velocity boid)) < boidView / 2

stepBoid :: Float -> [Boid] -> Boid -> Boid
stepBoid dt neighbors boid = if null neighbors then boid else  Boid
  (vapply (clamp (-halfWindowWidth) halfWindowWidth) (clamp (-halfWindowHeight) halfWindowHeight) newPosition)
  (if speed > boidSpeed then ((boidSpeed / speed) *^ newVelocity) else newVelocity)
  $ boidAcceleration *^ (normalize $ alignment + cohesion + seperation)
  where
    halfWindowWidth = fromIntegral $ div windowWidth 2
    halfWindowHeight = fromIntegral $ div windowHeight 2
    clamp start end x
      | x < start = end - (start - x)
      | x > end = start + x - end
      | otherwise = x
    newPosition = (position boid + dt *^ velocity boid)
    speed = norm newVelocity
    newVelocity = (velocity boid + dt *^ acceleration boid)
    alignment = normalize $ averageHeading - velocity boid
    averageHeading = vaverage $ map velocity neighbors
    cohesion = normalize $ averagePosition - position boid
    averagePosition = vaverage $ map position neighbors
    seperation = normalize $ averageDisplacement
    averageDisplacement = vaverage $ map displacement neighbors
    displacement boid' = let delta = position boid - position boid' in if norm delta > 0 then (1 / norm delta) *^ delta else 0
