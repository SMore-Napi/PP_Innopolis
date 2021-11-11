-- Lab 5
-- Roman Soldatov B19-SD-01

{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
-- | N-body simulation.
module Main where

import CodeWorld

main :: IO ()
main = drawingOf (renderSystem earthMoonSun)

sun :: Body
sun = Body 1.98892e30 (0, 0) (4.2, 5.3)

earth :: Body
earth = Body 5.9742e24 (5, 5) (4.2, 5.3)

moon :: Body
moon = Body 7.36e22 (6, 5) (4.2, 5.3)

earthMoon :: System
earthMoon = System [earth, moon]

earthSun :: System
earthSun = System [earth, sun]

earthMoonSun :: System
earthMoonSun = System [earth, moon, sun]

-- * Types

-- | Mass (in kilograms).
type Mass = Double

-- | Position (coordinates in meters).
type Position = (Double, Double)

-- | Velocity vector (components in meters per second).
type Velocity = (Double, Double)

type Acceleration = (Double, Double)

-- | An astronomical body (e.g. a planet or an asteroid).
data Body = Body Mass Position Velocity

-- | A system of bodies.
data System = System [Body]

-- * Rendering

-- | Render a single body, taking visualisation constants into account.
renderBody :: Body -> Picture
renderBody (Body m (x, y) _) = colored white (translated x y (solidCircle radius))
  where radius = massToRadius m

-- | Render a system of bodies on a black background.

renderSystem :: System -> Picture
renderSystem (System []) = background
  where
    background = solidRectangle 100 100
renderSystem (System (first_body:rest_bodies))
    = renderBody first_body <> renderSystem (System rest_bodies)

-- * Physics (updates)

-- | Update body's position based on its velocity.
moveBody :: Double -> Body -> Body
moveBody dt (Body m (x, y) (velx, vely))
  = Body m newpos (velx, vely)
  where
    newx = x + velx * dt
    newy = y + vely * dt
    newpos = (newx, newy)

-- | Update body's position and velocity.
updateBody :: Double -> [Body] -> Body -> Body
updateBody dt bodies = moveBody dt . applyGravity dt bodies

-- | Update all bodies in a system.
updateBodies :: Double -> [Body] -> [Body]
updateBodies dt bodies
  = map (moveBody dt . applyGravity dt bodies) bodies

-- | Update entire system, taking 'timeScale' into account.
updateSystem :: Double -> System -> System
updateSystem dt (System bodies) = System (updateBodies scaledTime bodies)
  where
    scaledTime = dt * timeScale

-- ** Gravity helpers

-- | Acceleration in a two body system.
--
-- NOTE: ignores gravitional effect of "small" objects

gravityAcc
  :: Body     -- ^ Body whose gravitational field is used.
  -> Body     -- ^ Body to compute acceleration for.
  -> Acceleration
gravityAcc (Body m1 (x1, y1) _) (Body _ (x2, y2) _)
  = if m1 >= smallMassThreshold
  then (ax, ay)
  else (0, 0)
    where
      rx = x1 - x2
      ry = y1 - y2
      ax = bigG * m1 / (rx * rx)
      ay = bigG * m1 / (ry * ry)

  -- Hint: use vectorLength, vectorDifference and scaledVector

applyAcceleration :: Acceleration -> Double -> Body -> Velocity
applyAcceleration (ax, ay) dt (Body _ _ (vx, vy)) = newVelocity
  where
    newVX = vx + ax * dt
    newVY = vy + ay * dt
    newVelocity = (newVX, newVY)

sumVelocities :: Velocity -> Velocity -> Velocity
sumVelocities (vx1, vy1) (vx2, vy2) = (vx1 + vx2, vy1 + vy2)

applyAccelerations :: Double -> [Body] -> Body -> Velocity
applyAccelerations _ [] _ = (0, 0)
applyAccelerations dt (headbody:bodies) body
  = sumVelocities newVelocity (applyAccelerations dt bodies body)
  where
    newVelocity = applyAcceleration (gravityAcc headbody body) dt body

-- | Compute and apply acceleration to update body's velocity.
applyGravity :: Double -> [Body] -> Body -> Body
applyGravity dt bodies (Body m pos (vx, vy)) = Body m pos newVelocity
  where
    newVelocity = applyAccelerations dt bodies (Body m pos (vx, vy))
-- Hint: split into smaller parts, and use vectorSum

-- * Controls

-- | Handle user input (e.g. mouse clicks).
handleSystem :: Event -> System -> System
handleSystem _ = id -- ignore all events

-- * Helpers

-- | Convert pointer position into coordinates in a simulation.
fromPointer :: Point -> Point
fromPointer (x, y) = (x * viewportScale, y * viewportScale)

-- * Constants

-- ** Physical constants

-- | One astronomical unit (in meters).
au :: Double
au = 149597900000

-- | Gravitational constant.
bigG :: Double
bigG = 6.67430e-11

-- ** Visualisation parameters

-- | Viewport scaling factor.
--
-- For inner solar system: 1 unit = 0.2 'au'.
-- For Earth-Moon: 1 unit = 0.0005 'au'.
viewportScale :: Double
viewportScale = 0.2 * au

-- | Time scale for the simulation.
--
-- For inner solar system: 1 second = 1 week
-- For Earth-Moon: 1 second = 1 day
timeScale :: Double
timeScale = 604800

-- | Mass to visualisation radius mapping.
-- For nicer visualisation we use logarithmic scale.
massToRadius :: Mass -> Double
massToRadius m = 0.01 + 3e-7 * logBase 10 (m + 10) ^ 4

-- | Smallest mass to take gravity into account for.
smallMassThreshold :: Mass
smallMassThreshold = 1e21
