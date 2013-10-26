{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
--------------------------------------------------------------------------------
-- Haskell asteroid game
--------------------------------------------------------------------------------
module Main where


import Data.IORef
import Data.List
import Data.Time
import Data.Fixed
import Control.Monad
import Control.Applicative
import System.Random
import Graphics.UI.GLUT


--------------------------------------------------------------------------------
-- Game state & objects
--------------------------------------------------------------------------------
type Asteroids = [ Asteroid ]
type Bullets = [ Bullet ]
type Vec2f = ( GLfloat, GLfloat )
type Collision = ( Asteroid, Bullet )


data Bullet = Bullet { bPos :: Vec2f
                     , bRot :: GLfloat
                     }
                     deriving (Eq, Show)


data Asteroid = Asteroid { aSize :: GLfloat
                         , aRot  :: GLfloat
                         , aPos  :: Vec2f
                         , aVel  :: Vec2f
                         }
                         deriving (Eq, Show)


data State = State { sSize      :: IORef Vec2f
                   , sPos       :: IORef Vec2f
                   , sMoving    :: IORef Bool
                   , sRot       :: IORef GLfloat
                   , sRotLeft   :: IORef Bool
                   , sRotRight  :: IORef Bool
                   , sAsteroids :: IORef [ Asteroid ]
                   , sBullets   :: IORef [ Bullet ]
                   , sCount     :: IORef Int
                   , sLastFrame :: IORef UTCTime
                   , sScore     :: IORef GLfloat
                   , sLives     :: IORef Int
                   }


--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------
color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b
  = color $ Color3 r g b


vertex2f :: GLfloat -> GLfloat -> IO ()
vertex2f x y
  = vertex $ Vertex2 x y


rasterPos2f :: GLfloat -> GLfloat -> IO ()
rasterPos2f x y
  = rasterPos $ Vertex2 x y


clamp ::GLfloat -> GLfloat -> GLfloat -> GLfloat
clamp x a b
  = max a (min x b)
  

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------
render :: State -> IO ()
render State {..} = do
  ( w, h )  <- get sSize
  ( x, y )  <- get sPos
  rot       <- get sRot
  asteroids <- get sAsteroids
  bullets   <- get sBullets
  score     <- get sScore
  lives     <- get sLives

  clear [ ColorBuffer ]

  -- Set up matrices
  matrixMode $= Projection
  loadIdentity
  ortho 0.0 (realToFrac w) (realToFrac h) 0.0 (-1.0) 1.0
  matrixMode $= Modelview 0
  loadIdentity

  -- Render stuff
  mapM_ renderAsteroid asteroids
  mapM_ renderBullet bullets

  -- Render the player
  preservingMatrix $ do
    translate $ Vector3 x y 0.0
    rotate rot $ Vector3 0 0 1
    color3f 1 1 1

    renderPrimitive LineLoop $ do
      vertex2f (-5) (-5)
      vertex2f (10) ( 0)
      vertex2f (-5) ( 5)


  -- Score
  rasterPos2f 5 15
  renderString Fixed8By13 $ show (floor score)

  -- Game over message
  when (lives <= 0) $ do
    rasterPos2f (w / 2 - 25) (h / 2 - 20)
    renderString Fixed8By13 $ "Score: " ++ show score
    rasterPos2f (w / 2 - 100) (h / 2)
    renderString Fixed8By13 $ "Game over! Press X to restart"

  -- Health
  mapM_ (renderHealth . fromIntegral) [1..lives]


  flush


renderAsteroid :: Asteroid -> IO ()
renderAsteroid Asteroid { aSize, aRot, aPos = ( x, y )}
  = preservingMatrix $ do
      translate $ Vector3 x y 0
      rotate aRot $ Vector3 0 0 1
      scale aSize aSize aSize
      color3f 1 1 1

      renderPrimitive LineLoop $ do
        vertex2f   1.0    1.0 
        vertex2f   0.1  (-0.4)
        vertex2f   1.0  (-0.5)
        vertex2f (-0.4) (-1.0)
        vertex2f (-1.0) (-0.2)
        vertex2f (-0.8)   0.2 
        vertex2f (-0.4)   0.1 
        vertex2f (-0.1)   1.0 


renderBullet :: Bullet -> IO ()
renderBullet Bullet { bPos = ( x, y ), bRot}
  = preservingMatrix $ do
      translate $ Vector3 x y 0
      rotate bRot $ Vector3 0 0 1
      color3f 1 0 0

      renderPrimitive LineLoop $ do
        vertex2f (-2) (-2)
        vertex2f ( 2) ( 0)
        vertex2f (-2) ( 2)


renderHealth :: GLfloat -> IO ()
renderHealth count
  = preservingMatrix $ do
      translate $ Vector3 (50 + count * 10) 10 0
      renderPrimitive Quads $ do
        vertex2f 0 0
        vertex2f 0 5
        vertex2f 5 5
        vertex2f 5 0

--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------
idle :: State -> IO ()
idle state@State {..} = do
  -- Retrieve stuff from the state
  ( w, h )  <- get sSize
  ( x, y )  <- get sPos
  rotLeft   <- get sRotLeft
  rotRight  <- get sRotRight
  asteroids <- get sAsteroids
  bullets   <- get sBullets  
  count     <- get sCount
  moving    <- get sMoving
  lives     <- get sLives

  -- Don't do anything if we're not playing
  when (lives > 0) $ do
    -- Compute the time elapsed since the last frame
    thisFrame <- getCurrentTime
    lastFrame <- get sLastFrame
    sLastFrame $= thisFrame

    let dt = 500 * (realToFrac $ diffUTCTime thisFrame lastFrame)
    
    -- Update entites
    let ( asteroids', bullets' ) = update w h dt asteroids bullets

    sBullets   $= bullets'
    sAsteroids $= map (moveAsteroid w h dt) asteroids'

    -- Cound the score
    sScore $~! (+) (foldl (\a x-> a + score x) 0 (asteroids \\ asteroids'))

    -- Check if player was hit
    when (any (hit x y) asteroids') $ do
      sLives     $~! (+(-1))
      sPos       $= ( w / 2, h / 2 )
      sAsteroids $= [ ]
      sBullets   $= [ ]

    -- Generate new asteroids
    let asteroidCount = length asteroids'
    when (asteroidCount < count) $
      replicateM_ (count - asteroidCount) (genAsteroid state)
      
    -- Update rotation
    when rotLeft $ sRot $~! (+ dt)
    when rotRight $ sRot $~! (+ (-dt))
    rot <- get sRot

    -- Update movement
    when moving $ do
      let [ dx, dy ] = [ cos, sin ] <*> pure (rot * pi / 180)
      sPos $= ( (x + dx * dt * 0.5) `mod'` w, (y + dy * dt * 0.5) `mod'` h )

  postRedisplay Nothing


hit :: GLfloat -> GLfloat -> Asteroid -> Bool
hit x y Asteroid { aSize, aPos = ( x', y' )}
  = abs (x - x') < aSize && abs (y - y') < aSize


-- Computes the score given for destroying an asteroid
score :: Asteroid -> GLfloat
score (Asteroid s r ( x, y ) ( vx, vy ))
  = vel * 100
  where
    vel = sqrt (vx * vx + vy * vy)


-- Updates the position of an asteroid
moveAsteroid :: GLfloat -> GLfloat -> GLfloat ->Asteroid -> Asteroid
moveAsteroid w h dt a@(Asteroid s r ( x, y ) ( vx, vy ))
  = a { aPos = ( (x + vx * dt) `mod'` w, (y + vy * dt) `mod'` h ) }
    

update :: GLfloat 
       -> GLfloat
       -> GLfloat
       -> Asteroids   
       -> Bullets 
       -> ( Asteroids, Bullets )
update w h dt as
  = foldr check ( as, [ ] )
  where  
    -- Check for collisions between bullets and asteroids
    check (Bullet ( x, y ) r ) ( as, bs )
      | outside || any hitA as = ( concatMap split as, bs )
      | otherwise              = ( as, newBullet : bs ) 
      where
        -- Check if the bullet is outside the screen
        outside
          = x < -w || w < x || y < -h || h < y

        -- Bullet with updated position
        newBullet
          = Bullet ( x + dx * dt, y + dy * dt ) r
          where
            [ dx, dy ] = [ cos, sin ] <*> pure (r * pi / 180)

        -- Check if the bullet hit an asteroid
        hitA Asteroid { aSize, aPos = ( x', y' ) }
          = abs (x - x') <= aSize && abs (y - y') <= aSize

        -- Splits an asteroid in two   
        split a@(Asteroid s r ( x', y' ) ( vx, vy ))
          | hit x y a && s < 10 = [ ]
          | hit x y a && s > 10 = [ shard 0.2, shard (-0.2) ]
          | otherwise = [ a ]
          where
            angle = atan2 vy vx
            speed = sqrt (vx * vx + vy * vy) * 1.5

            shard f
              = Asteroid (s / 2) r ( x', y') ( vx', vy' )
              where
                vx' = speed * cos (angle + f)
                vy' = speed * sin (angle + f)


genAsteroid :: State -> IO ()
genAsteroid State { sSize, sAsteroids } = do
  ( w, h )  <- get sSize
  asteroids <- get sAsteroids

  size <- randomIO :: IO GLfloat
  angle <- randomIO :: IO GLfloat
  rot <- randomIO :: IO GLfloat

  let vx = cos (angle * 2 * pi)
      vy = sin (angle * 2 * pi)
      x = 100
      y = 100
      a = Asteroid (10.0 + 30.0 * size) 
                   rot
                   ( clamp x 0 w, clamp y 0 h ) 
                   ( -vx / 10.0, -vy / 10.0 )

  sAsteroids $= a : asteroids


reshape :: State -> Size -> IO ()
reshape State { sSize, sPos } (Size w h) = do
  let w'  = fromIntegral w
      h' = fromIntegral h

  sSize    $= ( w', h' )
  sPos     $= ( w' / 2, h' / 2 )
  viewport $= (Position 0 0, Size w h)

  postRedisplay Nothing


keyboardDown :: State -> Char -> Position -> IO ()
keyboardDown State { sMoving } 'w' _pos
  = sMoving $= True
keyboardDown State { sRotRight } 'd' _pos
  = sRotRight $= True
keyboardDown State { sRotLeft } 'a' _pos
  = sRotLeft $= True
keyboardDown _state _char _pos
  = postRedisplay Nothing


keyboardUp :: State -> Char -> Position -> IO ()
keyboardUp State { sMoving } 'w' _pos
  = sMoving $= False
keyboardUp State { sRotRight } 'd' _pos
  = sRotRight $= False
keyboardUp State { sRotLeft } 'a' _pos
  = sRotLeft $= False
keyboardUp State { sBullets, sRot, sPos } ' ' _pos = do
  bullets  <- get sBullets  
  rot      <- get sRot
  ( x, y ) <- get sPos
  sBullets $= (Bullet ( x, y ) rot : bullets)
keyboardUp State {..} 'x' _pos = do
  lives <- get sLives
  when (lives <= 0) $ do
    sLives $= 3
    sScore $= 0
keyboardUp _state _char _pos
  = postRedisplay Nothing


--------------------------------------------------------------------------------
-- Setup
--------------------------------------------------------------------------------
main :: IO ()
main = do  
  ( _pname, _args ) <- getArgsAndInitialize
  _window           <- createWindow "Haskell Asteroids"

  globalKeyRepeat $= GlobalKeyRepeatOff

  state <- State <$> newIORef ( 800, 600 )
                 <*> newIORef ( 400, 300 )
                 <*> newIORef False
                 <*> newIORef 0
                 <*> newIORef False
                 <*> newIORef False
                 <*> newIORef [ ]
                 <*> newIORef [ ]
                 <*> newIORef 5
                 <*> (getCurrentTime >>= newIORef)
                 <*> newIORef 0
                 <*> newIORef 3

  displayCallback    $= render state
  idleCallback       $= Just (idle state)
  reshapeCallback    $= Just (reshape state)
  keyboardCallback   $= Just (keyboardDown state)
  keyboardUpCallback $= Just (keyboardUp state)

  mainLoop
