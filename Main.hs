{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
--------------------------------------------------------------------------------
-- Haskell asteroid game
--------------------------------------------------------------------------------
module Main where


import Data.IORef
import Data.Time
import Debug.Trace
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
                     deriving (Show)


instance Eq Bullet where
  (Bullet ( x, y ) r) == (Bullet ( x', y' ) r' )
    = abs (x - x') <= 1 && abs (y - y') <= 1 && abs (r - r') <= 1


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
                   , sScore     :: IORef Int
                   }


--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------
color4f :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
color4f r g b a
  = color $ Color4 r g b a


vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3f x y z
  = vertex $ Vertex3 x y z


wrap :: GLfloat -> GLfloat -> GLfloat
wrap x m
  | x < 0 = m
  | x > m = 0
  | otherwise = x


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

  clear [ ColorBuffer ]

  matrixMode $= Projection
  loadIdentity
  ortho 0.0 (realToFrac w) (realToFrac h) 0.0 (-1.0) 1.0

  matrixMode $= Modelview 0
  loadIdentity

  -- Render stuff
  mapM_ renderAsteroid asteroids
  mapM_ renderBullet bullets

  translate $ Vector3 x y 0.0

  -- Render player
  rotate rot $ Vector3 0 0 1
  renderPrimitive LineLoop $ do
    color4f 1 1 1 1
    vertex3f (-5) (-5) ( 0)
    vertex3f (10) ( 0) ( 0)
    vertex3f (-5) ( 5) ( 0)

  flush


renderAsteroid :: Asteroid -> IO ()
renderAsteroid (Asteroid s r ( x, y ) _) 
  = preservingMatrix $ do
      translate $ Vector3 x y 0
      rotate r $ Vector3 0 0 1
      scale s s s
      color4f 1 0 0 1

      renderPrimitive LineLoop $ do
        vertex3f 1.0      1.0  0.0
        vertex3f 0.1    (-0.4) 0.0
        vertex3f 1.0    (-0.5) 0.0
        vertex3f (-0.4) (-1.0) 0.0
        vertex3f (-1.0) (-0.2) 0.0
        vertex3f (-0.8)   0.2  0.0
        vertex3f (-0.4)   0.1  0.0
        vertex3f (-0.1)   1.0  0.0


renderBullet :: Bullet -> IO ()
renderBullet (Bullet ( x, y) r)
  = preservingMatrix $ do
      translate $ Vector3 x y 0
      rotate r $ Vector3 0 0 1
      color4f 0 1 0 1

      renderPrimitive LineLoop $ do
        vertex3f (-2) (-2) ( 0)
        vertex3f ( 2) ( 0) ( 0)
        vertex3f (-2) ( 2) ( 0)


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

  -- Compute the time elapsed since the last frame
  thisFrame <- getCurrentTime
  lastFrame <- get sLastFrame
  sLastFrame $= thisFrame

  let dt = 500 * (realToFrac $ diffUTCTime thisFrame lastFrame)
  
  -- Update entites
  let ( asteroids', bullets' ) = update w h dt asteroids bullets

  sBullets   $= bullets'
  sAsteroids $= map (updateAsteroid dt w h) asteroids'

  let asteroidCount = length asteroids'

  when (asteroidCount < count) $
    replicateM_ (count - asteroidCount) (genAsteroid state)
    
  -- Update rotation
  when rotLeft $ sRot $~! (+ dt)
  when rotRight $ sRot $~! (+ (-dt))
  rot <- get sRot

  when moving $ do
    let [ dx, dy ] = [ cos, sin ] <*> pure (rot * pi / 180)
    sPos $= ( wrap (x + dx * dt * 0.5) w, wrap (y + dy * dt * 0.5) h )

  postRedisplay Nothing


update :: GLfloat 
       -> GLfloat 
       -> GLfloat 
       -> Asteroids 
       -> Bullets 
       -> ( Asteroids, Bullets )
update w h dt as
  = foldr check ( as, [ ] )
  where
    check (Bullet ( x, y ) r ) ( as, bs )
      | outside || any hitA as = ( newAsteroids, bs )
      | otherwise             = ( as, newBullet : bs ) 
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
        hitA Asteroid { aSize = s, aPos = ( x', y' ) }
          = abs (x - x') <= s && abs (y - y') <= s

        -- Updated asteroid list
        newAsteroids 
          = concatMap split as
          where
            split a@Asteroid { aSize = s, aPos = ( x', y' ), aVel = ( vx, vy ) }
              | collides && s < 10 = [ ]
              | collides && s > 10 = [ shard 0.2, shard (-0.2) ]
              | otherwise = [ a ]
              where
                angle = atan2 vy vx
                speed = sqrt (vx * vx + vy * vy)

                shard f
                  = a { aSize = s / 2, aVel = ( speed * cos (angle + f)
                                              , speed * sin (angle + f) )}

                collides
                  = abs (x - x') <= s && abs (y - y') <= s


updateAsteroid :: GLfloat -> GLfloat -> GLfloat -> Asteroid -> Asteroid
updateAsteroid dt w h a@Asteroid { aSize = s, aPos = ( x, y ), aVel = ( vx, vy ) }
  = a { aPos = ( wrap (x + vx * dt) w, wrap (y + vy * dt) h ) }


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
                   ( -vx / 5.0, -vy / 5.0 )


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
keyboardUp _state _char _pos
  = postRedisplay Nothing


--------------------------------------------------------------------------------
-- Setup
--------------------------------------------------------------------------------
main :: IO ()
main = do  
  ( _pname, _args ) <- getArgsAndInitialize
  _window           <- createWindow "Haskell Asteroids"


  state <- State <$> newIORef ( 800, 600 )
                 <*> newIORef ( 400, 300 )
                 <*> newIORef False
                 <*> newIORef 0
                 <*> newIORef False
                 <*> newIORef False
                 <*> newIORef []
                 <*> newIORef []
                 <*> newIORef 20
                 <*> (getCurrentTime >>= newIORef)
                 <*> newIORef 0


  globalKeyRepeat $= GlobalKeyRepeatOff


  displayCallback       $= render state
  idleCallback          $= Just (idle state)
  reshapeCallback       $= Just (reshape state)
  keyboardCallback      $= Just (keyboardDown state)
  keyboardUpCallback    $= Just (keyboardUp state)

  mainLoop
