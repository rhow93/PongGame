module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

drawing :: Picture
drawing = pictures [ball, walls, 
                    mkPaddle rose 120 (-20),
                    mkPaddle orange (-120) 40]
  where
    -- | the pong ball
    ball = translate (-10) 40 $ color ballColor $ circleSolid 10
    ballColor = dark red
    
    -- | The bottom and top walls
    wall :: Float -> Picture
    wall offset = 
      translate 0 offset $
        color wallColor $
          rectangleSolid 270 10
          
    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]
    
    
    -- | Makes a paddle of a given border and vertical offset
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]
      
    paddleColor = light (light blue)
    

-- | A data structure to hold the state of the Pong game
-- This allows us to easily update the game state without worrying
-- how each piece is drawn. We can summarise the game state by
-- the pong ball location, it's velocity and the locations of the paddles.
data PongGame = Game 
  { ballLoc :: (Float, Float) -- ^ Pong ball (x, y) location.
  , ballVel :: (Float, Float) -- ^ Pong ball (x, y) velocity.
  , player1 :: Float          -- ^ Left player paddle height.
                              -- Zero is the middle of the screen. 
  , player2 :: Float          -- ^ right player paddle height
  } deriving Show
  
-- | Draw a pong game state (converts to a picture).
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A pircture of this game state
       
render game = 
  pictures [ball, walls,
            mkPaddle rose 120 $ player1 game,
            mkPaddle orange (-120) $ player2 game]
  
  where
    -- The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red
    
    -- The bottom and top walls
    wall :: Float -> Picture
    wall offset = 
      translate 0 offset $
        color wallColor $
          rectangleSolid 270 10
    
    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]
    
    -- Make a paddle of a given border and vertical offset
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]
      
    paddleColor = light (light blue)


-- | Initialise the game with this game state
initialState :: PongGame
initialState = Game
  { ballLoc = (-10, 30)
  , ballVel = (100, -3)
  , player1 = 40
  , player2 = -80
  }
  
moveBall :: Float     -- ^ The number of seconds since last update
         -> PongGame  -- ^ The initial game state
         -> PongGame  -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y') }
  where 
    -- Old locations and velocities
    (x, y) = ballLoc game
    (vx, vy) = ballVel game
    
    -- New locations
    x' = x + vx * seconds
    y' = y + vy * seconds
 
{-     
-- | Run a finite-time-step simulation in a window.
simulate :: Display -- ^ How to display the game.
         -> Color   -- ^ Background color.
         -> Int     -- ^ Number of simulation steps to take per second of real time.
         -> a       -- ^ The initial game state. 
         -> (a -> Picture) -- ^ A function to render the game state to a picture. 
         -> (ViewPort -> Float -> a -> a) -- ^ A function to step the game once. 
         -> IO ()
-}        
        
-- | Number of frames to show per second
fps :: Int
fps = 60


-- | Update the game by moving the ball
-- Ignores viewport argument
update :: ViewPort -> Float -> PongGame -> PongGame
update _ = moveBall

-- | The main method
--  This simply calls a simulate function which creates the window,
--  The background color, the number of simulations, an inital
-- state for the game to be in, a function to render, which converts
-- the game state to a picture, and then this is passed to the current viewport
main :: IO ()
main = simulate window background fps initialState render update




    
    
