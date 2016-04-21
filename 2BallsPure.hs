module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game


width, height, offset, fps :: Int
width = 300
height = 300
offset = 100
fps = 60

window :: Display
window = InWindow "Pong" (width + 300, height + 300) (offset, offset)

background :: Color
background = black

score :: Color
score = white

-- | A data structure to hold the state of the Pong game
-- This allows us to easily update the game state without worrying
-- how each piece is drawn. We can summarise the game state by
-- the pong ball location, it's velocity and the locations of the paddles.
data PongGame = Game 
  { ballLoc :: (Float, Float) -- ^ Pong ball (x, y) location.
  , ballVel :: (Float, Float) -- ^ Pong ball (x, y) velocity.
  , ball2Loc:: (Float, Float)
  , ball2Vel:: (Float, Float)
  , player1 :: Float          -- ^ Left player paddle height.
                              -- Zero is the middle of the screen. 
  , player2 :: Float          -- ^ right player paddle height
  , qKey    :: Bool
  , aKey    :: Bool
  , oKey    :: Bool
  , lKey    :: Bool           -- ^ stores the state of current key press
  , player1Score :: Int
  , player2Score :: Int

  } deriving Show
  
-- | Draw a pong game state (converts to a picture).
render :: PongGame -> Picture
render game = 
  pictures [ball, ball2, walls,
            mkPaddle rose 120 $ player1 game,
            mkPaddle orange (-120) $ player2 game,
            score1, score2, dash]
  
  where
    -- The pong ball.
    -- the uncurry keyword is the process of taking a function with a single 
    -- argument and converting these into multiple arguments. 
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red
    
    ball2 = uncurry translate (ball2Loc game) $ color ball2Color $ circleSolid 10
    ball2Color = dark blue
    
    -- The bottom and top walls
    wall :: Float -> Picture
    wall offset = 
      translate 0 offset $
        color wallColor $
          rectangleSolid 270 10
    
    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]
    
    -- shows the score on screen 
    score1 = translate 120 (-270) $ color score $ Text $ show (player1Score game)
    score2 = translate (-180) (-270) $ color score $ Text $ show (player2Score game)
    dash = translate (-30) (-270) $ color score $ Text "-"
    
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
  { ballLoc = (0, 0)
  , ballVel = (50, 100)
  , ball2Loc = (0, 0)
  , ball2Vel = (-50, -100)
  , player1 = 40
  , player2 = 100
  , qKey = False
  , aKey = False
  , oKey = False
  , lKey = False
  , player1Score = 0
  , player2Score = 0
  }
  
moveBall :: Float     -- ^ The number of milliseconds since last update
         -> PongGame  -- ^ The initial game state
         -> PongGame  -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y'), ball2Loc = (x'', y'') }
  where 
    -- Old locations and velocities
    (x, y) = ballLoc game
    (vx, vy) = ballVel game
    
    (xOld, yOld) = ball2Loc game
    (vx', vy') = ball2Vel game
    
    -- New locations
    x' = x + vx * seconds * 2
    y' = y + vy * seconds * 2
    
    x'' = xOld + vx' * seconds * 2
    y'' = yOld + vy' * seconds * 2
    
-- | Update the game by moving the ball
-- Ignores viewport argument
update :: Float -> PongGame -> PongGame
update seconds = (updateKeyPress) .                  -- checks for key presses, and updates paddle location
                 (goalScored) .                      -- checks if a goal has been scored
                 (wallBounce) .                      -- checks for wall collisions
                 (paddleBounce . moveBall seconds)   -- checks for paddle collisions

goalScored :: PongGame -> PongGame
goalScored game = game { player1Score = x', player2Score = y'}
  where
    
    x = player1Score game
    y = player2Score game
    (vx, vy) = ballLoc game
    
    x' = if vx < (-150) then x + 1 else x
    y' = if vx > 150 then y + 1 else y

-- | This function will detect a collision of the ball with the paddle.
-- When there is a collision, the velocity of the ball will change to 
-- bounce it off the paddle
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx1', vy1), ball2Vel = (vx2', vy2) }
  where
    radius = 10
    
    -- the old velocities
    (vx1, vy1) = ballVel game
    (vx2, vy2) = ball2Vel game
    
    
    vx1' = if paddleCollision (ballLoc game) game radius 
      then -vx1
      else  vx1
      
    vx2' = if paddleCollision (ball2Loc game) game radius
      then -vx2
      else  vx2
       
paddleCollision :: Position -> PongGame -> Radius -> Bool
paddleCollision (x, y) game radius = leftCollision || rightCollision
  where
    
    player1PaddleLoc = player1 game
    player2PaddleLoc = player2 game
    paddleLocX = fromIntegral width / 2

    -- This only detects collisions for the side, not the entire paddle
    -- Offset is 40 because paddles are created at 120, and not 150, which is the width of the screen
    rightCollision = (x + (radius + 40) >= paddleLocX) &&
        (x + (radius + 40) <= 152) &&
        -- checks if ball is above paddle (be a bit nice with hitboxes so people don't complain)
        (y - radius >= player1PaddleLoc - 60) &&
        -- checks if ball is below baddle
        (y + radius <=  player1PaddleLoc + 60)
    
    leftCollision =(x - (radius + 40) <= -paddleLocX) &&
        (x + (radius + 40) >= (-152)) &&
        (y - radius >= player2PaddleLoc - 60) &&
        (y + radius <=  player2PaddleLoc + 60)

type Radius = Float  -- ^ the type keyword creates an alias. Everytime we
-- see Radius, we can replace it with float.
type Position = (Float, Float) -- ^ this is similar with position, everytime 
-- we see a position type, we know this is a (Float, Float) couplet.

wallCollision :: Position -> Radius -> Bool -- ^ using this alias allows 
-- our code to be easy to read. This is standard haskell documentation
wallCollision (_, y) radius = topCollision || bottomCollision
  where 
    -- ^ You cannot directly compare a float and an int, so we use fromIntegral
    -- which allows us to convert an Int to a float.
    topCollision = y - radius <= -fromIntegral height / 2 
    bottomCollision = y + radius >= fromIntegral height / 2


-- | This will detect a collision of the ball with one of the side walls.
-- When there is a collision, the velocity will change to have the ball
-- bounce off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx1, vy1'), ball2Vel = (vx2, vy2') }
  where
    -- radius. using the same thing as in `render`.
    radius = 10    
    -- The old velocities
    (vx1, vy1) = ballVel game
    (vx2, vy2) = ball2Vel game 
    
    -- if there is a collision, flip y vel, else don't
    vy1' = if wallCollision (ballLoc game) radius
      then -vy1 
      else vy1
        
    vy2' = if wallCollision (ball2Loc game) radius
      then -vy2
      else vy2

-- | responds to a key event
handleKeys :: Event -> PongGame -> PongGame

handleKeys (EventKey (Char 'q') _ _ _ ) game =
  game { qKey = x' }
    where
      x = qKey game
      x' = not x
            
handleKeys (EventKey (Char 'a') _ _ _ ) game =
  game { aKey = x'}
    where 
      x = aKey game
      x' = not x
  
handleKeys (EventKey (Char 'o') _ _ _ ) game =
  game { oKey = x' }
    where     
      x = oKey game
      x' = not x

handleKeys (EventKey (Char 'l') _ _ _ ) game =
  game { lKey = x'}
    where
      x = lKey game
      x' = not x
      
-- when you press the s key, reset the ball to the center
handleKeys (EventKey (Char 'c') _ _ _ ) game =
  game { ballLoc = (0, 0) }
  
handleKeys (EventKey (Char 'v') _ _ _) game =
  game { ball2Loc = (0, 0) }
  
-- Do nothing for all other events.  
handleKeys _ game = game

-- checks if any keys are being pressed and updates the corresponding
-- player position
updateKeyPress :: PongGame -> PongGame
updateKeyPress game = game { player1 = x', player2 = y' }
  where
    
    -- old location of players
    x = player1 game
    y = player2 game
    
    y' = if aKey game && player2 game > (-100) then player2 game - 10
    else if qKey game && player2 game < 100    then player2 game + 10
    else y
    
    x' = if oKey game && player1 game < 100    then player1 game + 10
    else if lKey game && player1 game > (-100) then player1 game - 10
    else x
    
-- | The main method
--  This simply calls a simulate function which creates the window,
--  The background color, the number of simulations, an inital
-- state for the game to be in, a function to render, which converts
-- the game state to a picture, and then this is passed to the current viewport
main :: IO ()
main = play window background fps initialState render handleKeys update





    
    
