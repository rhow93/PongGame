{-|
Module      : Main
Description : A Basic Pong Game using Gloss and ALUT
License     : GPL-3
Maintainer  : rhow93@gmail.com
Stability   : experimental
Portability : POSIX

A Pong game written using the Gloss graphical libraries
and ALUT audio libraries to create a basic 2-D pong game, used as a basis
for comparison to a similar game created in C++. The two creations are to be
compared to each other and analysed to determine strengths and weaknesses
to a functional approach to game design and development. 
-}
module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Concurrent
import Sound.ALUT hiding (Static)

-- SOUND --

-- | Plays a sound permanently until the program has terminated
--   The sound does not need to be attached to a context to be initialised
--   which means that it can run in it's own thread without needing to be
--   interacted with.
playSound :: IO ()
playSound =
  withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ ->
  do
    (Just device) <- openDevice Nothing
    (Just context) <- createContext device []
    currentContext $= Just context
    buffer3 <- createBuffer $ File "sounds/theme.wav"
    [source] <- genObjectNames 1
    buffer source $= Just buffer3
    loopingMode source $= Looping
    Sound.ALUT.play [source]
    
    
-- GRAPHICS --
{- 
   It is important to know that the the (0,0) coordinate is the centre
   of the screen and anything drawn on the screen is done relative to this
-}

width, height, windowOffset, fps :: Int
width = 500
height = 300
windowOffset = 100
fps = 60

ballRadius :: Radius
ballRadius = 5

-- widths and height of paddle, note the paddle collisions interact
paddleWidth, paddleHeight, paddleBorderWidth, paddleBorderHeight, playerXVal, wallDepth :: Float
paddleWidth = paddleBorderWidth - 6
paddleHeight = paddleBorderHeight - 6
paddleBorderWidth = 20
paddleBorderHeight = 60
wallDepth = 10
-- the X location of each player, this means the player x value scales
-- with the width of the window
playerXVal = (fromIntegral width / 2) - 30 


-- | InWindow String (Int, Int) (Int, Int)	
--   Display in a window with the given name, size and position.

window :: Display
window = InWindow "Pong" (width, height + 300) (windowOffset, windowOffset) 

background :: Color
background = black

score :: Color
score = white

type Radius = Float  -- ^ An Alias of the type Float
type Position = (Float, Float) -- ^ Another Alias, replacing Position with (Float, Float)

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
  , player2 :: Float          -- ^ right player paddle height
  , wKey    :: Bool
  , sKey    :: Bool
  , upKey    :: Bool
  , downKey    :: Bool           -- ^ stores the state of current key press
  , player1Score :: Int
  , player2Score :: Int
  , p1GoalScored :: Bool
  , p2GoalScored :: Bool

  } deriving Show
 
-- | Draw a pong game state (converts to a picture).
render :: PongGame -> Picture
render game = 
  pictures [ball, ball2, walls,
            mkPaddle rose playerXVal $ player1 game,
            mkPaddle orange (-playerXVal) $ player2 game,
            score1, score2, dash]
  
  where
    -- The pong ball.
    -- the uncurry keyword is the process of taking a function with a single 
    -- argument and converting these into multiple arguments. 
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballRadius
    ballColor = dark red
    
    ball2 = uncurry translate (ball2Loc game) $ color ball2Color $ circleSolid ballRadius
    ball2Color = dark blue
    
    
    -- wall length was previously 270
    -- The bottom and top walls
    wall :: Float -> Picture
    wall wallOffset = 
      translate 0 wallOffset $
        color wallColor $
          rectangleSolid (fromIntegral width) wallDepth
    
    wallColor = greyN 0.5
    walls = pictures [wall (fromIntegral height/2), wall (-fromIntegral height/2)]
    
    -- create a small height offset value to lift the score values from the bottom
    heightOffset = 10
    score1XOffset = 100 
    dashXOffset = -50
    
    -- shows the score on screen 
    score1 = translate ((fromIntegral width/2) - score1XOffset) (-fromIntegral height + heightOffset) $ color score $ Text $ show (player1Score game)
    score2 = translate ((-fromIntegral width/2) ) (-fromIntegral height + heightOffset) $ color score $ Text $ show (player2Score game)
    dash = translate (dashXOffset) (-fromIntegral height) $ color score $ Text "-"
    
    -- Make a paddle of a given border and vertical offset
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid paddleBorderWidth paddleBorderHeight
      , translate x y $ color paddleColor $ rectangleSolid paddleWidth paddleHeight
      ]
      
    paddleColor = light (light blue)
    
-- | Initialise the game with this game state
initialState :: PongGame
initialState = Game
  { ballLoc = (0, 0)
  , ballVel = (80, 100)
  , ball2Loc = (0, 0)
  , ball2Vel = (-80, -50)
  , player1 = 40
  , player2 = 100
  , wKey = False
  , sKey = False
  , upKey = False
  , downKey = False
  , player1Score = 0
  , player2Score = 0
  , p1GoalScored = False
  , p2GoalScored = False
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
--   checks for keypresses, goals, wallbounces and paddlebounces
--   also moves the ball according to the frames per second value
update :: Float -> PongGame -> PongGame
update seconds = (updateKeyPress) .                  -- checks for key presses, and updates paddle location
                 (goal) .                            -- checks if a goal has been scored
                 (wallBounce) .                      -- checks for wall collisions
                 (paddleBounce . moveBall seconds)   -- checks for paddle collisions


-- | the aim of this function is to simplify the update funciton
--   this could easily be integrated into the update function if need be
goal :: PongGame -> PongGame
goal = (goalIterate) . (goalScored)


-- | Checks the GoalScored boolean. If this is true then we reset both balls
--   to the centre. Both balls are given (0,0) velocity and a score is added
--   to the player who scored. The goalscored boolean is then set to false again.
goalIterate :: PongGame -> PongGame
goalIterate game = game 
  { player1Score = x', 
    player2Score = y',
    ballLoc = (ball1X', ball1Y'), 
    ball2Loc = (ball2X', ball2Y'),
    ballVel = (ball1XVel', ball1YVel'), 
    ball2Vel = (ball2XVel', ball2YVel'),
    p1GoalScored = p1GoalScored',
    p2GoalScored = p2GoalScored' }
  
  where
  
    -- old values of all possibly changed fields
    x = player1Score game
    y = player2Score game
    (ball1X, ball1Y) = ballLoc game
    (ball2X, ball2Y) = ball2Loc game
    (ball1XVel, ball1YVel) = ballVel game
    (ball2XVel, ball2YVel) = ball2Vel game
  
    -- if statements to check for a goal scored & iterate goal score
    x' = if p1GoalScored game then x + 1 else x
    y' = if p2GoalScored game then y + 1 else y
    -- if a goal has been scored, set ball location to (0, 0), else keep in current position
    (ball1X', ball1Y') = if p1GoalScored game || p2GoalScored game then (0, 0) else (ball1X, ball1Y)
    (ball2X', ball2Y') = if p1GoalScored game || p2GoalScored game then (0, 0) else (ball2X, ball2Y)
    -- if a goal has been scored, set ball velocity to (0, 0), else keep current velocity
    (ball1XVel', ball1YVel') = if p1GoalScored game || p2GoalScored game then (0, 0) else (ball1XVel, ball1YVel)
    (ball2XVel', ball2YVel') = if p1GoalScored game || p2GoalScored game then (0, 0) else (ball2XVel, ball2YVel)
    -- if goal has been scored, reset boolean back to false
    -- not entirely sure if this code is necessary but I'll leave it like this for the moment
    p1GoalScored' = if p1GoalScored game then False else False
    p2GoalScored' = if p2GoalScored game then False else False
    
    
-- | Checks for goal, if a goal has been scored then set goalScored to true
goalScored :: PongGame -> PongGame
goalScored game = game { p1GoalScored = x', p2GoalScored = y'}
  where
    
    (vx, _) = ballLoc game
    (vx', _) = ball2Loc game
    
    x' = if vx < -(fromIntegral width / 2) then True
         else if vx' < -(fromIntegral width / 2) then True
         else False
    y' = if vx > fromIntegral width / 2 then True 
         else if vx' > fromIntegral width / 2 then True
         else False


-- | checks where the ball collided with the paddle and changes the y
--   velocity of the ball according to this. 
--   As it stands currently the y velocity can be a maximum of 30 and
--   minimum of -30.        
yVelocityMultiplier :: Float -> Float -> Float
yVelocityMultiplier ball_y racket_y = ((ball_y - racket_y) / 30) * 100
         

-- | This function will detect a collision of the ball with the paddle.
-- When there is a collision, the velocity of the ball will change to 
-- bounce it off the paddle
-- N.B - This should be changed to x collision as it determines a collision
-- on the X axis.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx1', vy1'), ball2Vel = (vx2', vy2') }
  where
    -- the old velocities
    (vx1, vy1) = ballVel game
    (vx2, vy2) = ball2Vel game
    (_, by1) = ballLoc game
    (_, by2) = ball2Loc game
    paddle1Loc = player1 game
    paddle2Loc = player2 game
    
    -- changes x velocity of first ball if there is a collision
    (vx1', vy1') =  if leftPaddleCollision (ballLoc game) game ballRadius 
      then (-vx1, yVelocityMultiplier by1 paddle2Loc)
      --then (-vx1, vy1)
      else  if rightPaddleCollision (ballLoc game) game ballRadius
      then (-vx1, yVelocityMultiplier by1 paddle1Loc)
      --then (-vx1, vy1)
      else (vx1, vy1)
     
      
   -- changes x velocity of second ball if there is a collision   
    (vx2', vy2') = if leftPaddleCollision (ball2Loc game) game ballRadius
      --then (-vx2, vy2)
      then (-vx2, yVelocityMultiplier by2 paddle2Loc)
      else if rightPaddleCollision (ball2Loc game) game ballRadius
      --then (-vx2, vy2)
      then (-vx2, yVelocityMultiplier by2 paddle1Loc)
      else  (vx2, vy2)

{-
IMPORTANT THINGS TO NOTE:
playerXVal is the xvalue of each player, where p1 is drawn on the right
and player 2 is drawn on the left.
the ball radius is 10.
-}


-- | Collision detection for the left paddle
--   this is a seperate function to the right paddle collision as it 
--   is important to know which paddle each ball has collided with
leftPaddleCollision :: Position -> PongGame -> Radius -> Bool
leftPaddleCollision (x, y) game radius = leftCollision
  where
    -- x is the x value of the the ball 
    -- game is the PongGame game state
    -- radius is the radius the ball (10)
    -- therefore playerXVal is 220
    
    player2PaddleLoc = player2 game

    -- length above / below paddle where we detect collision
    paddleRadius = paddleBorderHeight / 2
    
    -- the -10 takes into account that the paddle is drawn from the centre
    -- therefore there is 10 distance either side that is not accounted for
    xOffset = radius + (paddleBorderWidth / 2)
    
    leftCollision = (x - xOffset <= -playerXVal) &&
        (x - xOffset >= (-playerXVal - radius)) &&
        (y + radius >= player2PaddleLoc - paddleRadius) &&
        (y - radius <=  player2PaddleLoc + paddleRadius)


-- | Collision detection for the right paddle
--   returns true if the given (x,y) coordinates have collided with the
--   right paddle.        
rightPaddleCollision :: Position -> PongGame -> Radius -> Bool
rightPaddleCollision (x, y) game radius = rightCollision
  where
    
    player1PaddleLoc = player1 game
    
    -- length above / below paddle where we detect collision
    paddleRadius = paddleBorderHeight / 2
    
    xOffset = radius + (paddleBorderWidth / 2)
    
    rightCollision = (x + xOffset >= playerXVal) &&
        (x + xOffset <= (playerXVal + radius)) &&
        -- checks if ball is above paddle (be a bit nice with hitboxes so people don't complain)
        (y + radius >= player1PaddleLoc - paddleRadius) &&
        -- checks if ball is below paddle
        (y - radius <=  player1PaddleLoc + paddleRadius)

-- | Collision detection for the walls, works in a similar way to the paddle collisions
--   it is not important to know which wall has been collided with as
--   the resulting velocity changes are always the same.  
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
  where 

    topCollision = y - radius <= -fromIntegral height / 2 
    bottomCollision = y + radius >= fromIntegral height / 2


-- | This will detect a collision of the ball with one of the side walls.
-- When there is a collision, the velocity will change to have the ball
-- bounce off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx1, vy1'), ball2Vel = (vx2, vy2') }
  where
   
    -- The old velocities
    (vx1, vy1) = ballVel game
    (vx2, vy2) = ball2Vel game 
    
    -- if there is a collision, flip y vel, else don't
    vy1' = if wallCollision (ballLoc game) ballRadius
      then -vy1 
      else vy1
        
    vy2' = if wallCollision (ball2Loc game) ballRadius
      then -vy2
      else vy2

-- KEYBOARD INTERACTION --

-- | takes a certain keyboard event and returns an altered PongGame
--   world state.
handleKeys :: Event -> PongGame -> PongGame

handleKeys (EventKey (Char 'w') _ _ _ ) game =
  game { wKey = x' }
    where
      x = wKey game
      x' = not x
            
handleKeys (EventKey (Char 's') _ _ _ ) game =
  game { sKey = x'}
    where 
      x = sKey game
      x' = not x
      
handleKeys (EventKey (SpecialKey KeyUp) _ _ _ ) game =
  game { upKey = x' }
    where     
      x = upKey game
      x' = not x

handleKeys (EventKey (SpecialKey KeyDown) _ _ _ ) game =
  game { downKey = x'}
    where
      x = downKey game
      x' = not x
      
handleKeys (EventKey (SpecialKey KeySpace) _ _ _ ) game =
  game { ballVel = (90, 50), ball2Vel = (-60, 50) }
      
handleKeys (EventKey (Char 'c') _ _ _ ) game =
  game { ballLoc = (0, 0) }
  
handleKeys (EventKey (Char 'v') _ _ _) game =
  game { ball2Loc = (0, 0) }
  
-- Do nothing for all other events.  
handleKeys _ game = game


-- | checks if any keys are being pressed and updates the corresponding
--   player position
updateKeyPress :: PongGame -> PongGame
updateKeyPress game = game { player1 = x', player2 = y' }
  where
    
    -- old location of players
    x = player1 game
    y = player2 game
    
    upperBoundary = (fromIntegral height/2) - (paddleBorderHeight / 2) -- - (wallDepth / 2)
    lowerBoundary =  -(fromIntegral height/2) + (paddleBorderHeight / 2)-- + (wallDepth / 2)
    
    y' = if sKey game && player2 game > lowerBoundary then player2 game - 10
    else if wKey game && player2 game < upperBoundary    then player2 game + 10
    else y
    
    x' = if upKey game && player1 game < upperBoundary    then player1 game + 10
    else if downKey game && player1 game > lowerBoundary then player1 game - 10
    else x
 
-- | Main function
--   Since this function is of type IO, this allows us to perform multithreading
--   within this function. The playsound function loops indefinitely until 
--   the program itself has been terminated.
main :: IO ()
main = do
    forkIO playSound 
    Graphics.Gloss.Interface.Pure.Game.play window background fps initialState render handleKeys update
    






    
    
