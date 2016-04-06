module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import Sound.ALUT hiding (Static)

-- SOUND --

-- load sound
loadSound path = do
  -- create a buffer from the sound file
  buf <- createBuffer (File path)
  souce <- genObjectiveName
  buffer source $= Just buf
  return souce
  
-- loops the music and plays it  
backgroundMusic :: Source -> IO()
backgroundMusic source = do
  -- initialise ALUT context
  withProgNameAndArgs runALUT $ \progName args -> do
    loopingMode source $= Looping
    play [source]


-- GRAPHICS --

width, height, offset, fps :: Int
width = 500
height = 300
offset = 100
fps = 60

playerXVal :: Float
playerXVal = (fromIntegral width / 2) - 30 

window :: Display
window = InWindow "Pong" (600, height + 300) (offset, offset) 

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
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red
    
    ball2 = uncurry translate (ball2Loc game) $ color ball2Color $ circleSolid 10
    ball2Color = dark blue
    
    
    -- wall length was previously 270
    -- The bottom and top walls
    wall :: Float -> Picture
    wall offset = 
      translate 0 offset $
        color wallColor $
          rectangleSolid 670 10
    
    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]
    
    -- shows the score on screen 
    score1 = translate 200 (-270) $ color score $ Text $ show (player1Score game)
    score2 = translate (-200) (-270) $ color score $ Text $ show (player2Score game)
    dash = translate 0 (-270) $ color score $ Text "-"
    
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
-- Ignores viewport argument
update :: Float -> PongGame -> PongGame
update seconds = (updateKeyPress) .                  -- ^ checks for key presses, and updates paddle location
                 (goal) .                      -- ^ checks if a goal has been scored
                 (wallBounce) .                      -- ^ checks for wall collisions
                 (paddleBounce . moveBall seconds)   -- ^ checks for paddle collisions


-- the aim of this function is to simplify the update funciton
-- this could easily be integrated into the update function if need be
goal = (goalIterate) . (goalScored)

{-
Checks the GoalScored boolean. If this is true then we reset both balls
to the centre. Both balls are given (0,0) velocity and a score is added
to the player who scored. The goalscored boolean is then set to false again.
-}
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
    
    
-- Checks for goal, if a goal has been scored then set goalScored to true
goalScored :: PongGame -> PongGame
goalScored game = game { p1GoalScored = x', p2GoalScored = y'}
  where
    
    (vx, vy) = ballLoc game
    (vx', vy') = ball2Loc game
    
    x' = if vx < -(fromIntegral width / 2) then True
         else if vx' < -(fromIntegral width / 2) then True
         else False
    y' = if vx > fromIntegral width / 2 then True 
         else if vx' > fromIntegral width / 2 then True
         else False
         
         
-- | This function will detect a collision of the ball with the paddle.
-- When there is a collision, the velocity of the ball will change to 
-- bounce it off the paddle
-- N.B - This should be changed to x collision as it determines a collision
-- on the X axis.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx1', vy1'), ball2Vel = (vx2', vy2') }
  where
    radius = 10
    
    -- the old velocities
    (vx1, vy1) = ballVel game
    (vx2, vy2) = ball2Vel game
    (bx1, by1) = ballLoc game
    (bx2, by2) = ball2Loc game
    
    -- changes x velocity of first ball if there is a collision
    vx1' = if paddleCollision (ballLoc game) game radius 
      then -vx1
      else  vx1
     
      
    vy1' = if paddleCollision (ballLoc game) game radius
      then vy1
      else vy1
      
   -- changes x velocity of second ball if there is a collision   
    vx2' = if paddleCollision (ball2Loc game) game radius
      then -vx2
      else  vx2
      
    vy2' = if paddleCollision (ball2Loc game) game radius
      then vy2
      else vy2
       
       
-- Is there a way of making this more generic, such that it
-- simply detects a collision between a the ball and any area
-- on the x axis?
-- also, this code is disgusting and needs sorting
paddleCollision :: Position -> PongGame -> Radius -> Bool
paddleCollision (x, y) game radius = leftCollision || rightCollision
  where
    
    player1PaddleLoc = player1 game
    player2PaddleLoc = player2 game
    paddleLocX = fromIntegral width / 2
    
    -- adjust paddle location
    paddleAdjust = 60
    -- length above / below paddle where we detect collision
    paddleRadius = 60
   
    rightCollision = (x - radius >= paddleLocX - paddleAdjust) &&
        (x - radius <= (paddleLocX + 2) - paddleAdjust) &&
        -- checks if ball is above paddle (be a bit nice with hitboxes so people don't complain)
        (y - radius >= player1PaddleLoc - paddleRadius) &&
        -- checks if ball is below paddle
        (y + radius <=  player1PaddleLoc + paddleRadius)
    
    leftCollision = (x + radius <= -paddleLocX + paddleAdjust) &&
        (x + radius >= (-paddleLocX - 2) + paddleAdjust) &&
        (y - radius >= player2PaddleLoc - paddleRadius) &&
        (y + radius <=  player2PaddleLoc + paddleRadius)

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
  game { ballVel = (90, 50), ball2Vel = (-30, -10) }
      
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
    
    y' = if sKey game && player2 game > (-100) then player2 game - 10
    else if wKey game && player2 game < 100    then player2 game + 10
    else y
    
    x' = if upKey game && player1 game < 100    then player1 game + 10
    else if downKey game && player1 game > (-100) then player1 game - 10
    else x
    
-- putting the main function in a do bracket to encourage me to do some
-- concurrent processing (e.g adding sound)
main :: IO ()
main = do
         play window background fps initialState render handleKeys update
         music <- loadSound "theme.mp3"
         backgroundMusic music






    
    
