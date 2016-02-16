module Main(main) where

import Graphics.Gloss

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Nice Window" (width, height) (offset, offset)

background :: Color
background = black

drawing :: Picture
drawing = pictures
  [ translate (-20) (-100) $ color ballColor $ circleSolid 30
  -- here we are using combinator functions to draw a circle, set a
  -- color and then translate the shape created to a new position
  , translate 30 50 $ color paddleColor $ rectangleSolid 10 50
  ]
  -- we can combine two or more picture values by using the pictures
  -- function and supplying a list of pictures that we wish to display
  where
    ballColor = dark red
    -- dark is a function that takes a color and returns a darker color
    paddleColor = light (light blue)
    -- light is a function that takes a color and makes it lighter
    -- here we are using it multiple times to make a really light color
    
    



main :: IO ()
main = display window background drawing
