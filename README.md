# PongGame

This is a pong game created in Haskell for a 3rd year project at Swansea University.

The .hs file containing the pong game code in it's current form is located at file main.hs

To compile this code you'll need to have the full haskell platform installed

`sudo apt-get install haskell-platform`

navigate to the cloned repo and enter the following

`cabal update`
`cabal sandbox init`
`cabal init`

(here you want to make sure that you mark the program as executable, everything else should take default values)

`cabal install --only-dependencies`
`cabal run`

This should allow you to compile + run the main file from source.
