# PongGame

This is a pong game created in Haskell for a 3rd year project at Swansea University.

The .hs file containing the pong game code in it's current form is located at file main.hs

To compile this code you'll need to have the full haskell platform installed

`sudo apt-get install haskell-platform`

navigate to the cloned repo and enter the following

```
cabal update
cabal sandbox init
```

(here you want to make sure that you mark the program as executable, everything else should take default values)

```
cabal install --only-dependencies
cabal build
```

This should allow you to compile + run the main file from source.

You will need OpenGL and OpenAL/ALUT install for this program to run on your system.

The game is confirmed to work on Ubuntu distributions, and should theoretically work on Windows/Mac.

#GAME EXAMPLES

There are 5 different examples, highlighting the development process of the game.

you can run each of these with the following commands:

```
cabal run BasicPicture
cabal run BasicAnimation
cabal run SoundExample
cabal run 2BallsPure
cabal run PongGame
```
#DOCUMENTATION

The Main.hs file has been developed with a Haddock commenting style in mind. Because of this you can generate the documentation for the project at compile time using the following commands:

```
cabal haddock --internal --executables
```
