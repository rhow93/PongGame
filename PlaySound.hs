import Control.Monad
import Sound.ALUT

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
    play [source]
    sleep 4
    closeDevice device
    return ()

main = playSound
