module XMonad.Util.AutoUpdateLog
    ( autoUpdateLog
    , updateLogEventHook
    ) where

import Control.Concurrent
import Data.Monoid
import XMonad

updateLogAtom :: Display -> IO Atom
updateLogAtom d = internAtom d "XMONAD_UPDATE_LOG" False

runInterval :: Rational -> Display -> Window -> Atom -> IO ()
runInterval s d rw a = do
    threadDelay (fromEnum $ s * 1000000)
    allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw a 32 a currentTime
        sendEvent d rw False structureNotifyMask e
    sync d False
    runInterval s d rw a

autoUpdateLog :: Rational -> X ()
autoUpdateLog s = io $ do
    forkIO $ do
        d  <- openDisplay ""
        rw <- rootWindow d $ defaultScreen d
        a  <- updateLogAtom d
        runInterval s d rw a
    return ()

updateLogEventHook :: Event -> X All
updateLogEventHook (ClientMessageEvent {ev_message_type = mt}) = do
    d <- asks display
    a <- io $ updateLogAtom d
    if mt == a
        then ask >>= logHook . config
        else return ()
    return $ All True
updateLogEventHook _ = return $ All True
