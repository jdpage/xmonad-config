module XMonad.Util.MPD
    ( withMPDX
    , logMPD
    , sgGetFirstTag
    , logCurrentSong
    , nowPlaying
    , pangoNowPlaying
    , songTimeInfo
    , statusString
    , seekcur
    , toggleRepeat
    , toggleRandom
    , toggleSingle
    , toggleConsume
    , addMusicPrompt
    , ncmpcKeys
    , ncmpcKeys'
    ) where

import Control.Monad
import Data.ByteString.UTF8 (fromString)
import qualified Data.Map as M
import Network.MPD
import Network.MPD.Commands.Extensions
import Text.Printf
import XMonad
import XMonad.Actions.Submap
import XMonad.Prompt

withMPDX :: MPD a -> X ()
withMPDX x = io $ withMPD x >> return ()

logMPD :: MPD (Maybe a) -> X (Maybe a)
logMPD x = io $ do
    r <- withMPD x
    case r of
        Left  _ -> return Nothing
        Right x -> return x

sgGetFirstTag :: Metadata -> Song -> Maybe String
sgGetFirstTag m sg = liftM (toString . head) $ sgGetTag m sg

logCurrentSong :: (Song -> Maybe a) -> X (Maybe a)
logCurrentSong m = logMPD $ currentSong >>= (\cs -> return $ cs >>= m)

nowPlaying :: X (Maybe String)
nowPlaying = logCurrentSong $ \ sg -> do
    artist <- sgGetFirstTag Artist sg
    title  <- sgGetFirstTag Title  sg
    return $ title ++ " - " ++ artist

pangoNowPlaying :: X (Maybe String)
pangoNowPlaying = logCurrentSong $ \ sg -> do
    artist <- sgGetFirstTag Artist sg
    title  <- sgGetFirstTag Title  sg
    return $ "<b>" ++ title ++ "</b> - " ++ artist

songTimeInfo :: X (Maybe String)
songTimeInfo = logMPD $ do
    st <- status
    let (elapsed, total) = stTime st
    return $ case stState st of
        Playing -> Just $ asTime (round elapsed) ++ "/" ++ asTime total
        Paused  -> Just "Paused"
        Stopped -> Nothing
    where
        asTime :: Seconds -> String
        asTime t = let (m, s) = t `divMod` 60 in printf "%d:%02d" m s

statusString :: X (Maybe String)
statusString = logMPD $ do
    st <- status
    let r = if stRepeat  st then "r" else ""
    let z = if stRandom  st then "z" else ""
    let s = if stSingle  st then "s" else ""
    let c = if stConsume st then "c" else ""
    return $ Just $ r ++ z ++ s ++ c

seekcur :: (Functor m, MonadMPD m) => Seconds -> m ()
seekcur t = do
    cs <- currentSong
    case cs of 
        Just sg -> case sgIndex sg of
            Just idx -> seek idx t
            Nothing  -> return ()
        Nothing -> return ()

toggleRepeat :: (MonadMPD m) => m ()
toggleRepeat = status >>= (Network.MPD.repeat . not . stRepeat)

toggleRandom :: (MonadMPD m) => m ()
toggleRandom = status >>= (random . not . stRandom)

toggleSingle :: (MonadMPD m) => m ()
toggleSingle = status >>= (single . not . stSingle)

toggleConsume :: (MonadMPD m) => m ()
toggleConsume = status >>= (consume . not . stConsume)

data AddMusicPrompt = AddMusicPrompt
instance XPrompt AddMusicPrompt where
    showXPrompt _ = "add songs: "

addMusicPrompt :: XPConfig -> X ()
addMusicPrompt c = mkXPrompt AddMusicPrompt c noComplete addPath
    where
        noComplete _ = return []
        addPath = withMPDX . add_ . Path . fromString

ncmpcKeys' :: XPConfig -> M.Map (KeyMask, KeySym) (X ()) -> X ()
ncmpcKeys' config map = submap $ map <+> M.fromList
    [ ((0,          xK_s),          withMPDX stop)
    , ((shiftMask,  xK_p),          withMPDX toggle)
    , ((shiftMask,  xK_comma),      withMPDX previous)
    , ((shiftMask,  xK_period),     withMPDX next)
    , ((0,          xK_BackSpace),  withMPDX (seekcur 0))
    , ((shiftMask,  xK_BackSpace),  withMPDX (play $ Just 0))
    , ((0,          xK_r),          withMPDX toggleRepeat)
    , ((0,          xK_z),          withMPDX toggleRandom)
    , ((0,          xK_y),          withMPDX toggleSingle)
    , ((shiftMask,  xK_r),          withMPDX toggleConsume)
    , ((shiftMask,  xK_z),          withMPDX (shuffle Nothing))
    , ((0,          xK_u),          withMPDX (update []))
    , ((0,          xK_c),          withMPDX clear)
    , ((0,          xK_a),          addMusicPrompt config)
    ]

ncmpcKeys :: XPConfig -> X ()
ncmpcKeys config = ncmpcKeys' config $ M.fromList []
