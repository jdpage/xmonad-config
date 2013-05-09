{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module XMonad.Hooks.DBusLog
    ( dbusPP
    , connectDBusSession
    , dbusLog
    , dbusLogWithPP
    , pangoColor
    , pangoBold
    , pangoSanitize
    ) where

import Codec.Binary.UTF8.String (decodeString)
import Data.Monoid
import DBus
import DBus.Client
import XMonad
import XMonad.Hooks.DynamicLog
import qualified XMonad.Util.ExtensibleState as XS

data DBusSession = Connected Client | Disconnected deriving Typeable
instance ExtensionClass DBusSession where
    initialValue = Disconnected

connectDBusSession :: X ()
connectDBusSession = do
    dbus <- io $ connectSession
    io $ requestName dbus (busName_ "org.xmonad.Log")
        [ nameAllowReplacement
        , nameReplaceExisting
        , nameDoNotQueue
        ]
    XS.put $ Connected dbus

dbusLog :: X ()
dbusLog = dbusLogWithPP dbusPP

dbusLogWithPP :: PP -> X ()
dbusLogWithPP pp = do
    line <- dynamicLogString pp
    session <- XS.get
    case session of
        Connected client -> io $ dbusOutput client line
        Disconnected     -> io $ ppOutput pp line

dbusOutput :: Client -> String -> IO ()
dbusOutput dbus str = do
    let sig = (signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
            signalBody = [toVariant $ decodeString str]
        }
    emit dbus sig

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
    where
        left  = "<span foreground=\"" ++ fg ++ "\">"
        right = "</span>"

pangoBold :: String -> String
pangoBold = wrap "<b>" "</b>"

pangoSanitize = foldr sanitize ""
    where
        sanitize '>'    xs = "&gt;" ++ xs
        sanitize '<'    xs = "&lt;" ++ xs
        sanitize '\"'   xs = "&quot;" ++ xs
        sanitize '&'    xs = "&amp;" ++ xs
        sanitize x      xs = x:xs

dbusPP :: PP
dbusPP = defaultPP
    { ppCurrent = pangoColor "#4894E3" . pangoSanitize
    , ppVisible = pangoColor "yellow" . pangoSanitize
    , ppHidden = pangoSanitize
    , ppUrgent = pangoColor "red" . pangoSanitize
    , ppTitle = pangoSanitize . shorten 80
    , ppLayout = pangoSanitize
    }
