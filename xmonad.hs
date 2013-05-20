{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

import XMonad
import qualified XMonad.StackSet as W

import qualified XMonad.Actions.Search as S
import XMonad.Actions.Promote
import XMonad.Actions.UpdatePointer

import XMonad.Config.Desktop
import XMonad.Config.Xfce

import XMonad.Hooks.DBusLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Accordion
import XMonad.Layout.Drawer
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed

import XMonad.Layout.DwmStyle
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders hiding (Never)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName

import XMonad.Prompt
import XMonad.Prompt.Man

import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.AutoUpdateLog

import qualified Network.MPD as M
import qualified Network.MPD.Commands.Extensions as M
import qualified XMonad.Util.MPD as M

main = xmonad $ myUrgencyHook myConfig

myUrgencyHook = withUrgencyHookC NoUrgencyHook urgencyConfig
    { suppressWhen = Visible
    }

myConfig = (\c -> c
    { modMask = mod4Mask
    , startupHook = do
        return ()
        autoUpdateLog 1
        connectDBusSession
        startupHook c
    , layoutHook = myLayouts
    , logHook = do
        updatePointer $ Relative 0.9 0.9
        dbusLogWithPP myPrettyPrinter
        ewmhDesktopsLogHookCustom namedScratchpadFilterOutWorkspace
    , manageHook = myManageHook <+> manageHook c
    , handleEventHook = updateLogEventHook <+> ewmhDesktopsEventHookCustom namedScratchpadFilterOutWorkspace
    , workspaces = ["web", "2", "3", "4", "mail", "chat"]
    , normalBorderColor = "#1d1d1d"
    , focusedBorderColor = "#4894E3"
    , focusFollowsMouse = True
    , clickJustFocuses = False
    , keys = myKeys <+> keys c
    }) xfceConfig

myKeys c = mkKeymap c $
    [ ("M-<Return>", promote)
    , ("M-r", sendMessage $ Toggle MIRROR)
    , ("<XF86AudioPrev>", M.withMPDX $ M.previous)
    , ("<XF86AudioPlay>", M.withMPDX $ M.toggle)
    , ("<XF86AudioNext>", M.withMPDX $ M.next)
    , ("M-m", M.ncmpcKeys' defaultXPConfig $ mkKeymap c
        [ ("M-m", namedScratchpadAction myScratchpads "ncmpcpp")
        ])
    , ("M-s t", namedScratchpadAction myScratchpads "todo")
    , ("M-s p", namedScratchpadAction myScratchpads "htop")
    , ("M-s m", manPrompt defaultXPConfig)
    ] ++
    [ ("M-s " ++ k, S.promptSearch defaultXPConfig f) | (k, f) <- searchList ] ++
    [ ("M-S-s " ++ k, S.selectSearch f) | (k, f) <- searchList ]
    where
        searchList = 
            [ ("g", S.google)
            , ("h", S.hoogle)
            , ("w", S.wikipedia)
            , ("v", S.youtube)
            , ("e", S.wiktionary)
            , ("a", S.alpha)
            , ("x", S.multi)
            ]

myScratchpads =
    [ termPad "ncmpcpp" "-e music" (customFloating $ W.RationalRect 0.25 0.25 0.5 0.5)
    , termPad "todo" "" (customFloating $ W.RationalRect 0.75 0.1 0.25 0.8)
    , termPad "htop" "-e htop" (customFloating $ W.RationalRect 0.55 0.1 0.45 0.8)
    ]
    where
        termPad n c p = NS n
            (  "Terminal --role=" ++ n
            ++ " --title=" ++ n
            ++ " " ++ c
            ) (windowRole =? n) p
        windowRole = stringProperty "WM_WINDOW_ROLE"

myPrettyPrinter = dbusPP
    { ppOrder = \ (_:l:_:e) -> l:e
    , ppExtras  = [ M.nowPlaying, M.songTimeInfo, M.statusString ]
    }

myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , namedScratchpadManageHook myScratchpads
    , transience'
    , associateWith "web"  ["Firefox"]
    , associateWith "chat" ["Pidgin", "Skype", "Konversation"]
    , associateWith "mail" ["Thunderbird"]
    , forClasses ["Xfrun4", "Xfce4-appfinder"] $ placeHook (smart (0.5, 0.5)) <+> doFloat
    , isNotification --> doIgnore
    ]
    where
        forClasses clsx ac = composeAll [ className =? c --> ac | c <- clsx]
        associateWith ws clsx = forClasses clsx $ doShift ws
        isNotification = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NOTIFICATION"

myLayouts =
    desktopLayoutModifiers $
    showWName $
    smartBorders $
    renamed [CutWordsLeft 1] $
    dwmStyle shrinkText defaultTheme $
    onWorkspace "chat" (renamed [CutWordsLeft 2] chatLayouts) $
    layouts
    where
        layouts = (mkToggle (single MIRROR) $ tall ||| Grid ||| Accordion) ||| tabbed
        chatLayouts =
            (drawer skype) `onLeft`
                ((drawer pidgin) `onRight` (Mirror Grid ||| tabbed))
        tall = Tall 1 (3/100) (1/2)
        tabbed = renamed [Replace "Tabbed"] simpleTabbed
        pidgin = Title "Buddy List"
        skype =
            (ClassName "Skype") `And`
            (Not $ Role "ConversationsWindow") `And`
            (Not $ Title "Options")
        drawer = simpleDrawer 0.01 (1/5)
