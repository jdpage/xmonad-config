import XMonad
import XMonad.Util.EZConfig

import XMonad.Actions.Promote

import XMonad.Config.Desktop
import XMonad.Config.Xfce

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place

import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ShowWName
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts

import Data.Ratio ((%))

main = xmonad $ xfceConfig
    { modMask = mod4Mask
    , manageHook = myManageHook
    , layoutHook = myLayouts
    , workspaces = ["web", "e1", "e2", "e3", "mail", "chat", "mus"]
    , normalBorderColor = "#1d1d1d"
    , focusedBorderColor = "#4894E3"
    } `additionalKeysP`
    [ ("M-<Return>", promote)
    ]

{-
 - Helper functions
 -}

-- perform an action on windows with a class in a list
doForClasses ac clsx = composeAll $ [ className =? c --> ac | c <- clsx]

-- associate a list of windows with a workspace so that they are automatically
-- placed there when opened.
associateWith ws clsx = doForClasses (doShift ws) clsx

-- center a floating window
doCenter = placeHook $ withGaps (40, 10, 10, 10) $ smart (0.5, 0.5)

-- check if a window is a notification
isNotification = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NOTIFICATION"

{-
 - Management hooks
 -}

myManageHook = composeAll $
    [ manageHook xfceConfig
    , (isNotification --> doIgnore)
    , associateWith "web"  ["Firefox"]
    , associateWith "chat" ["Pidgin", "Skype", "Konversation"]
    , assocaiteWith "mail" ["Thunderbird"]
    , associateWith "mus"  ["Pragha"]
    , doForClasses (doCenter <+> doFloat) ["Xfrun", "Xfce4-appfinder"]
    , transience'
    , (isFullscreen --> doFullFloat)
    ]

{-
 - Layouts
 -}

twoPaneLayouts = twoPane ||| Mirror twoPane
    where
        twoPane = Tall 1 3/100 1/2

twoPaneFirst = twoPaneLayouts ||| Grid ||| simpleTabbed
gridFirst    = Grid ||| simpleTabbed ||| twoPaneLayouts
tabbedFirst  = simpleTabbed ||| twoPaneLayouts ||| Grid

withTwoIM left right base =
    withIM (1%6) left $ reflectHoriz $
    withIM (1%5) right $ reflectHoriz $
    base

myLayouts = desktopLayoutModifiers $ showWName $ smartBorders $ toggleLayouts Full perWS
    where
        perWS = onWorkspace "web" tabbedFirst $
                onWorkspace "chat" withTwoIM skype pidgin gridFirst $
                twoPaneFirst
        pidgin = Title "Buddy List"
        skype = (ClassName "Skype") `And` (Not $ Role "ConversationsWindow")
