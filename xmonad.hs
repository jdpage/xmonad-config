import XMonad

import XMonad.Config.Desktop
import XMonad.Config.Xfce

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place

import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts

import XMonad.Util.EZConfig

import qualified XMonad.StackSet as W
import Data.Ratio ((%))

main = xmonad $ xfceConfig
    { modMask = mod4Mask
    , manageHook = myManageHook
    , layoutHook = myLayouts
    , workspaces = ["web", "e1", "e2", "e3", "mail", "chat", "mus"]
    } `additionalKeysP`
    [ ("M-S-h", sendMessage $ IncMasterN   1 )
    , ("M-S-l", sendMessage $ IncMasterN (-1))
    ]

myManageHook = manageHook defaultConfig <+>
               manageDocks <+>
               fullscreenManageHook <+>
               floatingHook <+>
               placeWindowsHook <+>
               (isFullscreen --> doFullFloat)

floatingHook = composeAll . concat $
    [ [ className       =? c --> (centerFloat <+> doFloat) | c <- myCFloats ]
    , [ title           =? t --> (centerFloat <+> doFloat) | t <- myTFloats ]
    , [ className       =? c --> (cornerFloat <+> doFloat) | c <- myNFloats ]
    ]
    where
        myCFloats = ["Xfrun4", "Xfce4-appfinder"]
        myTFloats = []
        myNFloats = ["Xfce4-notifyd"]
        avoidEdges = withGaps (40, 10, 10, 10)
        centerFloat = placeHook $ avoidEdges $ smart (0.5, 0.5)
        cornerFloat = placeHook $ avoidEdges $ smart (1.0, 0.0)

placeWindowsHook = composeAll . concat $
    [ [ className       =? c --> doF (W.shift "web")       | c <- webApps  ]
    , [ className       =? c --> doF (W.shift "chat")      | c <- chatApps ]
    , [ className       =? c --> doF (W.shift "mail")      | c <- mailApps ]
    , [ className       =? c --> doF (W.shift "mus")       | c <- musApps  ]
    ]
    where
        webApps  = ["Firefox"]
        chatApps = ["Pidgin", "Skype", "Konversation", "Choqok"]
        mailApps = ["Thunderbird"]
        musApps  = ["Pragha"]

twoPaneLayout = Tall nmaster delta ratio
    where
        nmaster = 1
        delta = 3/100
        ratio = 1/2

withTwoIM left right base = withIM (1%6) right $ reflectHoriz $ withIM (1%5) left $ reflectHoriz $ base

twoPaneFirst = twoPaneLayout ||| Mirror twoPaneLayout ||| Grid ||| simpleTabbed
gridFirst    = Grid ||| simpleTabbed ||| twoPaneLayout ||| Mirror twoPaneLayout
tabbedFirst  = simpleTabbed ||| twoPaneLayout ||| Mirror twoPaneLayout ||| Grid

myChat = withTwoIM pidgin skype gridFirst
    where
        pidgin = Title "Buddy List"
        skype = (ClassName "Skype") `And` (Not $ Role "ConversationsWindow")
        
myLayouts = desktopLayoutModifiers $ smartBorders $ toggleLayouts Full perWS
    where
        perWS = onWorkspace "web" tabbedFirst $
                onWorkspace "chat" myChat $
                twoPaneFirst
