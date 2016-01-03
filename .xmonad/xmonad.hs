{-# LANGUAGE DeriveDataTypeable #-}

import Data.Monoid

import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Run

main = do
    h <- spawnPipe "lemonbar -p -d -b -g 'x55' -f \"Open Sans:size=12\""
    xmonad $ def
        { terminal    = "urxvt"
        , borderWidth = 8
        , layoutHook  = myLayout
        , logHook     = myLogHook h
        , startupHook = myStartup
        , handleEventHook = fullscreenEventHook <+> docksEventHook
        , manageHook = composeAll [ fullscreenManageHook
                                  , manageDocks
                                  ]
        , normalBorderColor  = colors "black"
        , focusedBorderColor = colors "darkred"
        } `additionalKeysP`
          [ ("M1-p", spawn "rofi -show run -font 'Open Sans 30' -bg '#282828' -fg '#ebdbb2' -hlbg '#458588' -hlfg '#ebdbb2' -fuzzy -bw 0 -separator-style solid -bc '#282828' -width 100 -padding 800 -eh 2 -opacity 90 -lines 6 -hide-scrollbar") -- open program
          , ("M1-o", spawn "rofi -show window -font 'Open Sans 30' -bg '#282828' -fg '#ebdbb2' -hlbg '#458588' -hlfg '#ebdbb2' -hlbg-active '#458588' -fuzzy -bw 0 -separator-style solid -bc '#282828' -width 100 -padding 800 -eh 2 -opacity 90 -lines 6 -hide-scrollbar") -- switch window
          , ("M1-C-l", spawn "xscreensaver-command --lock") -- to lock
          , ("M1-C-<End>", spawn "amixer -q sset Capture toggle") -- toggle mute mic
          , ("<XF86_AudioMute>", spawn "amixer -q sset Master toggle") -- toggle mute
          , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 5%+") -- raise volume
          , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 5%-") -- lower volume
          , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10") -- raise brightness
          , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10") -- lower brightness
          , ("C-<Home>", spawn "playerctl play-pause") -- mpd toggle play pause
          , ("C-<End>", spawn "playerctl stop") -- mpd stop
          , ("C-<Page_Up>", spawn "playerctl previous") -- mpd previous
          , ("C-<Page_Down>", spawn "playerctl next") -- mpd next
          ]

myLayout = ( avoidStruts
           . smartBorders
           . smartSpacingWithEdge space
           $ tiled
         ||| Mirror tiled
         )
       ||| smartBorders Full
  where
    -- Space between windows
    space = 20
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio = 1/2

    -- Percent of screen to increment by when resizing panes
    delta = 3/100

mpdL :: Logger
mpdL = (fmap . fmap) (shorten 130) . logCmd $ "echo $(playerctl metadata title) - $(playerctl metadata album) - $(playerctl metadata artist)"

volL :: Logger
volL = logCmd "amixer sget Master | egrep -o \"[0-9]+%\" | head -n 1"

-- | Append a string to a logger with optional separator
appendLog :: Bool -> String -> Logger -> Logger
appendLog False xs = (fmap . fmap) (\x -> xs ++ x ++ " ")
appendLog True xs  = (fmap . fmap) (\x -> xs ++ x ++ " " ++ sep)
  where
    sep = " %{F" ++ colors "darkgrey" ++ "}/ "

-- | Assign a foreground color in the bar to a string
barColor :: String -> String -> String
barColor x = (++) $ "%{F" ++ x ++ "}"

-- | Assign a background color in the bar to a string
barBColor :: String -> String -> String
barBColor x = (++) $ "%{B" ++ x ++ "}"

-- | gruvbox color scheme
myLogHook h = dynamicLogWithPP
            $ def { ppOutput  = hPutStrLn h
                  , ppSep     = " "
                  , ppLayout  = const ""
                  , ppTitle   = const ""
                  , ppCurrent = barColor (colors "darkred")
                  , ppHidden  = barColor (colors "darkblue")
                  , ppVisible = barColor (colors "darkgreen")
                  , ppUrgent  = barColor (colors "darkmagenta") . wrap "[" "]"
                  , ppOrder   = (:) (barColor (colors "white") "" ++ barBColor (colors "black") "")
                  , ppExtras  = [ appendLog False ("%{c}" ++ barColor (colors "darkblue") "" ++ "♞") mpdL
                                , appendLog True ("%{r}" ++ barColor (colors "darkmagenta") "" ++ "♛") volL
                                , appendLog True (barColor (colors "darkred") "" ++ "♚") battery
                                , appendLog False (barColor (colors "lightgrey") "" ++ "♜") $ date "%a %b %d %T"
                                ]
                  }

myStartup :: X ()
myStartup = do
    -- Delay between button presses
    spawn "xset r rate 220"
    -- No black screen after inactivity
    spawn "xset -dpms"
    spawn "xset s off"
    -- Caps as control
    spawn "setxkbmap -option ctrl:nocaps"
    -- Compositor
    spawn "compton"
    -- Cursor
    spawn "xsetroot -cursor_name left_ptr"
    -- Random background
    spawn "feh --randomize --bg-fill ~/Dropbox/Desktops/*"
    -- Notifications
    spawn "twmnd"
    -- NetworkManager applet
    spawn "nm-applet"
    -- Dropbox
    spawn "dropbox"
    -- mopidy
    spawn "mopidy"
    -- Locking
    spawn "xscreensaver -no-splash"

colors :: String -> String
colors "background"  = "#282828"
colors "foreground"  = "#ebdbb2"
colors "black"       = "#282828"
colors "darkgrey"    = "#928374"
colors "darkred"     = "#cc241d"
colors "red"         = "#fb4934"
colors "darkgreen"   = "#98971a"
colors "green"       = "#b8bb26"
colors "darkyellow"  = "#d79921"
colors "yellow"      = "#fabd2f"
colors "darkblue"    = "#458588"
colors "blue"        = "#83a598"
colors "brightblue"  = "#2e9ef4"
colors "darkmagenta" = "#b16286"
colors "magenta"     = "#d3869b"
colors "darkcyan"    = "#689d6a"
colors "cyan"        = "#8ec07c"
colors "lightgrey"   = "#a89984"
colors "white"       = "#ebdbb2"
