{-# LANGUAGE DeriveDataTypeable #-}

import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W

main = do
    h <- spawnPipe "lemonbar -b -d -g 'x55' -f 'Open Sans:size=12' -f 'FontAwesome:size=14'"
    xmonad $ def
        { terminal           = "urxvt"
        , borderWidth        = 8
        , workspaces         = myWorkspaces
        , layoutHook         = fullscreenFull myLayout
        , logHook            = myLogHook h
        , startupHook        = myStartup
        , handleEventHook    = fullscreenEventHook <+> docksEventHook
        , manageHook         = composeAll [ fullscreenManageHook
                                          , manageDocks
                                          ]
        , normalBorderColor  = colors "black"
        , focusedBorderColor = colors "darkred"
        } `additionalKeysP` myKeys


-- My shortcuts. Also changes greedyView to view for multiple monitors
myKeys = [ ("M1-p", spawn "rofi -show run -font 'Open Sans 30' -bg '#282828' -fg '#ebdbb2' -hlbg '#458588' -hlfg '#ebdbb2' -fuzzy -bw 0 -separator-style solid -bc '#282828' -width 100 -padding 800 -eh 2 -opacity 90 -lines 6 -hide-scrollbar") -- open program
         , ("M1-o", spawn "rofi -show window -font 'Open Sans 30' -bg '#282828' -fg '#ebdbb2' -hlbg '#458588' -hlfg '#ebdbb2' -hlbg-active '#458588' -fuzzy -bw 0 -separator-style solid -bc '#282828' -width 100 -padding 800 -eh 2 -opacity 90 -lines 6 -hide-scrollbar") -- switch window
         , ("M1-C-l", spawn "xscreensaver-command --lock") -- to lock
         , ("M1-C-<End>", spawn "amixer -q sset Capture toggle") -- toggle mute mic
         , ("<XF86AudioMute>", spawn "amixer -q sset Master toggle") -- toggle mute
         , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 5%+") -- raise volume
         , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 5%-") -- lower volume
         , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10") -- raise brightness
         , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10") -- lower brightness
         , ("C-<Home>", spawn "playerctl play-pause") -- mpd toggle play pause
         , ("C-<End>", spawn "playerctl stop") -- mpd stop
         , ("C-<Page_Up>", spawn "playerctl previous") -- mpd previous
         , ("C-<Page_Down>", spawn "playerctl next") -- mpd next
         ]
      ++ [ (otherModMasks ++ "M-" ++ [key], action tag)
      | (tag, key)  <- zip myWorkspaces "123456789"
      , (otherModMasks, action) <- [ ("", windows . W.view) -- was W.greedyView
                                      , ("S-", windows . W.shift)]
         ]

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myLayout = ( avoidStruts
           . smartBorders
           . smartSpacingWithEdge space
           $ tiled
         ||| Mirror tiled
         )
       ||| noBorders Full
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
mpdL = (fmap . fmap) (shorten 110)
     . logCmd
     $ "echo $(playerctl metadata title)\
       \ - $(playerctl metadata album) - $(playerctl metadata artist)"

volL :: Logger
volL = logCmd "amixer sget Master | egrep -o \"[0-9]+%\\] \\[[a-z]+\\]\" | head -n 1"

-- | Custom volume logger to change icon depending on volume level
volumeIconL :: Logger
volumeIconL = (fmap . fmap) volumeIcon volL

-- | Get the correct icon for the battery
volumeIcon :: String -> String
volumeIcon x
    | mute == "[off]"  = "\xf026  " ++ "MUTE"
    | read volNum > 50 = "\xf028  " ++ volNum ++ "%"
    | read volNum > 0  = "\xf027  " ++ volNum ++ "%"
    | otherwise        = "\xf026  " ++ volNum ++ "%"
  where
    mute   = drop 1 . dropWhile (/= ' ') $ x
    volNum = takeWhile (/= '%') x

-- | Custom battery logger to change icon depending on battery level
batteryIconL :: Logger
batteryIconL = (fmap . fmap) batteryIcon battery

-- | Get the correct icon for the battery
batteryIcon :: String -> String
batteryIcon x
    | bat > 90  = "\xf240  " ++ x
    | bat > 60  = "\xf241  " ++ x
    | bat > 30  = "\xf242  " ++ x
    | bat > 5   = "\xf243  " ++ x
    | otherwise = "\xf244  " ++ x
  where
    bat = read . reverse . takeWhile (/= ' ') . drop 1 . dropWhile (/= '%') . reverse $ x

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
                  , ppOrder   = (:) ("%{Sl}" ++ barColor (colors "white") "" ++ barBColor (colors "black") "")
                  , ppExtras  = [ appendLog False ("%{c}" ++ barColor (colors "darkblue") "\xf001  ") mpdL
                                , appendLog True ("%{r}" ++ barColor (colors "darkmagenta") "") volumeIconL
                                , appendLog True (barColor (colors "darkred") "") batteryIconL
                                , appendLog False (barColor (colors "lightgrey") "\xf017  ") $ date "%a %b %d %T"
                                ]
                  }

myStartup :: X ()
myStartup = do
    -- Delay between button presses
    spawnOnce "xset r rate 220"
    -- No black screen after inactivity
    spawnOnce "xset -dpms"
    spawnOnce "xset s off"
    -- Caps as control
    spawnOnce "setxkbmap -option ctrl:nocaps"
    -- Compositor
    spawnOnce "compton"
    -- Cursor
    spawnOnce "xsetroot -cursor_name left_ptr"
    -- Random background each restart
    spawn "feh --randomize --bg-fill ~/Dropbox/Desktops/*"
    -- Notifications
    spawnOnce "twmnd"
    -- NetworkManager applet
    spawnOnce "nm-applet"
    -- Dropbox
    spawnOnce "dropbox"
    -- mopidy
    spawnOnce "mopidy"
    -- Locking
    spawnOnce "xscreensaver -no-splash"

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
