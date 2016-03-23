

-- Standard
import Data.Monoid

import XMonad
import XMonad.Actions.GridSelect
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Run
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W

import DBus.Client
import System.Taffybar.Hooks.PagerHints (pagerHints)

main = do
    xmonad . ewmh . pagerHints $ myConfig

myConfig = def { terminal           = "konsole"
               , borderWidth        = 0
               , workspaces         = myWorkspaces
               , layoutHook         = noBorders myLayout
               , handleEventHook    = docksEventHook <+> fullscreenEventHook
               , manageHook         = manageDocks
               , normalBorderColor  = colors "black"
               , focusedBorderColor = colors "darkred"
               } `additionalKeysP` myKeys

-- My shortcuts. Also changes greedyView to view for multiple monitors
myKeys = [ ("M1-p", spawn "rofi -show run -font 'Open Sans 25' -bg '#282828' -fg '#ebdbb2' -hlbg '#458588' -hlfg '#ebdbb2' -fuzzy -bw 0 -separator-style solid -bc '#282828' -width 100 -padding 400 -eh 2 -opacity 90 -lines 6 -hide-scrollbar") -- open program
         , ("M1-o", spawn "rofi -show window -font 'Open Sans 25' -bg '#282828' -fg '#ebdbb2' -hlbg '#458588' -hlfg '#ebdbb2' -hlbg-active '#458588' -fuzzy -bw 0 -separator-style solid -bc '#282828' -width 100 -padding 400 -eh 2 -opacity 90 -lines 6 -hide-scrollbar") -- switch window
         , ("M1-z", sendMessage MirrorShrink) -- lower bottom focused right column
         , ("M1-a", sendMessage MirrorExpand) -- raise bottom focused right column
         , ("M1-C-l", spawn "xscreensaver-command --lock") -- to lock
         , ("M1-C-<End>", spawn "amixer -q sset Capture toggle") -- toggle mute mic
         , ("M1-r", restart "xmonad" True) -- to restart without recompile
         , ("M1-g", goToSelected myGSConfig) -- grid select
         , ("M1-<End>", spawn "amixer -q sset Master toggle") -- toggle mute
         , ("M1-=", spawn "amixer -q sset Master 5%+") -- raise volume
         , ("M1--", spawn "amixer -q sset Master 5%-") -- lower volume
         , ("M1-c", spawn "killall compton || compton --config ~/.config/compton.conf &") -- toggle compositor
         , ("C-<Home>", spawn "playerctl play-pause") -- mpd toggle play pause
         , ("C-<End>", spawn "playerctl stop") -- mpd stop
         , ("C-<Page_Up>", spawn "playerctl previous") -- mpd previous
         , ("C-<Page_Down>", spawn "playerctl next") -- mpd next
         , ("C-S-1", spawn "setxkbmap -v us") -- qwerty
         , ("C-S-2", spawn "setxkbmap -v us -variant colemak") -- colemak
         ]
      ++ [ (otherModMasks ++ "M-" ++ [key], action tag)
      | (tag, key)  <- zip myWorkspaces "123456789"
      , (otherModMasks, action) <- [ ("", windows . W.view) -- was W.greedyView
                                      , ("S-", windows . W.shift)]
         ]

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myLayout = ( avoidStruts
           . smartSpacingWithEdge space
           $ tiled
           )
       ||| Full
  where
    -- Space between windows
    space = 10
    -- default tiling algorithm partitions the screen into two panes
    tiled = ResizableTall nmaster delta ratio []

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio = 1/2

    -- Percent of screen to increment by when resizing panes
    delta = 3/100

myGSConfig = def { gs_font = "xft:Open Sans-14"
                 , gs_cellheight = 100
                 , gs_cellwidth = 300 }

tabbedTheme :: Theme
tabbedTheme = def { activeColor         = colors "darkred"
                  , activeBorderColor   = colors "red"
                  , activeTextColor     = colors "white"
                  , inactiveColor       = colors "black"
                  , inactiveBorderColor = colors "grey"
                  , inactiveTextColor   = colors "white"
                  , urgentColor         = colors "darkmagenta"
                  , urgentBorderColor   = colors "magenta"
                  , urgentTextColor     = colors "white"
                  , fontName            = "xft:Open Sans-11"
                  , decoHeight          = fromIntegral barSize
                  }

-- | Size of the bar
barSize :: Int
barSize = 25

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
