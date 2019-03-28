-- Standard
import Data.List
import Data.Monoid

-- Cabal
import DBus.Client
import Graphics.X11 (openDisplay)
import Graphics.X11.Xinerama (getScreenInfo)
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.GridSelect
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

type Height = Int
type Chassis = Int

data Device = Desktop | Laptop

-- | Be sure to choose the correct option: HD vs. UHD and Desktop vs.
-- Laptop
main :: IO ()
main = do
    height <- fmap (fromIntegral . rect_height . head)
            $ openDisplay [] >>= getScreenInfo :: IO Int
    chassis <- fmap read $ readFile "/sys/class/dmi/id/chassis_type"
    xmonad . myConfig height $ getDevice chassis

myConfig height dev = desktopConfig
    { modMask            = mod4Mask
    , terminal           = "kitty"
    , borderWidth        = borderRes height
    , workspaces         = myWorkspaces
    , manageHook         = namedScratchpadManageHook scratchpads
                       <+> manageHook desktopConfig
    , layoutHook         = smartBorders . myLayout $ height
    , handleEventHook    = handleEventHook def
    , normalBorderColor  = colors "black"
    , focusedBorderColor = colors "darkred"
    } `additionalKeysP` myKeys height dev

-- | Determine the device of the system.
getDevice :: Chassis -> Device
getDevice chassis | elem chassis [8, 9, 10, 11, 14] = Laptop
                  | otherwise = Desktop

-- | Define the border width
borderRes :: Height -> Dimension
borderRes = fromIntegral . round . (/ 200) . fromIntegral

-- My shortcuts. Also changes greedyView to view for multiple monitors
myKeys :: Height -> Device -> [(String, X ())]
myKeys height dev =
    [ ("M4-p", spawn . rofiRunCommand $ height) -- open program
    , ("M4-o", spawn . rofiWindowCommand $ height) -- switch window
    , ("M4-z", sendMessage MirrorShrink) -- lower bottom focused right column
    , ("M4-a", sendMessage MirrorExpand) -- raise bottom focused right column
    , ("M4-C-l", spawn "xscreensaver-command --lock") -- to lock
    , ("M4-C-<End>", spawn "amixer -q sset Capture toggle") -- toggle mute mic
    , ("M4-r", restart "xmonad" True) -- to restart without recompile
    , ("M4-g", goToSelected . myGSConfig $ height) -- grid select
    , ("M4-x", spawn "xkill") -- kill program with mouse
    , ("M4-C-e", namedScratchpadAction scratchpads "editor") --scratchpad
    , ("M4-C-m", namedScratchpadAction scratchpads "music") --scratchpad
    , ("M4-C-s", namedScratchpadAction scratchpads "slack") --scratchpad
    , ("M4-C-k", namedScratchpadAction scratchpads "keepass") --scratchpad
    , ("M4-c", placeFocused . fixed $ (0.5, 0.5)) -- center window
    , ("M4-v", windows copyToAll) -- Make focused window always visible
    , ("M4-S-v", killAllOtherCopies) -- Toggle window state back
    , ("M4-C-c", spawn "killall compton || compton --config ~/.config/compton.conf &") -- toggle compositor
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
                                 , ("S-", windows . W.shift)
                                 ]
    ]
  ++ resolutionKeys dev

-- | Assign keys based on laptop UHD or HD
resolutionKeys :: Device -> [(String, X ())]
resolutionKeys Desktop =
    [ ("M1-<End>", spawn "amixer -q sset Master toggle") -- toggle mute
    , ("M1-=", spawn "amixer -q sset Master 5%+") -- raise volume
    , ("M1--", spawn "amixer -q sset Master 5%-") -- lower volume
    ]
resolutionKeys Laptop  =
    [ ("<XF86AudioMute>", spawn "amixer -q sset Master toggle") -- toggle mute
    , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 5%+") -- raise volume
    , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 5%-") -- lower volume
    , ("<XF86MonBrightnessUp>", spawn "light -A 5") -- raise brightness
    , ("<XF86MonBrightnessDown>", spawn "light -U 5") -- lower brightness
    ]

-- | The names of the workspaces
myWorkspaces :: [String]
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myLayout height = (avoidStruts . smartSpacingWithEdge (space height) $ tiled)
              ||| Full
  where
    -- Space between windows
    space = fromIntegral . round . (/ 100) . fromIntegral
    -- default tiling algorithm partitions the screen into two panes
    tiled = ResizableTall nmaster delta ratio []

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio = 1/2

    -- Percent of screen to increment by when resizing panes
    delta = 3/100

scratchpads :: [NamedScratchpad]
scratchpads = [ NS "editor" "emacsclient -c -a \"\" -F '((name  . \"Emacs Scratchpad\"))'" (title =? "Emacs Scratchpad") scratchFloat
              , NS "music" "gpmdp" (className =? "Google Play Music Desktop Player") scratchFloat
              , NS "slack" "slack" (className =? "Slack") scratchFloat
              , NS "keepass" "keepass" (className =? "KeePass2") scratchFloat
              ]
  where
    scratchFloat = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)

myGSConfig height = def { gs_font = "xft:Open Sans Light-14"
                        , gs_cellheight = size
                        , gs_cellwidth = size * 3
                        }
  where
    size = fromIntegral . round . (/ 10) . fromIntegral $ height

tabbedTheme :: Height -> Theme
tabbedTheme height = def { activeColor         = colors "darkred"
                         , activeBorderColor   = colors "red"
                         , activeTextColor     = colors "white"
                         , inactiveColor       = colors "black"
                         , inactiveBorderColor = colors "grey"
                         , inactiveTextColor   = colors "white"
                         , urgentColor         = colors "darkmagenta"
                         , urgentBorderColor   = colors "magenta"
                         , urgentTextColor     = colors "white"
                         , fontName            = "xft:Open Sans-11"
                         , decoHeight          = fromIntegral . barSize $ height
                         }

-- | Size of the bar
barSize :: Height -> Int
barSize = round . (/ 40) . fromIntegral

-- | rofi run command. Goes in this order: "bg,fg,bgalt,hlbg,hlfg"
rofiRunCommand :: Height -> String
rofiRunCommand height = "rofi -show run -font '" ++ font height ++ "' "
                     ++ "-color-normal '"
                     ++ intercalate "," [ colors "background"
                                        , colors "foreground"
                                        , colors "background"
                                        , colors "darkblue"
                                        , colors "foreground"
                                        ]
                     ++ "' -color-window '"
                     ++ intercalate "," [ colors "background"
                                        , colors "background"
                                        ]
                     ++ "' -fuzzy -bw 0 -separator-style solid -fullscreen -padding " ++ padding height ++ " -eh 2 -opacity 90 -lines 6 -hide-scrollbar"
  where
    font = ("Open Sans Light " ++) . show . round . (/ 40) . fromIntegral
    padding = show . round . (/ 5) . fromIntegral

-- | rofi window command. Goes in this order: "bg,fg,bgalt,hlbg,hlfg"
rofiWindowCommand :: Height -> String
rofiWindowCommand height = "rofi -show window -font '" ++ font height ++ "' "
                     ++ "-color-normal '"
                     ++ intercalate "," [ colors "background"
                                        , colors "foreground"
                                        , colors "background"
                                        , colors "darkblue"
                                        , colors "foreground"
                                        ]
                     ++ "' -color-active '"
                     ++ intercalate "," [ colors "background"
                                        , colors "foreground"
                                        , colors "background"
                                        , colors "darkred"
                                        , colors "foreground"
                                        ]
                     ++ "' -color-window '"
                     ++ intercalate "," [ colors "background"
                                        , colors "background"
                                        ]
                     ++ "' -fuzzy -bw 0 -separator-style solid -bc '#282828' -fullscreen -padding " ++ padding height ++ " -eh 2 -opacity 90 -lines 6 -hide-scrollbar"
  where
    font = ("Open Sans Light " ++) . show . round . (/ 40) . fromIntegral
    padding = show . round . (/ 5) . fromIntegral

-- | Colors for everything
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
