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
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Timer
import Graphics.X11.ExtraTypes.XF86

-- wrapper for the Timer id, so it can be stored as custom mutable state
data TidState = TID TimerId deriving Typeable

instance ExtensionClass TidState where
  initialValue = TID 0

main = do
    h <- spawnPipe "bar-aint-recursive -p -g 3000x50+100+20 -d -f \"PragmataPro:size=11\""
    xmonad $ defaultConfig
        { terminal    = "konsole"
        , borderWidth = 8
        , layoutHook  = myLayout
        , logHook     = myLogHook h
        , startupHook = myStartup <+> clockStartupHook
        , handleEventHook = fullscreenEventHook <+> docksEventHook <+> clockEventHook
        , manageHook = fullscreenManageHook <+> manageDocks
        , normalBorderColor  = "#666666"
        , focusedBorderColor = "white"
        } `additionalKeys`
          [ (( mod1Mask, xK_p), spawn "synapse") -- to open synapse
          , (( mod4Mask, xK_l), spawn "gdmflexiserver") -- to lock
          , (( 0, xF86XK_AudioMute), spawn "amixer -q sset Master toggle") -- toggle mute
          , (( 0, xF86XK_AudioRaiseVolume), spawn "amixer -q sset Master 5%+") -- raise volume
          , (( 0, xF86XK_AudioLowerVolume), spawn "amixer -q sset Master 5%-") -- lower volume
          , (( 0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10") -- raise brightness
          , (( 0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10") -- lower brightness
          ]

myLayout = ( avoidStruts
           . spacing space
           . gaps [(U, space), (D, space), (L, space), (R, space)] $ tiled )
       ||| ( avoidStruts
           . spacing space
           . gaps [ (U, space)
                  , (D, space)
                  , (L, space)
                  , (R, space)] $ Mirror tiled )
       ||| smartBorders Full
  where
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
mpdL = logCmd "mpc -f \"%title% - %album% - %artist%\" | head -n 1"

volL :: Logger
volL = logCmd "amixer sget Master | egrep -o \"[0-9]+%\" | head -n 1"

strL :: String -> Logger
strL = return . Just

-- gruvbox color scheme
myLogHook h = dynamicLogWithPP
            $ defaultPP { ppOutput = hPutStrLn h
                        , ppSep    = " "
                        , ppLayout = (\_ -> "")
                        , ppOrder  = (:) "%{F#ebdbb2}%{B#282828} "
                        , ppExtras = [ strL "%{c}%{F#83a598}"
                                     , mpdL
                                     , strL "%{r}%{F#d79921}V"
                                     , volL
                                     , strL "%{F#d3869b} B"
                                     , battery
                                     , strL "%{F#8ec07c}"
                                     , date "%a %b %d %T"
                                     , strL " " ]
                        }

-- For updating panel
clockEventHook e = do               -- e is the event we've hooked
    (TID t) <- XS.get                 -- get the recent Timer id
    handleTimer t e $ do              -- run the following if e matches the id
        startTimer 1 >>= XS.put . TID   -- restart the timer, store the new id
        ask >>= logHook.config          -- get the loghook and run it
        return Nothing                  -- return required type
    return $ All True                 -- return required type

-- start the initial timer, store its id | for updating panel
clockStartupHook = startTimer 1 >>= XS.put . TID

myStartup :: X ()
myStartup = do
        spawn "feh --randomize --bg-fill /home/gw/Dropbox/Desktops/minimal/*"
        spawn "compton -c -C -G -f -e 0.5 --no-fading-openclose"
        spawn "pulseaudio"
        spawn "stalonetray"
        spawn "nm-applet"
        spawn "dropboxd"
