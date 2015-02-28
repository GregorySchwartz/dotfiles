import XMonad
import XMonad.Util.EZConfig
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks
import Graphics.X11.ExtraTypes.XF86

main = xmonad $ defaultConfig
    { terminal    = "konsole"
    , borderWidth = 8
    , layoutHook  = myLayout
    , startupHook = myStartup
    , handleEventHook = fullscreenEventHook <+> docksEventHook
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

myStartup :: X ()
myStartup = do
        spawn "feh --randomize --bg-fill /home/gw/Dropbox/Desktops/minimal/*"
        spawn "compton -c -r 20 -f -D 6 -e 0.5"
        spawn "bash /home/gw/scripts/bar_info.sh | bar-aint-recursive -p -g 3000x50+100+20 -d -f \"PragmataPro:size=11\""
        spawn "pulseaudio"
        spawn "stalonetray"
        spawn "nm-applet"
        spawn "dropboxd"
