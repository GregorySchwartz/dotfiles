{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

import "lens-aeson" Data.Aeson.Lens
import Control.Exception (throwIO)
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Graphics.X11 (openDisplay, Rectangle (..))
import Graphics.X11.Xinerama (getScreenInfo)
import Safe
import System.Process
import Xmobar
import qualified Data.Text as T
import qualified System.IO as IO
import qualified XMonad.StackSet as W

type Height = Int
type Chassis = Int

data Device     = Desktop | Laptop
data MyMusic   = MyMusic deriving (Read, Show)
data MyVolume  = MyVolume deriving (Read, Show)
data MyBattery = MyBattery deriving (Read, Show)

instance Exec MyMusic where
    alias MyMusic = "music"
    run   MyMusic = musicString

instance Exec MyVolume where
    alias MyVolume = "volume"
    run   MyVolume = volString

instance Exec MyBattery where
    alias MyBattery = "battery"
    run   MyBattery = batString

config :: Device -> Config
config dev = defaultConfig {
  font = "Open Sans Condensed 25"
  , additionalFonts = ["Font Awesome 6 Free Solid 25"]
  , borderColor = "black"
  , border = NoBorder
  , bgColor = colors "background"
  , fgColor = colors "foreground"
  , alpha = 255
  , position = BottomH 50
  , textOffset = -1
  , iconOffset = -1
  , lowerOnStart = True
  , pickBroadest = False
  , persistent = False
  , hideOnStart = False
  , iconRoot = "."
  , allDesktops = True
  , overrideRedirect = True
  , commands = [ Run StdinReader
               , Run MyVolume
               , Run $ Date "%a %b %_d %H:%M:%S" "date" 10
               , Run $ Mpris2 "gpmdp" ["-t", "<artist> - <album> - <title>"] 10
               , Run MyMusic
               , Run MyBattery
              ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "%StdinReader% }{ " <> colorize (colors "darkblue") "%music%" <> sep <> colorize (colors "darkmagenta") "%volume%" <> sep <> battery dev <> colorize (colors "lightgrey") (fontAwesome "\xf017" <> "  %date%")
}
  where
    sep = colorize (colors "darkgrey") "  /  "
    battery Laptop = "%battery%" <> sep
    battery Desktop = ""

main :: IO ()
main = do
  chassis <- fmap read $ readFile "/sys/class/dmi/id/chassis_type"
  let dev     = getDevice chassis
  xmobar (config dev)

-- | Determine the device of the system.
getDevice :: Chassis -> Device
getDevice chassis | elem chassis [8, 9, 10, 11, 14] = Laptop
                  | otherwise = Desktop

-- | Returns the MPRIS string.
musicString :: IO String
musicString = do
    (_, artist, _) <- readProcessWithExitCode "playerctl" ["metadata", "artist"] []
    (_, album, _) <- readProcessWithExitCode "playerctl" ["metadata", "album"] []
    (_, title, _) <- readProcessWithExitCode "playerctl" ["metadata", "title"] []

    -- let format = escape . take 90 $ title ++ " - " ++ album ++ " - " ++ artist
    let format = take 90 . filter (/= '\n') $ title ++ " - " ++ album ++ " - " ++ artist
        music  = colorize
                 (colors "darkblue")
                 (fontAwesome "\xf001" ++ "  " ++ format)

    return music

-- | Returns the battery text
batString :: IO String
batString = do
    batList <- fmap (headMay . filter (isInfixOf "battery") . lines)
             . readProcess "upower" ["-e"]
             $ []
    batInfo <- readProcess "upower" ["-i", fromMaybe "" batList] []

    let batPercent = filter (/= ' ')
                   . dropWhile (not . isNumber)
                   . fromMaybe ""
                   . headMay
                   . filter (isInfixOf "percentage:")
                   . lines
                   $ batInfo
        batState = fmap toUpper
                 . filter (/= ' ')
                 . dropWhile (/= ' ')
                 . dropWhile (== ' ')
                 . fromMaybe ""
                 . headMay
                 . filter (isInfixOf "state:")
                 . lines
                 $ batInfo
        charge :: String -> String
        charge x
            | isInfixOf "DISCHARGING" x   = "-"
            | isInfixOf "CHARGING" x      = "+"
            | isInfixOf "FULLY-CHARGED" x = "="
            | isInfixOf "UNKNOWN" x       = "+"
            | otherwise                   = ""
        battery = colorize (colors "darkred")
                . (flip (++) (charge batState))
                . batteryIcon
                $ batPercent

    return battery

-- | Returns the volume string.
volString :: IO String
volString = do
    output1 <- readProcess "amixer" ["sget", "Master"] []
    output2 <- readProcess "egrep" ["-o", "[0-9]+%\\] \\[[a-z]+\\]"] output1
    output3 <- readProcess "head" ["-n", "1"] output2

    let volume = colorize (colors "darkmagenta") . volumeIcon $ output3

    return volume

-- | Get the correct icon for the battery
volumeIcon :: String -> String
volumeIcon x
    | mute == "[off]"  = fontAwesome "\xf026" ++ "  " ++ "MUTE"
    | readMay volNum > Just 50 = fontAwesome "\xf028" ++ "  " ++ volNum ++ "%"
    | readMay volNum > Just 0  = fontAwesome "\xf027" ++ "  " ++ volNum ++ "%"
    | otherwise        = fontAwesome "\xf026" ++ "  " ++ volNum ++ "%"
  where
    mute   = dropWhile (/= '[') . reverse . dropWhile (/= ']') . reverse $ x
    volNum = takeWhile (/= '%') x

-- | Get the correct icon for the battery
batteryIcon :: String -> String
batteryIcon x
    | bat > Just 90  = fontAwesome "\xf240" ++ "  " ++ show (fromMaybe 0 bat) ++ "%"
    | bat > Just 60  = fontAwesome "\xf241" ++ "  " ++ show (fromMaybe 0 bat) ++ "%"
    | bat > Just 40  = fontAwesome "\xf242" ++ "  " ++ show (fromMaybe 0 bat) ++ "%"
    | bat > Just 10  = fontAwesome "\xf243" ++ "  " ++ show (fromMaybe 0 bat) ++ "%"
    | otherwise = fontAwesome "\xf244" ++ "  " ++ show bat ++ "%"
  where
    bat = readMay . reverse . takeWhile (/= ' ') . drop 1 . dropWhile (/= '%') . reverse $ x

-- | Change the font to font awesome here
fontAwesome :: String -> String
fontAwesome x = "<fn=1>" <> x <> "</fn>"

-- | Color format
colorize :: String -> String -> String
colorize x xs = "<fc=" <> x <> ">" <> xs <> "</fc>"

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
