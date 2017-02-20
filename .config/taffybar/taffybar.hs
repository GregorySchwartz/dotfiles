{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

import System.Information.Battery
import System.Information.CPU
import System.Taffybar
import System.Taffybar.Battery
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.MPRIS2
import System.Taffybar.Pager
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.Widgets.PollingLabel

import Control.Exception (throwIO)
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Network.HTTP.Req
import Safe
import System.Process
import qualified "gtk" Graphics.UI.Gtk as Gtk
import qualified Data.Text as T
import qualified System.IO as IO

data Resolution = HD | UHD
data Device     = Desktop | Laptop

instance MonadHttp IO where
    handleHttpException = throwIO

main :: IO ()
main = do
  let dev     = Desktop
      res     = HD
      clock   = textClockNew Nothing ("<span fgcolor='" ++ colors "lightgrey" ++ "'>" ++ fontAwesome "\xf017  " ++ "%a %b %_d %H:%M:%S</span>") 1
      pager   = taffyPagerNew myPagerConfig
      tray    = systrayNew
      music   = customW 1 musicString
      battery = customW 30 batString
      vol     = customW 1 volString
      train   = customW 60 . trainString "Departure" $ "Arrival"
      notify  = notifyAreaNew myNotificationConfig
      sep     = textW . colorize (colors "darkgrey") "" $ "  /  "
      buffer  = textW "  "

      batDev Desktop = []
      batDev Laptop  = [battery, sep]

      monitorDev Desktop x = x
      monitorDev Laptop x  = x { monitorNumber = 1 }

      startW  = [pager]
      endW    = [notify, buffer, tray, buffer, clock, sep]
             ++ batDev dev
             ++ [vol, sep, music, sep, train]

  defaultTaffybar
    . monitorDev dev
    $ defaultTaffybarConfig { startWidgets  = startW
                            , endWidgets    = endW
                            , barHeight     = barSize res
                            , barPosition   = Bottom
                            , widgetSpacing = 0
                            }

myPagerConfig =
    defaultPagerConfig { activeWindow     = const ""
                       , activeLayout     = const ""
                       , activeWorkspace  = colorize (colors "darkred") ""
                                          . escape
                       , hiddenWorkspace  = colorize (colors "darkblue") ""
                                          . escape
                       , visibleWorkspace = colorize (colors "darkgreen") ""
                                          . escape
                       , urgentWorkspace  = colorize (colors "darkmagenta") ""
                                          . escape
                       , widgetSep        = ""
                       }
myNotificationConfig :: NotificationConfig
myNotificationConfig = defaultNotificationConfig { notificationFormatter = myFormatter
                                                 , notificationMaxLength = 40
                                                 }

myFormatter :: Notification -> String
myFormatter note = msg
  where
    msg = case T.null (noteBody note) of
            True -> T.unpack $ noteSummary note
            False -> T.unpack
                   $ mconcat [ mconcat ["<span fgcolor='", T.pack (colors "red"), "'>"]
                             , noteSummary note
                             , " | "
                             , noteBody note
                             , "</span>"
                             ]

-- | Returns text as a widget
textW :: String -> IO Gtk.Widget
textW x = do
    label <- Gtk.labelNew (Nothing :: Maybe String)
    Gtk.labelSetMarkup label x

    let l = Gtk.toWidget label

    Gtk.widgetShowAll l
    return l

-- | A simple textual battery widget that auto-updates once every
-- polling period (specified in seconds).
customW :: Double -- ^ Poll period in seconds
        -> IO String
        -> IO Gtk.Widget
customW interval f = do
    l <- pollingLabelNew "" interval f
    Gtk.widgetShowAll l
    return l

-- | Returns the MPRIS string.
musicString :: IO String
musicString = do
    (_, artist, _) <- readProcessWithExitCode "playerctl" ["metadata", "artist"] []
    (_, album, _) <- readProcessWithExitCode "playerctl" ["metadata", "album"] []
    (_, title, _) <- readProcessWithExitCode "playerctl" ["metadata", "title"] []

    let format = escape . take 90 $ title ++ " - " ++ album ++ " - " ++ artist
        music  = colorize
                 (colors "darkblue")
                 ""
                 (fontAwesome "\xf001  " ++ format)

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
            | isInfixOf "DISCHARGING" x = "-"
            | isInfixOf "CHARGING" x    = "+"
            | isInfixOf "UNKNOWN" x     = "+"
            | otherwise                 = ""
        battery = colorize (colors "darkred") ""
                . (flip (++) (charge batInfo))
                . batteryIcon
                $ batPercent

    return battery

-- | Returns the volume string.
volString :: IO String
volString = do
    output1 <- readProcess "amixer" ["sget", "Master"] []
    output2 <- readProcess "egrep" ["-o", "[0-9]+%\\] \\[[a-z]+\\]"] output1
    output3 <- readProcess "head" ["-n", "1"] output2

    let volume = colorize (colors "darkmagenta") "" . volumeIcon $ output3

    return volume

-- | Returns train information from SEPTA.
trainString :: T.Text -> T.Text -> IO String
trainString start end = do
    let base = https "www3.septa.org" /: "hackathon" /: "NextToArrive"

    res <- req GET (base /: start /: end /: "2") NoReqBody jsonResponse mempty

    let departures = fmap T.strip
                   . toListOf
                        (values . key "orig_departure_time" . _String)
                   $    (responseBody res :: Value)
        arrivals   = fmap T.strip
                   . toListOf
                        (values . key "orig_arrival_time" . _String)
                   $    (responseBody res :: Value)
        delays     = toListOf
                        (values . key "orig_delay" . _String)
                        (responseBody res :: Value)
        output     = mconcat
                   $ [start, " to ", end, " - ", T.intercalate " | " $
                        zipWith3 (\ depart arrive delay -> T.intercalate "   "
                                                        $ [ "D" <> depart
                                                          , "A" <> arrive
                                                          , delay
                                                          ]
                                )
                                departures
                                arrivals
                                delays
                     ]

    let train = colorize (colors "lightgrey") "" . T.unpack $ output

    return train

-- | Get the correct icon for the battery
volumeIcon :: String -> String
volumeIcon x
    | mute == "[off]"  = fontAwesome "\xf026  " ++ "MUTE"
    | read volNum > 50 = fontAwesome "\xf028  " ++ volNum ++ "%"
    | read volNum > 0  = fontAwesome "\xf027  " ++ volNum ++ "%"
    | otherwise        = fontAwesome "\xf026  " ++ volNum ++ "%"
  where
    mute   = dropWhile (/= '[') . reverse . dropWhile (/= ']') . reverse $ x
    volNum = takeWhile (/= '%') x

-- | Get the correct icon for the battery
batteryIcon :: String -> String
batteryIcon x
    | bat > 90  = fontAwesome "\xf240  " ++ show bat ++ "%"
    | bat > 60  = fontAwesome "\xf241  " ++ show bat ++ "%"
    | bat > 40  = fontAwesome "\xf242  " ++ show bat ++ "%"
    | bat > 10  = fontAwesome "\xf243  " ++ show bat ++ "%"
    | otherwise = fontAwesome "\xf244  " ++ show bat ++ "%"
  where
    bat = read . reverse . takeWhile (/= ' ') . drop 1 . dropWhile (/= '%') . reverse $ x

-- | Change the font to font awesome here
fontAwesome :: String -> String
fontAwesome x = "<span font_desc='FontAwesome'>" ++ x ++ "</span>"

-- | Size of the bar
barSize :: Resolution -> Int
barSize HD  = 25
barSize UHD = 55

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
