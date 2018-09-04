{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

import System.Taffybar
import qualified System.Taffybar.Context as STC
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingLabel
import System.Taffybar.Widget.SNITray

import "lens-aeson" Data.Aeson.Lens
import Control.Exception (throwIO)
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Network.HTTP.Req
import Safe
import System.Process
import qualified GI.Gtk as Gtk
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
      clock   = textClockNew Nothing ("<span fgcolor='" ++ colors "lightgrey" ++ "'>" ++ fontAwesome "\xf017" ++ "  " ++ "%a %b %_d %H:%M:%S</span>") 1
      -- pager   = taffyPagerNew myPagerConfig
      workspaces = workspacesNew defaultWorkspacesConfig
      tray    = sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt
      music   = customW 1 musicString
      battery = customW 30 batString
      vol     = customW 1 volString
      train   = customW 60 . trainString "Departure" $ "Arrival"
      notify  = notifyAreaNew myNotificationConfig
      sep     = textW . T.pack . colorize (colors "darkgrey") "" $ "  /  "
      buffer  = textW "  "

      batDev Desktop = []
      batDev Laptop  = [battery, sep]

      startW  = [workspaces]
      endW    = [notify, buffer, tray, buffer, clock, sep]
             ++ batDev dev
             ++ [vol, sep, music]

  simpleTaffybar
    $ defaultSimpleTaffyConfig { startWidgets  = startW
                               , endWidgets    = endW
                               , barHeight     = barSize res
                               , barPosition   = Bottom
                               , widgetSpacing = 0
                               }

-- myPagerConfig =
--     defaultPagerConfig { activeWindow     = const ""
--                        , activeLayout     = const ""
--                        , activeWorkspace  = colorize (colors "darkred") ""
--                                           . escape
--                        , hiddenWorkspace  = colorize (colors "darkblue") ""
--                                           . escape
--                        , visibleWorkspace = colorize (colors "darkgreen") ""
--                                           . escape
--                        , urgentWorkspace  = colorize (colors "darkmagenta") ""
--                                           . escape
--                        , widgetSep        = ""
--                        }
myNotificationConfig :: NotificationConfig
myNotificationConfig = defaultNotificationConfig { notificationFormatter = mconcat . fmap myFormatter
                                                 , notificationMaxLength = 40
                                                 }

myFormatter :: Notification -> T.Text
myFormatter note = msg
  where
    msg = case T.null (noteBody note) of
            True -> noteSummary note
            False -> mconcat [ mconcat ["<span fgcolor='", T.pack (colors "red"), "'>"]
                             , noteSummary note
                             , " | "
                             , noteBody note
                             , "</span>"
                             ]

-- | Returns text as a widget
textW :: MonadIO m => T.Text -> m Gtk.Widget
textW x = do
    label <- liftIO $ Gtk.labelNew (Nothing :: Maybe T.Text)
    liftIO $ Gtk.labelSetMarkup label x

    l <- Gtk.toWidget label

    liftIO $ Gtk.widgetShowAll l
    return l

-- | A simple textual battery widget that auto-updates once every
-- polling period (specified in seconds).
customW :: Double -- ^ Poll period in seconds
        -> IO T.Text
        -> STC.TaffyIO Gtk.Widget
customW interval f = do
    l <- pollingLabelNew "" interval f
    liftIO $ Gtk.widgetShowAll l
    return l

-- | Returns the MPRIS string.
musicString :: IO T.Text
musicString = do
    (_, artist, _) <- readProcessWithExitCode "playerctl" ["metadata", "artist"] []
    (_, album, _) <- readProcessWithExitCode "playerctl" ["metadata", "album"] []
    (_, title, _) <- readProcessWithExitCode "playerctl" ["metadata", "title"] []

    -- let format = escape . take 90 $ title ++ " - " ++ album ++ " - " ++ artist
    let format = take 90 $ title ++ " - " ++ album ++ " - " ++ artist
        music  = colorize
                 (colors "darkblue")
                 ""
                 (fontAwesome "\xf001" ++ "  " ++ format)

    return . T.pack $ music

-- | Returns the battery text
batString :: IO T.Text
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
                . (flip (++) (charge batState))
                . batteryIcon
                $ batPercent

    return . T.pack $ battery

-- | Returns the volume string.
volString :: IO T.Text
volString = do
    output1 <- readProcess "amixer" ["sget", "Master"] []
    output2 <- readProcess "egrep" ["-o", "[0-9]+%\\] \\[[a-z]+\\]"] output1
    output3 <- readProcess "head" ["-n", "1"] output2

    let volume = colorize (colors "darkmagenta") "" . volumeIcon $ output3

    return . T.pack $ volume

-- | Returns train information from SEPTA.
trainString :: T.Text -> T.Text -> IO T.Text
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

    return . T.pack $ train

-- | Get the correct icon for the battery
volumeIcon :: String -> String
volumeIcon x
    | mute == "[off]"  = fontAwesome "\xf026" ++ "  " ++ "MUTE"
    | read volNum > 50 = fontAwesome "\xf028" ++ "  " ++ volNum ++ "%"
    | read volNum > 0  = fontAwesome "\xf027" ++ "  " ++ volNum ++ "%"
    | otherwise        = fontAwesome "\xf026" ++ "  " ++ volNum ++ "%"
  where
    mute   = dropWhile (/= '[') . reverse . dropWhile (/= ']') . reverse $ x
    volNum = takeWhile (/= '%') x

-- | Get the correct icon for the battery
batteryIcon :: String -> String
batteryIcon x
    | bat > 90  = fontAwesome "\xf240" ++ "  " ++ show bat ++ "%"
    | bat > 60  = fontAwesome "\xf241" ++ "  " ++ show bat ++ "%"
    | bat > 40  = fontAwesome "\xf242" ++ "  " ++ show bat ++ "%"
    | bat > 10  = fontAwesome "\xf243" ++ "  " ++ show bat ++ "%"
    | otherwise = fontAwesome "\xf244" ++ "  " ++ show bat ++ "%"
  where
    bat = read . reverse . takeWhile (/= ' ') . drop 1 . dropWhile (/= '%') . reverse $ x

-- | Change the font to font awesome here
fontAwesome :: String -> String
fontAwesome x = "<span font_desc='Font Awesome 5 Free'>" ++ x ++ "</span>"

-- | Size of the bar
barSize :: Resolution -> Int
barSize HD  = 45
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
