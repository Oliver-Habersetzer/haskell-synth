module Live (
    live
) where

import Control.Monad
import System.IO
import Control.Monad.IO.Class
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import System.Glib.UTFString
import Data.Char
import FileHandler
import Key
import Render
import Instrument
import Oscilators
import System.Directory
import Playback
import Control.Concurrent
import Data.Typeable

--keyConv :: Data.Text.Internal.Text -> Key
keyConv key = do
    let keyString = safeKeyString $ glibToString key
    if (length keyString) == 1 then do
        let c = keyString !! 0
        let musicalKey = qwertzToKey c 0
        musicalKey
    else do
        NoKey

keyToIndex :: Key -> Maybe Int
keyToIndex (Key bk oct) = Just $ fromIntegral $ floor (fromIntegral oct * 12 + (baseKeyIndex bk) + 9)
keyToIndex _ = Nothing

safeKeyString str
        | str == "comma" = ","
        | str == "period" = "."
        | str == "minus" = "-"
        | otherwise = str
live :: [Instrument] -> [Char] -> Double -> p -> IO ()
live instruments defaultInstrument tuning stereoMode = do
    let filteredInstruments = filter (\(Instrument name _ _ ) -> name == defaultInstrument) instruments
    if (length filteredInstruments) == 0 then do
        error $ "Could not find instrument " ++ defaultInstrument
    else do
        let osc = fromInstrument $ filteredInstruments !! 0
        let keys = [Key bk o | o <- [0..9], bk <- baseKeys]
        putStrLn $ "Selected oscilator: " ++ (show osc)
        createDirectoryIfMissing True "./samples"
        mapM_ (\k -> renderKey osc k tuning) keys
        initPlayback
        samples <- loadSamples $ map keyPath keys
        writeFile "./.hs-synth-tmp" ""
        initGUI
        window <- windowNew
        set window [  windowTitle         := "Synthi"
                    , windowResizable     := False ]

        window `on` deleteEvent $ liftIO mainQuit >> return False
        image <- imageNewFromFile "./resources/qwertz-layout.png"
        containerAdd window image
        widgetShowAll window
        windowSetKeepAbove window True

        window `on` keyReleaseEvent $ tryEvent $ do
            key <- Graphics.UI.Gtk.Gdk.EventM.eventKeyName
            let k = keyConv key
            let i = keyToIndex k
            case i of
                Just _i -> do
                  liftIO $ deleteNote $ show _i
                  sampleList <- liftIO getTMP
                  liftIO $ stopSamples
                  let sample = map (read :: String -> Int) sampleList
                  mapM_ (\x -> liftIO $ playSample (samples !! x ) True False ) sample
                  return ()
                Nothing -> return ()
            return ()

        window `on` keyPressEvent $ tryEvent $ do
            key <- Graphics.UI.Gtk.Gdk.EventM.eventKeyName
            let k = keyConv key
            let i = keyToIndex k
            case i of
                Just _i -> do
                  bool <- liftIO $ saveNote $ show _i
                  if bool
                    then
                      liftIO $ playSample (samples !! _i) True False
                    else return ()
                Nothing -> return ()
            return ()

        widgetShowAll window
        mainGUI
