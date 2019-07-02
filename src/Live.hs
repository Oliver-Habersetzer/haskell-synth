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
import Key
import Render
import Instrument
import Oscilators
import System.Directory
import Playback

keyConv key = do
    let keyString = safeKeyString $ glibToString key
    if (length keyString) == 1 then do
        let c = keyString !! 0
        let musicalKey = qwertzToKey c 0
        musicalKey
    else do
        NoKey

safeKeyString str
        | str == "comma" = ","
        | str == "period" = "."
        | str == "minus" = "-"
        | otherwise = str

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
        samples <- loadSamples $ map keyPath keys

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
            let x = keyConv key
            liftIO $ putStrLn (show x ++ " released")

        window `on` keyPressEvent $ tryEvent $ do
            key <- Graphics.UI.Gtk.Gdk.EventM.eventKeyName
            let x = keyConv key
            liftIO $ putStrLn (show x ++ " pressed")

        widgetShowAll window
        mainGUI