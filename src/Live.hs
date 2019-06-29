module Live (
    live
) where

import Control.Monad
import Control.Monad.IO.Class
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import System.Glib.UTFString
import Data.Char
import Key

safeKeyString str
        | str == "comma" = ","
        | str == "period" = "."
        | str == "minus" = "-"
        | otherwise = str

live = do
    initGUI
    window <- windowNew
    window `on` deleteEvent $ liftIO mainQuit >> return False
    window `on` focus $ \dirtype -> putStrLn "focused!" >> return False

    window `on` keyReleaseEvent $ tryEvent $ do
        key <- Graphics.UI.Gtk.Gdk.EventM.eventKeyName
        liftIO $ putStrLn (glibToString key ++ " released ")

    window `on` keyPressEvent $ tryEvent $ do
        key <- Graphics.UI.Gtk.Gdk.EventM.eventKeyName
        let keyString = safeKeyString $ glibToString key
        liftIO $ putStrLn keyString
        if (length keyString) == 1 then do
            let c = keyString !! 0
            let musicalKey = qwertzToKey c 0
            liftIO $ putStrLn $ [c] ++ " -> " ++ (show musicalKey)
        else do
            liftIO $ putStrLn $ "Uninteresting key..."


    widgetShowAll window
    mainGUI