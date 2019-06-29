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

live = do
    initGUI
    window <- windowNew
    set window [  windowTitle         := "Synthi"
                , windowResizable     := False
                , windowDefaultWidth  := 400
                , windowDefaultHeight := 400 ]
    
    window `on` deleteEvent $ liftIO mainQuit >> return False
    window `on` focus $ \dirtype -> putStrLn "focused!" >> return False
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