module Live (
    live
) where

import Control.Monad
import Control.Monad.IO.Class
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import System.Glib.UTFString
import Data.Char

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
        liftIO $ putStrLn (glibToString key ++ " pressed ")

    widgetShowAll window
    mainGUI