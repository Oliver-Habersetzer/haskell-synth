module MidiConverter (
    convertMidiToScoreFile
) where

import Score
import Data.List
import Data.Maybe
import qualified Sound.MIDI.File.Load as Load
import Sound.MIDI.File
import Data.Typeable
import qualified Sound.MIDI.File.Event as Event
import qualified Sound.MIDI.File.Event.Meta as MetaEvent
import qualified Data.EventList.Relative.TimeBody as EventList
import Sound.MIDI.File.Event.Meta (ElapsedTime, fromElapsedTime)
import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as Voice
import System.IO

data NoteAction = On | Off deriving Show

{-
MIDI-tempo / time signature: http://www.deluge.co/?q=midi-tempo-bpm
-}

fst3 (v, _, _) = v
snd3 (_, v, _) = v

convertMidiToScoreFile :: String -> String -> IO Scores
convertMidiToScoreFile inPath outPath = do
    -- load & decode midi
    mDat <- Load.fromFile inPath :: IO T

    -- write temp file (for debugging)
    let shown = showLines mDat
    writeFile (inPath ++ ".decoded") shown

    -- get timecode info
    let ppqn = getTempo mDat

    -- get tracks
    let tracks = getTracks mDat
    let parsedTracks = filter filterEmptyTracks $ map parseTrack tracks

    let noteTracks =
            map (\(_, _, events) -> events)
            $ filter (\(_, _, events) -> isNotEmpty events) parsedTracks
    let noteTrackC = length noteTracks
    let timeSignature =
            fromMaybe ((-1), (-1), (-1), (-1)) $
            just2to1 $
            fst3 <$>
            (maybeFirst $ filter (\(timeSignature, _, _) -> isJust timeSignature) parsedTracks)
    let setTempo =
            fromMaybe (-1) $
            just2to1 $
            snd3 <$>
            (maybeFirst $ filter (\(_, setTempo, _) -> isJust setTempo) parsedTracks)

    putStrLn "\nEnter the names of the tracks with notes.\nYou can enter sine, saw, square or triangle for a corressponding oscilators\n"
    instrumentNames <- getInstrumentNames noteTrackC noteTrackC

    mapM_ (\t -> putStrLn $ show t) noteTracks

    putStrLn $ "Tempo: " ++ (show ppqn) ++ " Pulses/quarter note"
    putStrLn $ "Tracks with notes: " ++ (show noteTrackC)
    putStrLn $ "Instrument names: " ++ (show instrumentNames)
    putStrLn $ "Time signature: " ++ (show timeSignature)
    putStrLn $ "Set tempo: " ++ (show setTempo)

    -- convert timecode
    let bpm = 60000000.0 / (fromIntegral setTempo)
    let bpb = (\(v, _, _, _) -> v) timeSignature :: Int
    let dpb = ppqn
    putStrLn "\nCalculated timecode:"
    putStrLn $ "  Beats per minute: " ++ (show bpm)
    putStrLn $ "  Beats per bar: " ++ (show bpb)
    putStrLn $ "  Divisions per beat: " ++ (show dpb)

    -- return
    return $ Scores bpm bpb dpb [] []

    where
        getInstrumentNames 0 c = return []
        getInstrumentNames n c = do 
            putStr $ "Instrument name for channel " ++ (show (1 + c - n)) ++ ": "
            hFlush stdout
            input <- getLine
            moreinputs <- getInstrumentNames (n - 1) c
            return (input : moreinputs)
    

        filterEmptyTracks (Nothing, Nothing, []) = False
        filterEmptyTracks _ = True

        getTempo (Cons _ (Ticks tempo) _) = (fromIntegral tempo) :: Int
        getTempo (Cons _ (SMPTE a b) _) = do
            error "SMPTE time mode currently not supported"

        parseTrack eventList = do
            let asdf = EventList.viewR eventList

            let _times = EventList.getTimes eventList
            let times = [sum $ take i _times | i <- [1..(length _times)]]

            let bodies = EventList.getBodies eventList
            let zippedEvents = zip times bodies
            let metaOnlyBodies = and $ (map isMetaEvent bodies)
            let timeSig = getTimeSig <$> (maybeFirst (filter isTimeSigEvent bodies))
            let setTempo = getSetTempo <$> (maybeFirst (filter isSetTempoEvent bodies))
            let noteEvents =
                    map toSafeNoteEvent 
                    $ map (\(t, b) -> (t, getNoteEvent b)) 
                    $ filter (\(t, b) -> isNoteEvent b)
                    zippedEvents
            
            {-
            putStrLn $ "    Track:"
            putStrLn $ "      Track times / bodies: [" ++ (show $ length bodies) ++ "]"
            putStrLn $ "      Meta only bodies: " ++ (show metaOnlyBodies)
            onJust timeSig (\(timeSig) -> putStrLn $ "      Time signature: " ++ (show timeSig))
            onJust setTempo (\(setTempo) -> putStrLn $ "      Set tempo: " ++ (show setTempo))
            if metaOnlyBodies then
                mapM_ (\body -> putStrLn $ "        Body: " ++ (show body)) bodies
            else
                putStrLn "        [...]"
            putStrLn $ "      Note events: " ++ (show $ length noteEvents)
            putStrLn $ "        " ++ (show noteEvents)

            -}

            (timeSig, setTempo, noteEvents)

        getTimeSig (Event.MetaEvent (MetaEvent.TimeSig n d c b)) = (n, d, c, b)
        isTimeSigEvent (Event.MetaEvent (MetaEvent.TimeSig _ _ _ _)) = True
        isTimeSigEvent _ = False

        getSetTempo (Event.MetaEvent (MetaEvent.SetTempo tempo)) = tempo
        isSetTempoEvent (Event.MetaEvent (MetaEvent.SetTempo _)) = True
        isSetTempoEvent _ = False

        toSafeNoteEvent (time, (channel, On, pitch, 0)) = (fromElapsedTime time, channel, Off, pitch, 0)
        toSafeNoteEvent (time, (channel, Off, pitch, vol)) = (fromElapsedTime time, channel, Off, pitch, 0)
        toSafeNoteEvent (time, (channel, On, pitch, vol)) = (fromElapsedTime time, channel, On, pitch, vol)

        getNoteEvent (Event.MIDIEvent (ChannelMsg.Cons ch (ChannelMsg.Voice (Voice.NoteOff pitch vel)))) = (ChannelMsg.fromChannel ch, Off, ChannelMsg.fromPitch pitch, ChannelMsg.fromVelocity vel)
        getNoteEvent (Event.MIDIEvent (ChannelMsg.Cons ch (ChannelMsg.Voice (Voice.NoteOn pitch vel)))) = (ChannelMsg.fromChannel ch, On, ChannelMsg.fromPitch pitch, ChannelMsg.fromVelocity vel)
        isNoteEvent (Event.MIDIEvent (ChannelMsg.Cons ch (ChannelMsg.Voice (Voice.NoteOff _ _)))) = True
        isNoteEvent (Event.MIDIEvent (ChannelMsg.Cons ch (ChannelMsg.Voice (Voice.NoteOn _ _)))) = True
        isNoteEvent _ = False

        isMidiEvent (Event.MIDIEvent _) = True
        isMidiEvent _ = False

        isMetaEvent (Event.MetaEvent _) = True
        isMetaEvent _ = False

        maybeFirst l
                | length l == 0 = Nothing
                | otherwise = Just (l !! 0)

        onJust (Just x) action = action x
        onJust Nothing _ = return ()

        isNotEmpty [] = False
        isNotEmpty _ = True

        just2to1 (Just (Just v)) = Just v
        just2to1 _ = Nothing