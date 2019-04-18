module Music (
  playMusic,
  playNote,
  myNote,
  playScale,
  mergeSort
) where


import Euterpea.IO.MIDI.MidiIO
import Data.Maybe
import Sound.PortMidi


--playScale :: (Enum a, Fractional a) => a -> [a] -> a -> (a, MidiMessage)
playScale tempo scale root =
  (\x->(snd x, myNote (fst x + root) (60/tempo))) <$> (zip scale [0, 60/tempo..])


playMusic :: IO ()
playMusic = do
  i <- getDefaultOutputDeviceID
  outputMidi (unsafeOutputID $ fromJust i)


playNote :: (Time, MidiMessage) -> IO ()
playNote mes = do
  i <- getDefaultOutputDeviceID
  deliverMidiEvent (unsafeOutputID $ fromJust i) mes


myNote :: Int -> Double -> MidiMessage
myNote x d = ANote 0 (20+40+x) 127 d


mergeSort :: (Ord i) => [i] -> [i]
-- Sorts a list into ascending order.
mergeSort ( [] ) = [ ]
mergeSort (x:[]) = [x]
mergeSort ( xs ) = merge
  (mergeSort $ take (length xs `div` 2) xs) 
  (mergeSort $ drop (length xs `div` 2) xs)


merge :: (Ord i) => [i] -> [i] -> [i]
-- Merges two ordered sublists into an ordered list.
merge (a:[]) (b:[]) = if a < b  then [a,b]              else [b,a]
merge (a:[]) (b:bx) = if a < b  then a:b:bx             else b:merge [a] bx
merge (a:ax) (b:[]) = if a < b  then a:merge [b] ax     else b:a:ax 
merge (a:ax) (b:bx) = if a < b  then a:merge ax (b:bx)  else b:merge (a:ax) bx
merge ( ma ) ( mb ) = ma ++ mb


-- Code Graveyard

--import Codec.Midi (Time, Channel, Key, Velocity, 
--                   Message (..), Midi (..), Track, 
--                   toRealTime, toAbsTime, toSingleTrack, isTrackEnd)
--import Data.Heap
--import Data.IORef
--import System.IO.Unsafe (unsafePerformIO)

--newtype OutputDeviceID = OutputDeviceID DeviceID
--  deriving (Eq, Show)

--type MidiEvent = (Time, MidiMessage)

--data MidiMessage = ANote { channel :: !Channel, key :: !Key,
--                          velocity :: !Velocity, duration :: !Time }
--                 | Std Message
--  deriving Show

--data PrioChannel a b = PrioChannel
--    { get           :: IO (MinPrioHeap a b),
--      push          :: a -> b -> IO (),
--      pop           :: IO (a,b),
--      peek          :: IO (Maybe (a,b)) }

--getOutDev :: OutputDeviceID -> IO (PrioChannel Time Message, (Time, Message) -> IO (), IO ())
--getOutDev devId = do
--  inits <- readIORef outDevMap
--  case lookup devId inits of
--    Just f -> return f
--    Nothing -> do
--        x <- midiOutRealTime' devId -- Changes made by Donya Quick: this line used to pattern match against Just.
--        pChan <- makePriorityChannel
--        case x of 
--          Just (mout,stop) -> do -- Case statement added.
--                modifyIORef outDevMap ((devId,(pChan,mout,stop)):)
--                return (pChan,mout,stop)
--          Nothing -> return (pChan, const (return ()), return ()) -- Nothing case added

--getTimeNow :: IO Time 
--getTimeNow = do
--  t <- time
--  return (fromIntegral t / 1000)

--outDevMap :: IORef [(OutputDeviceID, 
--                     (PrioChannel Time Message, -- priority channel
--                      (Time, Message) -> IO (), -- sound output function
--                      IO ()))]                  -- stop/terminate function
--outDevMap = unsafePerformIO $ newIORef []

--deliverMidiEvent :: OutputDeviceID -> MidiEvent -> IO ()
--deliverMidiEvent devId (t,m) = do
--  x <- midiOutRealTime' devId -- Changes made by Donya Quick: this line used to pattern match against Just.
--  pChan <- makePriorityChannel
--  case x of
--    Just (mout,stop) -> do -- Case statement added.
--          modifyIORef outDevMap ((devId,(pChan,mout,stop)):)
--          return (pChan,mout,stop)
--    Nothing -> return (pChan, const (return ()), return ())
--  inits <- readIORef outDevMap
--  (pChan, out, _stop) <- return (fromJust $ lookup devId inits) -- getOutDev devId
--  now <- getTimeNow
--  let deliver t m = do
--      if t == 0
--        then out (now,m) 
--        else push pChan (now+t) m
             
--  case m of
--    Std m -> deliver t m
--    ANote c k v d -> do
--        deliver t     (NoteOn c k v)
--        deliver (t+d) (NoteOff c k v)

--outputMidi :: OutputDeviceID -> IO ()
--outputMidi devId = do
--  inits <- readIORef outDevMap
--  (pChan, out, _stop) <- return (fromJust $ lookup devId inits) -- getOutDev devId
--  let loop = do
--        r <- peek pChan
--        case r of
--          Nothing     -> return ()
--          Just (t,m)  -> do
--            now <- getTimeNow
--            if t <= now 
--              then out (now, m) >> pop pChan >> loop
--              else return ()
--  loop
--  return ()

