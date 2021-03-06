{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
import qualified Sound.ALSA.Exception            as AlsaExc
import qualified Sound.ALSA.Sequencer            as SndSeq
import qualified Sound.ALSA.Sequencer.Address    as Addr
import qualified Sound.ALSA.Sequencer.Client     as Client
import qualified Sound.ALSA.Sequencer.Connect    as Connect
import qualified Sound.ALSA.Sequencer.Port       as Port
import qualified Sound.ALSA.Sequencer.Event      as Event
import qualified Sound.MIDI.ALSA                 as MidiAlsa
import qualified Sound.MIDI.General              as Midi
import qualified Sound.MIDI.Message.Channel      as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Mode as Mode

import           Control.Concurrent           (threadDelay)
import           Control.Exception            (finally)
import           Control.Monad                (forM_, when)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Cont     (ContT(ContT), runContT)
import           Data.Int                     (Int32)
import           Data.List                    (groupBy)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust)
import           Data.Word                    (Word8)
import           System.Console.CmdArgs
import           System.Environment           (getArgs)
import           System.Exit                  (exitSuccess)
import           System.IO                    as IO

import           Machines
import           Turing
import qualified Scale

main :: IO ()
main = handleExceptionCont $ do
  h <- ContT $ SndSeq.withDefault SndSeq.Block
  liftIO $ Client.setName h "Turing Music"
  p <- ContT $ Port.withSimple h "out" (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric
  liftIO $ midiMain h p

midiMain :: SndSeq.T SndSeq.OutputMode -> Port.T -> IO ()
midiMain h p = do
  Arguments{..} <- cmdArgs arguments
  when (machine < 0 || machine > 41) $ do
    error "machine number must be between 0 and 41"
  when (width < 1) $ do
    error "terminal width must be at least 1"
  when (list /= Nothing) $ do
    case fromJust list of
      Machines    -> putStr $ unlines hints
      Instruments -> putStrLn "list instruments"
      Scales      -> putStrLn "list scales"
      Bases       -> putStrLn "list bases"
    exitSuccess
  let
    basePitch = read base :: Scale.Pitch
    scaleKeys = Scale.scaleValues basePitch scale
  putStrLn $ "base note: " ++ show basePitch
  let tapes = run (machines !! machine) initialState blankTape
  c <- Client.getId h
  putStrLn ("Created sequencer with id: " ++ show c)
  conn <- parseDestArgs h (Addr.Cons c p) port
  _ <- Event.outputDirect h $ Event.forConnection conn $ Event.CtrlEv Event.PgmChange
         $ MidiAlsa.programChangeEvent channel0 (Midi.instrumentToProgram instrument)
  finally (playTapes h conn width scaleKeys tapes) (allSoundOff h conn)

allSoundOff :: SndSeq.T SndSeq.OutputMode -> Connect.T -> IO ()
allSoundOff h conn = do
  putStrLn "Turning all sound off"
  _ <- Event.outputDirect h $ Event.forConnection conn $ Event.CtrlEv Event.Controller
         $ MidiAlsa.modeEvent channel0 Mode.AllSoundOff
  return ()

channel0 = MidiAlsa.toChannel $ Event.Channel 0

deriving instance Data Midi.Instrument
deriving instance Typeable Midi.Instrument

data Arguments = Arguments
  { port       :: String
  , list       :: Maybe Listing
  , machine    :: Int
  , instrument :: Midi.Instrument
  , scale      :: Scale.Pattern
  , base       :: String
  , width      :: Int
  } deriving (Data, Eq, Show, Typeable)

data Listing = Machines | Instruments | Scales | Bases
  deriving (Data, Eq, Show, Typeable)

defaultPort       :: String
defaultPort       = "128:0"
defaultList       :: Maybe Listing
defaultList       = Nothing
defaultMachine    :: Int
defaultMachine    = 41
defaultInstrument :: Midi.Instrument
defaultInstrument = Midi.AcousticGrandPiano
defaultScale      :: Scale.Pattern
defaultScale      = Scale.MajorPentatonic
defaultBase       :: String
defaultBase       = "C5"
defaultWidth      :: Int
defaultWidth      = 78

arguments :: Arguments
arguments = Arguments
  { port
    =  defaultPort
    &= typ "CLIENT:PORT,..."
    &= help ("MIDI port(s) to play to (default: " ++ defaultPort ++ ")")
  , list
    = defaultList
    &= help ("List possible values for a given flag")
  , machine
    =  defaultMachine
    &= help ("Machine to use as generator (default: " ++ show defaultMachine ++ ")")
  , instrument
    = defaultInstrument
    &= help ("Musical instrument (default: " ++ show defaultInstrument ++ ")")
  , scale
    =  defaultScale
    &= help ("Musical scale (default: " ++ show defaultScale ++ ")")
  , base
    =  defaultBase
    &= typ "PITCH"
    &= help ("Base note of the scale (default: " ++ defaultBase ++ ")")
  , width
    =  defaultWidth
    &= help ("Terminal width for visualization (default: " ++ show defaultWidth ++ ")")
  } &= program "turing-tunes-midi" &= summary "Generate MIDI tunes from simple Turing machines"

playTapes :: SndSeq.T SndSeq.OutputMode -> Connect.T -> Int -> [Int] -> [Tape] -> IO ()
playTapes h conn width scale states = do
  putStrLn $ replicate (width `div` 2) '-' ++ "_" ++ replicate (width - width `div` 2 - 1) '-'
  mapM_ (play scale) groups
 where
  groups = groupBy eq states
  note pitch vel =
    Event.forConnection conn $ Event.NoteEv Event.NoteOn
      $ Event.simpleNote (Event.Channel 0) (Event.Pitch pitch) $ Event.Velocity vel
  
  play scale stateGroup = do
    let
      Tape{..} = head stateGroup
      key = fromIntegral $ scale !! (pos `mod` length scale)
      volume = fromIntegral $ if head right then 127 else 64
      duration = 100 :: Int
      diff = (' ', '0', '1', '0', '1')
      same = (' ', '0', '1', '0', '1')
    _ <- Event.outputDirect h $ note key volume
    forM_ (zip stateGroup (diff : repeat same)) $ \(tape, style) -> do
      putStr $ showTapeCentered style width tape
      IO.hFlush IO.stdout
      putStr "\r"
      threadDelay (duration * 10^(3 :: Int))
    _ <- Event.outputDirect h $ note key 0
    return ()

parseDestArgs ::
   (SndSeq.AllowOutput mode) =>
   SndSeq.T mode -> Addr.T -> String -> IO Connect.T
parseDestArgs h me destStr = do
   let p = Addr.port me
   Connect.createTo h p =<< Addr.parse h destStr

handleExceptionCont :: ContT () IO () -> IO ()
handleExceptionCont = handleException . runContUnit

handleException :: IO () -> IO ()
handleException act =
   act `AlsaExc.catch` \e -> putStrLn $ "alsa_exception: " ++ AlsaExc.show e

runContUnit :: (Monad m) => ContT a m a -> m a
runContUnit cont = runContT cont return
