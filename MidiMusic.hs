{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
import qualified Sound.ALSA.Exception         as AlsaExc
import qualified Sound.ALSA.Sequencer         as SndSeq
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client  as Client
import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Port    as Port
import qualified Sound.ALSA.Sequencer.Event   as Event

import           Control.Concurrent           (threadDelay)
import           Control.Monad                (forM_)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Cont     (ContT(ContT), runContT)
import           Data.Int                     (Int32)
import           Data.List                    (groupBy)
import qualified Data.Map                     as Map
import           Data.Word                    (Word8)
import           System.Console.CmdArgs
import           System.Environment           (getArgs)

import Machines
import Turing

main :: IO ()
main = handleExceptionCont $ do
  h <- ContT $ SndSeq.withDefault SndSeq.Block
  liftIO $ Client.setName h "Turing Music"
  p <- ContT $ Port.withSimple h "out" (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric
  liftIO $ midiMain h p

midiMain :: SndSeq.T SndSeq.OutputMode -> Port.T -> IO ()
midiMain h p = do
  Arguments{..} <- cmdArgs arguments
  let m = machines !! 14
  let tapes = run m initialState blankTape
  let destStr = "128:0"
  c <- Client.getId h
  putStrLn ("Created sequencer with id: " ++ show c)
  conn <- parseDestArgs h (Addr.Cons c p) destStr
  _ <- Event.outputDirect h $ Event.forConnection conn $ Event.CtrlEv Event.PgmChange
         $ Event.Ctrl (Event.Channel 0) (Event.Parameter 0) (Event.Value $ instrumentCode instrument)
  playTapes h conn tapes

data Arguments = Arguments
    { port       :: String
    , machine    :: Int
    , instrument :: Instrument
    , scale      :: String
    , base       :: String
    } deriving (Data, Eq, Show, Typeable)

defaultPort       :: String
defaultPort       = "128:0"
defaultMachine    :: Int
defaultMachine    = 41
defaultScale      :: String
defaultScale      = "pentatonic"
defaultBase       :: String
defaultBase       = "C"

arguments :: Arguments
arguments = Arguments
  { port
    =  defaultPort
    &= typ "CLIENT:PORT,..."
    &= help ("MIDI port(s) to play to (default: " ++ defaultPort ++ ")")
  , machine
    =  defaultMachine
    &= help ("Machine to use as generator (default: " ++ show defaultMachine ++ ")")
  , instrument
    = def
    &= help ("Musical instrument (default: " ++ show (def :: Instrument) ++ ")")
  , scale
    =  defaultScale
    &= help ("Musical scale (default: " ++ defaultScale ++ ")")
  , base
    =  defaultBase
    &= help ("Base note of the scale (default: " ++ defaultBase ++ ")")
  } &= program "turing-tunes-midi" &= summary "Generate MIDI tunes from simple Turing machines"

data Instrument
  = AcousticGrandPiano
  | Marimba
  | Xylophone
  | AcousticNylonGuitar
  | ElectricJazzGuitar
  | ElectricBassPick
  | TenorSax
  | Flute
  | Shakuhachi
  deriving (Data, Eq, Read, Show, Typeable)

instance Default Instrument where
  def = AcousticGrandPiano

instrumentCode :: Instrument -> Int32
instrumentCode = \case
  AcousticGrandPiano  ->  0
  Marimba             -> 12
  Xylophone           -> 13
  AcousticNylonGuitar -> 24
  ElectricJazzGuitar  -> 26
  ElectricBassPick    -> 34
  TenorSax            -> 66
  Flute               -> 73
  Shakuhachi          -> 77

playTapes :: SndSeq.T SndSeq.OutputMode -> Connect.T -> [Tape] -> IO ()
playTapes h conn states = do
  mapM_ play groups
 where
  groups = groupBy eq states
  note pitch vel =
    Event.forConnection conn $ Event.NoteEv Event.NoteOn
      $ Event.simpleNote (Event.Channel 0) (Event.Pitch pitch) $ Event.Velocity vel
  play group = do
    let tape = head group
    putStrLn $ showTape 78 tape
    let Note{..} = groupToNote (length group) tape
    _ <- Event.outputDirect h $ note noteKey noteVol
    threadDelay (noteLen * 10^3 :: Int)
    _ <- Event.outputDirect h $ note noteKey 0
    return ()

data Note =
  Note
  { noteKey :: Word8 -- MIDI key
  , noteVol :: Word8 -- MIDI velocity
  , noteLen :: Int -- milliseconds
  }

groupToNote :: Int -> Tape -> Note
groupToNote len Tape{..} =
  Note
  { noteKey = fromIntegral $ 88 + pos
  , noteVol = fromIntegral $ 100 * if head right == '0' then 1 else 2
  , noteLen = 100 * len
  }

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
