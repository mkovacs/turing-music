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
import           Data.List                    (groupBy)
import           Data.Word                    (Word8)
import           System.Environment           (getArgs)

import Machines
import Turing

main :: IO ()
main = handleExceptionCont $ do
  h <- ContT $ SndSeq.withDefault SndSeq.Block
  liftIO $ Client.setName h "Turing Music"
  p <- ContT $ Port.withSimple h "out" (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric
  let m = machines !! 41
  let tapes = run m initialState blankTape
  liftIO $ playTapes h p "128:0" tapes

playTapes :: SndSeq.T SndSeq.OutputMode -> Port.T -> String -> [Tape] -> IO ()
playTapes h p destStr states = do
  c <- Client.getId h
  putStrLn ("Created sequencer with id: " ++ show c)
  conn <- parseDestArgs h (Addr.Cons c p) destStr
  let groups = groupBy eq states
  let note pitch vel =
        Event.forConnection conn $ Event.NoteEv Event.NoteOn
        $ Event.simpleNote (Event.Channel 0) (Event.Pitch pitch) $ Event.Velocity vel
      play group = do
        let tape = head group
        putStrLn $ showTape 40 tape
        let Note{..} = groupToNote (length group) tape
        Event.outputDirect h $ note noteKey noteVol
        threadDelay (noteLen * 10^3 :: Int)
        Event.outputDirect h $ note noteKey 0
  Event.outputDirect h $ Event.forConnection conn $ Event.CtrlEv Event.PgmChange
    $ Event.Ctrl (Event.Channel 0) (Event.Parameter 0) (Event.Value tenorSax)
  mapM_ play groups
 where
  acousticGrandPiano  = 0
  marimba             = 12
  xylophone           = 13
  acousticNylonGuitar = 24
  electricJazzGuitar  = 26
  electricBassPick    = 34
  tenorSax            = 66
  flute               = 73

data Note =
  Note
  { noteKey :: Word8 -- MIDI key
  , noteVol :: Word8 -- MIDI velocity
  , noteLen :: Int -- milliseconds
  }

groupToNote :: Int -> Tape -> Note
groupToNote len Tape{..} =
  Note
  { noteKey = fromIntegral $ 64 + pos
  , noteVol = fromIntegral $ 64 * if head right == '0' then 1 else 2
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
   act
   `AlsaExc.catch` \e ->
      putStrLn $ "alsa_exception: " ++ AlsaExc.show e

runContUnit :: (Monad m) => ContT a m a -> m a
runContUnit cont = runContT cont return
