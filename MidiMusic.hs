{-# LANGUAGE RecordWildCards #-}
import Common (handleExceptionCont, parseDestArgs, )

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq

import Data.List (groupBy)
import Data.Word (Word8)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Control.Monad.Trans.Cont (ContT(ContT), )
import Control.Monad.IO.Class (liftIO, )

import System.Environment (getArgs, )

import Machines
import Turing

main :: IO ()
main = handleExceptionCont $ do
  h <- ContT $ SndSeq.withDefault SndSeq.Block
  liftIO $ Client.setName h "Turing Music"
  p <- ContT $ Port.withSimple h "out" (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric
  let m = machines !! 41
  let tapes = run m initialState blankTape
  liftIO $ playTapes h p tapes

playTapes :: SndSeq.T SndSeq.OutputMode -> Port.T -> [Tape] -> IO ()
playTapes h p states = do
  c <- Client.getId h
  putStrLn ("Created sequencer with id: " ++ show c)
  conn <- parseDestArgs h (Addr.Cons c p) =<< getArgs
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
