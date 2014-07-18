import Turing
import Machines

import Sound.ALUT
import Data.Maybe
import Data.List
import Data.Char

main :: IO ()
main = withProgNameAndArgs runALUT main'

main' :: String -> [String] -> IO ()
main' _progName [i]  | all isDigit i && read i<length machines   = playMachine $ machines !! read i
main' progName _ = mapM_ putStrLn $
    [ "Usage:"
    , " " ++ progName ++ " NUM"
    , "  where 0 <= NUM < " ++ show (length machines)
    , ""
    , "Hints:"
    ] ++ map (" "++) hints

playMachine :: Machine -> IO ()
playMachine m = do
    bData <- createBufferData (Sine 440 0 0.1)
    buffs <- mapM (createBuff bData) pentaton

    [source] <- genObjectNames 1
    loopingMode source $= Looping

    let play1 l@(h: _) = do
            stop [source]
            buffer source $= Just (buffs !! ((pos h + 5) `mod` length buffs))
            sourceGain source $= 0.5 + 0.5 * read [symbolAtHead h]
            play [source]
            sequence_ $ replicate (length l) $ putStr ('\n': showTape 40 h) >> sleep 0.1

    putStrLn ""
    mapM_ play1 $ groupBy eq $ run m initialState blankTape

eq :: Tape -> Tape -> Bool
eq t t' = goToMiddle t == goToMiddle t'

createBuff :: BufferData a -> Frequency -> IO Buffer
createBuff (BufferData m fmt f) x = do
    [b] <- genObjectNames 1
    bufferData b $= BufferData m fmt (x*f)
    return b

pentaton :: [Frequency]
pentaton = l ++ map (2*) l where l = [3/4, 8/9, 1/1, 9/8, 4/3]




