module Machines where

import Turing

readMachine :: String -> Machine
readMachine = zip x . map f . words  where

    x = concat [[(s, blankSymbol), (s, '1')] | s<-['A'..]]

    f [st, s, d] = (s, read [d], st)

machines :: [Machine]
machines = map (readMachine . fst) table

table :: [(String, String)]
table =
    [ (,) "C1L E1L  H1L D1L  D1R D0L  A1L E1R  B0L C0R" "slow"
    , (,) "C1L E0R  H1L C0R  D1R A0L  A1R D1R  A1L B0R" "fast, down slopes"
    , (,) "C1L A0R  H1L E1L  D1R B0L  A1R C1R  C0L D1L" "slow, down slopes"
    , (,) "C1L D0R  H1L E0L  D1R C1L  E1L A1R  B1L D0L" "tale #1"
    , (,) "C1L A1L  H1L D0L  D1R E0L  A1L C0R  C1R B0L" "fast"
    , (,) "C1L B0R  H1L D0R  D1L A0R  E1R C0L  C1R E1R" "fast with slow parts"
    , (,) "C1L B0R  H1L E1R  D1L A1L  A1R D0L  A0R C1R" "fast, hills"
    , (,) "C1L B0R  H1L C0R  D1L C0L  E0R C1L  A0R E1R" "slow, romboids"
    , (,) "C1L D1R  H1L C0L  A1R C1L  E1R A0R  B1L E0L" "repeat #5"
    , (,) "C1L A0L  H1L C0L  D0R A1L  B1L E1R  D1R E0R" "mountains in both direction #4"
    , (,) "C1L A0L  H1L A0R  D0R A1L  E0R D1R  A1L B0R" "slow, romboids #2"
    , (,) "C1L E0L  H1L E1L  D0R A1L  A0L C1R  C1R B0L" "slow increase"
    , (,) "C1L B0R  H1L A1R  D0L E1R  E0R C1L  C1R A0R" "slow, hills"
    , (,) "B1L H1L  C1R E0R  D1L B0R  D0L A1L  C0R A0L" "fast"
    , (,) "B1L H1L  C1L B1R  D1R E1L  B1R D0R  A1L C0L" "slow with fast parts, left mountains"
    , (,) "B1L H1L  C0R D1L  D1R C1R  E1L E0L  A0L B0R" "slow with fast parts"
    , (,) "B1L H1L  C0R E1L  D0R C1R  A1L B1R  B0L A0L" "very slow"
    , (,) "B1L H1L  C0L D0R  D1L E0R  E1L A0L  C1R D0R" "fast"
    , (,) "B1L H1L  C0L B0L  C1R D0R  A1L E0R  A0R E0R" "fast, mountains in clouds ||  ||, #1"
    , (,) "B1L H1L  C0L D1L  D0R C1L  E1R A0L  A1L E0R" "fast & slow, left mountains"
    , (,) "C1L E1L  A1L H1L  D1R E0R  B1R E1R  C1R A0L" "fast, |/"
    , (,) "C1L E0L  A1R H1L  D1R A0L  D0R B1R  C0L B0R" "fast"
    , (,) "C1L C0R  D0L H1L  D1R E0L  C1L E0R  A1R B1L" "tale"
    , (,) "C1L A1L  E1R H1L  D1R D0R  B0R E0L  A0L C1R" "hills with clouds"
    , (,) "C1L A0R  A1L H1L  D1R E1L  A1R D0R  E0L B0R" "tale"
    , (,) "C1L E1R  D1R H1L  D1L C0L  A1R D1L  B1R A0R" "mountains - hills"
    , (,) "C1L E0L  D1R H1L  B1L E1L  A1R E1R  A1L D0R" "fast, left mountains"
    , (,) "C1L D0R  A0L H1L  A1R D0L  E1R B1L  C1L C0R" "tale"
    , (,) "C1L E0L  C1R H1L  D0R A1L  A1R E0R  B1R E0L" "hills with clouds"
    , (,) "C1L B0R  E0R H1L  D0L C1L  E1L C0L  A1R C0R" "slow"
    , (,) "C1L E0R  C0L H1L  D0L B0L  D1R A0R  A1R D1L" "fast, decreasing"
    , (,) "C1L D1R  E1R H1L  D0L C0L  B1R A0R  A1R E1L" "slow tale, mountains"
    , (,) "C1L D1R  E1R H1L  D0L C0L  B1R A0R  A1R A1L" "jazz, hills"
    , (,) "C1L D1R  E1R H1L  D0L C0L  B1R A0R  A1R A0R" "mountains - hills #1"
    , (,) "C1L E1R  D1R H1L  D0L C0L  B1R A1L  D1L A0R" "mountains - hills"
    , (,) "C1L B0R  C1R H1L  D0L D0R  A1R E0L  D1L E1L" "fast, | |/"
    , (,) "C1L C0L  D1L H1L  B0L D0R  E0R A1L  A1R E1R" "slow with fast parts"
    , (,) "B1L D1L  C1R H1L  E1R D1R  E1L C0R  A1L D0L" "left mountains"
    , (,) "B1L A0L  C1R H1L  C0R D0R  E1L B0L  E0L A1L" "slow, |"
    , (,) "B1L A0R  C1L H1L  D0L E1R  E1L A0L  C1R A0R" "inverse mountains"
    , (,) "B1L E0R  C1L H1L  D0L C0L  D1R A0R  B0R E0R" "fast, mountains in clouds ||  ||"
    , (,) "B1L A0R  C0L H1L  C1R D1L  E1L A1R  B0L D0R" "slow jazz #2"
    ]

hints :: [String]
hints = map h $ zip [0::Int ..] table where

    h (n, (_, s)) = show n ++ replicate (5 - length (show n)) ' ' ++ s

