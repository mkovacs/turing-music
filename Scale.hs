{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Scale
  ( Letter(..)
  , Note(..)
  , Pitch(..)
  , pitchValue
  , Pattern(..)
  , toIntervals
  ) where

import Control.Applicative ((<$>), (<$), (<*>))
import Data.Data
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec.Number
import Text.Parsec.Prim
import Text.Parsec.String

x ==> y = y <$ x

reader p =
  const $ either (const []) (\x -> [(x, "")]) . parse p ""

data Letter = C | D | E | F | G | A | B
  deriving (Eq, Show)

parseLetter :: Parser Letter
parseLetter = choice
  [ char 'C' ==> C
  , char 'D' ==> D
  , char 'E' ==> E
  , char 'F' ==> F
  , char 'G' ==> G
  , char 'A' ==> A
  , char 'B' ==> B
  ]

instance Read Letter where
  readsPrec = reader parseLetter

letterValue :: Letter -> Int
letterValue = \case
  C ->  0
  D ->  2
  E ->  4
  F ->  5
  G ->  7
  A ->  9
  B -> 11

data Accidental = Flat | Natural | Sharp
  deriving (Eq, Show)

parseAccidental :: Parser Accidental
parseAccidental = choice
  [ char 'b'   ==> Flat
  , char '#'   ==> Sharp
  , string ""  ==> Natural
  ]

instance Read Accidental where
  readsPrec = reader parseAccidental

accidentalValue :: Accidental -> Int
accidentalValue = \case
  Flat    -> (-1)
  Natural -> 0
  Sharp   -> 1

data Note = Note Letter Accidental
  deriving (Eq, Show)

parseNote :: Parser Note
parseNote =
  Note <$> parseLetter <*> parseAccidental

instance Read Note where
  readsPrec = reader parseNote

isValid :: Note -> Bool
isValid = \case
  Note C Flat  -> False
  Note E Sharp -> False
  Note F Flat  -> False
  Note B Sharp -> False
  _            -> True

noteValue :: Note -> Int
noteValue (Note letter accidental) =
  letterValue letter + accidentalValue accidental

data Pitch = Pitch { pitchNote :: Note, pitchOctave :: Int }
  deriving (Show)

parsePitch :: Parser Pitch
parsePitch =
  Pitch <$> parseNote <*> decimal

instance Read Pitch where
  readsPrec = reader parsePitch

pitchValue :: Pitch -> Int
pitchValue Pitch{..} =
  12 * pitchOctave + noteValue pitchNote

data Pattern
  = Major
  | Minor
  | Ionian
  | Dorian
  | Phrygian
  | Lydian
  | Mixolydian
  | Aeolian
  | Locrian
  | MinorPentatonic
  | MajorPentatonic
  | Blues
  | SpanishPhrygian
--  | Intervals [Int]
  deriving (Data, Eq, Show, Typeable)

toIntervals :: Pattern -> [Int]
toIntervals = \case
  Major           -> [2, 2, 1, 2, 2, 2, 1]
  Minor           -> [2, 1, 2, 2, 1, 2, 2]
  Ionian          -> [2, 2, 1, 2, 2, 2, 1]
  Dorian          -> [2, 1, 2, 2, 2, 1, 2]
  Phrygian        -> [1, 2, 2, 2, 1, 2, 2]
  Lydian          -> [2, 2, 2, 1, 2, 2, 1]
  Mixolydian      -> [2, 2, 1, 2, 2, 1, 2]
  Aeolian         -> [2, 1, 2, 2, 1, 2, 2]
  Locrian         -> [1, 2, 2, 1, 2, 2, 2]
  MinorPentatonic -> [3, 2, 2, 3, 2]
  MajorPentatonic -> [2, 2, 3, 2, 3]
  Blues           -> [3, 2, 1, 1, 3, 2]
  SpanishPhrygian -> [1, 3, 1, 2, 1, 2, 2]
--  Intervals ints  -> ints
