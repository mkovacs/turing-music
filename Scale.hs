{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
module Scale
  ( Pattern(..)
  , toIntervals
  ) where

import Data.Data

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
