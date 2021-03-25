module Int where

import Data.Monoid

type Elem = Int
type Measure = Sum Int

measure :: Elem -> Measure
measure _ = Sum 1
