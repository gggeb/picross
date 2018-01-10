module Loader
    ( parseGrid
    ) where

import Logic

parseGrid str = map (map check) $ lines str
  where
    check 'X' = Full
    check _   = Empty
