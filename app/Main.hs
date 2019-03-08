 module Main where

import Control.Monad
import Control.Applicative
import NanoParsec

main :: IO ()
main = tryMiniLanguage
