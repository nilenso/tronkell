module Main where

import qualified TestServer as S
import qualified TestEngine as E

main = S.main >> E.main
