module Main ( main
            ) where

import Control.Arrow
import Data.Foldable
import Data.List
import Data.Monoid
import Text.Parsec hiding ((<?>))
import Text.Parsec.String
import Text.Parsec.Error

import System.Environment
import System.Process (system)

import Parser

main :: IO ()
main = do
        args <- getArgs
        case args of
          [] -> usage
          (outputPrefix:files) -> traverse_ (process outputPrefix) files

usage :: IO ()
usage = putStrLn "usage: meta output-prefix files.."

process :: String -> String -> IO ()
process prefix inFile = let file = prefix <> "/" <> inFile
                        in system ("rm -f " <> file)
                        >> parseFromFile parser inFile
                        >>= either propogateError (traverse_ $ outputChunk file)

propogateError :: ParseError -> IO ()
propogateError =  errorMessages
              >>> map messageString
              >>> ("Errors occurred:" :)
              >>> intercalate "\n"
              >>> putStrLn

outputChunk :: String -> Chunk -> IO ()
outputChunk file c = output c >>= appendFile file

output :: Chunk -> IO String
output (Data s) = return s
output (Code s) = do
        _ <- system "rm -f /tmp/meta-haskell* /tmp/out.txt"
        writeFile "/tmp/meta-haskell.hs" s
        _ <- system "runhaskell < /tmp/meta-haskell.hs > /tmp/out.txt"
        readFile "/tmp/out.txt"
