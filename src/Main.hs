module Main ( main
            ) where

import Prelewd
import IO

import Data.Either
import Storage.List
import Text.Parsec hiding ((<?>))
import Text.Parsec.String
import Text.Parsec.Error

import Parser

main :: SystemIO ()
main = runIO $ do
        args <- getArgs
        (concatMap . process <$> head args <*> tail args) <?> usage

usage :: IO ()
usage = putStrLn "usage: meta output-prefix files.."

process :: Text -> Text -> IO ()
process prefix inFile = let file = outFile prefix inFile
                        in do
                            system $ "rm -f " <> file
                            io (parseFromFile parser inFile)
                            >>= either propogateError (concatMap $ outputChunk file)

propogateError :: ParseError -> IO ()
propogateError = putStrLn . intercalate "\n" . ("Errors occurred:":) . map messageString . errorMessages

outputChunk :: Text -> Chunk -> IO ()
outputChunk file c = output c >>= appendFile file

output :: Chunk -> IO Text
output (Data s) = return s
output (Code s) = do
        system "rm -f /tmp/meta-haskell* /tmp/out.txt"
        writeCode "/tmp/meta-haskell.hs" s
        rawSystem "ghc" ["/tmp/meta-haskell.hs"]
        system "/tmp/meta-haskell > /tmp/out.txt"
        readFile "/tmp/out.txt"

writeCode :: Text -> Text -> IO ()
writeCode file s = writeFile file $ "module Main ( main ) where\nout = " <> s <> "\n\nmain = putStr out"

outFile :: Text -> Text -> Text
outFile x y = x <> "/" <> y
