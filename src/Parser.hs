module Parser ( parser
              , Chunk (..)
              ) where

import Prelewd hiding (join, try)

import Data.Char
import Data.Maybe (catMaybes)
import Text.Parsec hiding ((<|>))
import Text.Parsec.String

type Data = Text
type Code = Text

data Chunk = Data Data
           | Code Code

-- `anyChar` as a singleton list
anyChars :: Parser [Char]
anyChars = anyChar <&> (:[])

infixl 6 `until`

-- | act `until` stop performs act repeatedly until we reach stop or end, concatenating the values.
until :: Monoid a => Parser a -> Parser b -> Parser a
until act s = (eof $> mempty)
            <|> (try s $> mempty)
            <|> act <&> (<>) <*> until act s

parser :: Parser [Chunk]
parser = catMaybes <$> sequence chunks `until` eof

chunks :: [Parser (Maybe Chunk)]
chunks = [ make Data $ anyChars `until` string "{-@"
         , make Code $ anyChars `until` string "@-}"
         ]

make :: (Text -> Chunk) -> Parser Text -> Parser (Maybe Chunk)
make f = map $ iff <$> not . null <*> Just . f <*> pure Nothing
