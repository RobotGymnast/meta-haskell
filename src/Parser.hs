module Parser ( parser
              , Chunk (..)
              ) where

import Control.Applicative hiding (Alternative (..))
import Control.Monad
import Data.Maybe
import Data.Monoid
import Text.Parsec
import Text.Parsec.String

type Data = String
type Code = String

data Chunk = Data Data
           | Code Code

-- `anyChar` as a singleton list
anyChars :: Parser [Char]
anyChars = (:[]) <$> anyChar

infixl 6 `muntil`

-- | `(<$)` with arguments interchanges.
($>) :: Functor f => f a -> b -> f b
f $> b = b <$ f

-- | act `muntil` stop performs act repeatedly muntil we reach stop or end, concatenating the values.
muntil :: Monoid a => Parser a -> Parser b -> Parser a
muntil act s
    = (eof $> mempty)
  <|> (try s $> mempty)
  <|> mappend <$> act <*> muntil act s

parser :: Parser [Chunk]
parser = catMaybes <$> sequence chunks `muntil` eof

chunks :: [Parser (Maybe Chunk)]
chunks = [ make Data $ anyChars `muntil` string "{-@"
         , make Code $ anyChars `muntil` string "@-}"
         ]

make :: (String -> Chunk) -> Parser String -> Parser (Maybe Chunk)
make f = fmap $ \s -> guard (not $ null s) >> Just (f s)
