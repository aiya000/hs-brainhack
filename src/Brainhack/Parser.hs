{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parse a text to an abstract program
module Brainhack.Parser
  ( parse
  ) where

import Brainhack.Parser.Items
import Control.Applicative ((<|>))
import Control.Exception.Safe (MonadCatch, throw)
import Control.Monad (void, mapM)
import Data.Attoparsec.Text (Parser)
import Data.List (foldl1')
import Data.Text (Text)
import qualified Control.Applicative as P (many)
import qualified Data.Attoparsec.Combinator as P (lookAhead)
import qualified Data.Attoparsec.Text as P
import qualified Data.Map.Lazy as M
import qualified Data.Text as T


-- |
-- Parse source code.
-- If parsing is succeed, return result.
-- Otherwise, throw the exception.
parse :: forall m a. (MonadCatch m, BrainfuckOperation a) => Text -> m (BrainfuckProgram a)
parse txt =
  case P.parseOnly codeParser txt of
    Left  e        -> throw $ BrainfuckParserException e
    Right Nothing  -> throw $ BrainfuckParserException tokenParseErrorMsg
    Right (Just a) -> return a
  where
    -- Throw this message if any looking up a token is failure
    tokenParseErrorMsg = "Fatal error:\n" ++
                         "An other than token is detected in the code parser\n" ++
                         "`tokenParser` should return the tokens only"

    -- Determine monomorphic type of BrainfuckProgram and its tokens
    tokenTexts :: [Text]
    tokenTexts = map toToken ([ forward
                              , backword
                              , incr
                              , decr
                              , output
                              , input
                              , loopBegin
                              , loopEnd
                              ] :: [a])

    -- Parse a code to brainf*ck operations
    codeParser :: Parser (Maybe [a])
    codeParser = mapM fromToken <$> tokensParser

    -- Parse a code to tokens
    tokensParser :: Parser [Text]
    tokensParser = concat <$> P.many tokenBlockParser

    -- A block means "あああ にっこにっこにーにこにーって覚えてラブニコ！ いいい" .
    --                       |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^|
    -- And parse its text to [にっこにっこにー, にこにーって覚えてラブニコ！]
    tokenBlockParser :: Parser [Text]
    tokenBlockParser = do
      skipNonToken
      P.many tokenParser

    -- Match any token
    tokenParser :: Parser Text
    tokenParser = foldl1' (<|>) . map (P.try . P.string) $ tokenTexts

    -- Skip other than the token
    -- (Seek to just before the token)
    skipNonToken :: Parser ()
    skipNonToken = do
      let heads = map T.head tokenTexts
      P.skipWhile (`notElem` heads)
      (void $ P.lookAhead tokenParser) <|> (P.anyChar >> skipNonToken)