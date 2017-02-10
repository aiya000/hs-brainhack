{-# LANGUAGE OverloadedStrings #-}

-- | Define about the types used in around the parser
module Brainhack.Parser.Items where

import Control.Exception.Safe (Exception, Typeable)
import Data.Map.Lazy (Map)
import Data.Text (Text)
import qualified Data.Map.Lazy as M

-- | The expression of the brainf*ck's ><+-.,[]
class BrainfuckOperation a where
  --TODO: Rename forward, backword, incr, decr, output, input, loopBegin, loopEnd to special name
  forward   :: a
  backword  :: a
  incr      :: a
  decr      :: a
  output    :: a
  input     :: a
  loopBegin :: a
  loopEnd   :: a
  toToken   :: a -> Text  -- ^ Convert BrainfuckOperation to token text
  fromToken :: Text -> Maybe a  -- ^ Convert text to BrainfuckOperation, but return Nothing if the text isn't token

-- | The whole of the brainf*ck source code abstract
type BrainfuckProgram a = [a]

-- | An exception of brainf*ck instance's parser with the cause
data BrainfuckParserException = BrainfuckParserException String
  deriving (Show, Typeable)

instance Exception BrainfuckParserException
