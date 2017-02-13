-- | Define about the types used in around the parser
module Brainhack.Parser.Items
  ( BrainfuckOperator (..)
  , BrainfuckToken (..)
  , BrainfuckProgram
  , BrainfuckParserException (..)
  ) where

import Control.Exception.Safe (Exception, Typeable)
import Data.Text (Text)

data BrainfuckOperator = ForwardOp
                       | BackwardOp
                       | IncrOp
                       | DecrOp
                       | OutputOp
                       | InputOp
                       | LoopBeginOp
                       | LoopEndOp
  deriving (Eq)

instance Show BrainfuckOperator where
  show ForwardOp   = ">"
  show BackwardOp  = "<"
  show IncrOp      = "+"
  show DecrOp      = "-"
  show OutputOp    = "."
  show InputOp     = ","
  show LoopBeginOp = "["
  show LoopEndOp   = "]"


-- |
-- The text type for ><+-.,[] .
-- This type is homotype of BrainfuckOperator and Text.
-- Almost instance of this is Text newtype
class BrainfuckToken a where
  forwardToken   :: a  -- ^ The token
  backwordToken  :: a  -- ^ The token
  incrToken      :: a  -- ^ The token
  decrToken      :: a  -- ^ The token
  outputToken    :: a  -- ^ The token
  inputToken     :: a  -- ^ The token
  loopBeginToken :: a  -- ^ The token
  loopEndToken   :: a  -- ^ The token
  toText         :: a -> Text  -- ^ Almost this is the undata record of your newtype
  fromText       :: Text -> a  -- ^ Almost this is the value constructor of your newtype
  toOperator     :: a -> Maybe BrainfuckOperator  -- ^ Return the correspond brainf*ck operator if `a` is the token

-- | The whole of the abstract expression
type BrainfuckProgram = [BrainfuckOperator]

-- | The exception of the parser with the cause
data BrainfuckParserException = BrainfuckParserException String
  deriving (Show, Typeable)

instance Exception BrainfuckParserException
