# brainhack
The library for making brainf*ck dialect :+1:


# Example
[nico-lang](https://github.com/aiya000/nico-lang) is implemented by brainhack !


# How to implement brainf*ck dialect with this ?
- 1: Define the data type of brainf*ck operators

```haskell
-- | The nico-lang's expression of the brainf*ck's ><+-.,[]
data NicoOperation = NicoForward    -- ^ >
                   | NicoBackword   -- ^ <
                   | NicoIncr       -- ^ +
                   | NicoDecr       -- ^ -
                   | NicoOutput     -- ^ .
                   | NicoInput      -- ^ ,
                   | NicoLoopBegin  -- ^ [
                   | NicoLoopEnd    -- ^ ]
  deriving (Eq)
```

- 2: Define `BrainfuckOperation` instance

```haskell
instance BrainfuckOperation NicoOperation where
  forward   = NicoForward
  backword  = NicoBackword
  incr      = NicoIncr
  decr      = NicoDecr
  output    = NicoOutput
  input     = NicoInput
  loopBegin = NicoLoopBegin
  loopEnd   = NicoLoopEnd
  toToken   = ...
  fromToken = ...
```

- 3: Run `parse` and `eval` with type specifying

```haskell
main :: IO ()
main = do
  nicoCode <- pack <$> readFile "hello.nico"
  case (parse nicoCode :: Either SomeException NicoLangProgram) of
    Left  e -> error $ "Caught the error: " ++ show e
    Right a -> do
        void $ flip runBrainState emptyMachine $ eval a
        --(mem, logs) <- fst . flip runBrainState emptyMachine $ eval a
        --print mem
        --mapM_ putStrLn logs
```

- Please see here for details
    - [NicoLang.Parser.Items](https://github.com/aiya000/nico-lang/blob/001433d326cb2f86fd61ced0d6c5f2469eaf2788/src/NicoLang/Parser/Items.hs)
    - [Main](https://github.com/aiya000/nico-lang/blob/001433d326cb2f86fd61ced0d6c5f2469eaf2788/app/Main.hs)


# TODO
- Simplify the way of making brainf*ck dialect
