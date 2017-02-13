# brainhack
The library for making brainf*ck dialect :+1:


# Example
- These are implemented by brainhack
    - [nico-lang](https://github.com/aiya000/nico-lang)
        - detailed
    - [hs-kemono-friends](https://github.com/aiya000/hs-kemono-friends)
        - easily


# How to implement brainf*ck dialect with this ?
- 1: Define your Text newtype for your brainf*ck tokens

```haskell
newtype NicoToken = NicoToken { unNicoToken :: Text }
  deriving (Eq, IsString)
```

- 2: Define `BrainfuckToken` instance

```haskell
instance BrainfuckToken NicoToken where
  forwardToken   = "笑顔届ける矢澤にこにこ！"
  backwordToken  = "だめだめだめっ！"
  incrToken      = "にっこにっこにー"
  decrToken      = "にこにーって覚えてラブニコ！"
  outputToken    = "ぴょんぴょんぴょんっ！"
  inputToken     = "あなたのハートににこにこにー！"
  loopBeginToken = "にこにーはみんなのもの！"
  loopEndToken   = "ｷﾓﾁﾜﾙｲ"
  toText         = unNicoToken
  fromText       = NicoToken
  toOperator     = flip lookup [ ("笑顔届ける矢澤にこにこ！", ForwardOp)
                               , ("だめだめだめっ！", BackwardOp)
                               , ("にっこにっこにー", IncrOp)
                               , ("にこにーって覚えてラブニコ！", DecrOp)
                               , ("ぴょんぴょんぴょんっ！", OutputOp)
                               , ("あなたのハートににこにこにー！", InputOp)
                               , ("にこにーはみんなのもの！", LoopBeginOp)
                               , ("ｷﾓﾁﾜﾙｲ", LoopEndOp)
                               ]

```

- 3: Run `parse` and `eval`

```haskell
main :: IO ()
main = do
  nicoCode <- NicoToken . pack <$> readFile "hello.nico"
  case parse nicoCode of
    Left  e -> error $ "Caught the error: " ++ show (e :: SomeException)
    Right a -> do
        void $ flip runBrainState emptyMachine $ eval a
        --(mem, logs) <- fst . flip runBrainState emptyMachine $ eval a
        --print mem
        --mapM_ putStrLn logs
```
