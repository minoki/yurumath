{-# LANGUAGE FlexibleContexts #-}
module Main where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Tokenizer
import Control.Monad.State.Strict
import Control.Monad.Except

tokenizeAll :: (MonadState (TeXState a) m, MonadError String m) => m [TeXToken]
tokenizeAll = do
  t <- nextToken
  case t of
    Nothing -> return []
    Just t -> (t:) <$> tokenizeAll

main :: IO ()
main = do
  input <- getContents
  let tokenResult = runExcept (runStateT tokenizeAll (initialState input))
  case tokenResult of
    Left err -> putStrLn ("tokenize error: " ++ err)
    Right (tokenList, _state) -> do
      forM_ tokenList $ \t -> do
        putStrLn (show t)
