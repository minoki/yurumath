{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Main where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Tokenizer
import Text.YuruMath.TeX.Expansion
import Text.YuruMath.TeX.Execution
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.OpenUnion

tokenizeAll :: (MonadTeXState s m, MonadError String m, Value s ~ CommonValue, Expandable s ~ Union '[ConditionalMarker, CommonExpandable, CommonBoolean]) => m [TeXToken]
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
