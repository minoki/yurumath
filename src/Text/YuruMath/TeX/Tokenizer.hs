{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.YuruMath.TeX.Tokenizer where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.State
import Data.Text (Text)
import qualified Data.Text as T
import Data.Bits (xor)
import Data.Char
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Control.Lens.Getter (use)
import Control.Lens.Setter (assign,modifying)

maybeNextChar :: MonadTeXState a m => m (Maybe Char)
maybeNextChar = do input <- use ttInput
                   case input of
                     [] -> return Nothing
                     x:xs -> do
                       assign ttInput xs
                       return (Just x)

nextChar :: MonadTeXState a m => MaybeT m Char
nextChar = MaybeT maybeNextChar

peekChar :: MonadTeXState a m => m (Maybe Char)
peekChar = do input <- use ttInput
              return $ case input of
                [] -> Nothing
                x:_ -> Just x

unreadChar :: MonadTeXState a m => Char -> m ()
unreadChar c = modifying ttInput (c :)

readControlWord :: MonadTeXState a m => m String
readControlWord = do
  -- TODO: Handle ^^ notation
  c <- maybeNextChar
  case c of
    Nothing -> return ""
    Just c -> do
      cc <- categoryCodeOf c
      if cc == CCLetter
        then (c:) <$> readControlWord
        else do unreadChar c
                return ""

setNewLineMode, setSkipSpacesMode, setMiddleOfLineMode :: MonadTeXState a m => m ()
setNewLineMode = assign ttSpacingState SSNewLine
setSkipSpacesMode = assign ttSpacingState SSSkipSpaces
setMiddleOfLineMode = assign ttSpacingState SSMiddleOfLine

skipComment :: MonadTeXState a m => m ()
skipComment = do c <- maybeNextChar
                 if c == Just '\n' || c == Nothing
                   then return ()
                   else skipComment

assertMoreInput :: (MonadTeXState a m, MonadError String m) => String -> m ()
assertMoreInput s = do input <- use ttInput
                       case input of
                         [] -> throwError s
                         _:_ -> return ()

nextToken :: (MonadTeXState a m, MonadError String m) => m (Maybe TeXToken)
nextToken = do
  c <- maybeNextChar
  case c of
    Nothing -> return Nothing
    Just c -> do
      catCodeOf <- categoryCodeFn
      ss <- use ttSpacingState
      case catCodeOf c of
        CCEscape -> do
          assertMoreInput "control sequence"
          w <- readControlWord
          if null w
            then do -- control symbol
              Just d <- maybeNextChar
              case catCodeOf d of
                CCSpace -> do
                  setSkipSpacesMode
                  return $ Just $ TTControlSeq " " -- control space
                CCEndLine -> do
                  setNewLineMode
                  -- TODO: read \endlinechar and emit it
                  return $ Just $ TTControlSeq " " -- control space
                _ -> do
                  setMiddleOfLineMode
                  return $ Just $ TTControlSeq (T.singleton d)
            else do -- control word
              setSkipSpacesMode
              return $ Just $ TTControlSeq (T.pack w)
        CCEndLine -> case ss of
                       SSNewLine -> return $ Just $ TTControlSeq "par"
                       SSSkipSpaces -> do setNewLineMode
                                          nextToken
                       SSMiddleOfLine -> do setNewLineMode
                                            return $ Just $ TTCharacter ' ' CCSpace
        CCSpace -> case ss of
                     SSNewLine -> nextToken
                     SSSkipSpaces -> nextToken
                     SSMiddleOfLine -> do setSkipSpacesMode
                                          return $ Just $ TTCharacter ' ' CCSpace
        CCComment -> do skipComment
                        setNewLineMode
                        nextToken
        CCIgnored -> nextToken
        CCSup -> do d <- maybeNextChar
                    case d of
                      Just d | d == c -> do
                                 assertMoreInput "double superscript"
                                 Just e <- maybeNextChar
                                 if ord e <= 0x7f
                                   then do unreadChar (chr (ord e `xor` 0x40))
                                           nextToken
                                   else throwError "invalid double superscript notation for >= U+0080"
                                 -- TODO: ^^xx
                                 -- TODO: ^^^^xxxx, ^^^^^^xxxxxx (LuaTeX extension)
                             | otherwise -> do
                                 unreadChar d
                                 setMiddleOfLineMode
                                 return $ Just $ TTCharacter c CCSup
                      Nothing -> return $ Just $ TTCharacter c CCSup
        cc -> do
          -- CCBeginGroup, CCEndGroup, CCMathShift, CCAlignmentTab
          -- CCParam, CCSub, CCLetter, CCOther, CCActive
          setMiddleOfLineMode
          return $ Just $ TTCharacter c cc

-- context: Char -> CatCode
-- state: SSNewLine, SSSkipSpaces, SSMiddleOfLine
