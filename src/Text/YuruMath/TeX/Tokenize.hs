{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.YuruMath.TeX.Tokenize where
import Text.YuruMath.TeX.Token
import Data.Text (Text)
import qualified Data.Text as T
import Data.Bits (xor)
import Data.Char
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State.Class
import Control.Monad.Error.Class

defaultCategoryCode :: Char -> CatCode
defaultCategoryCode c = case c of
  '\\' -> CCEscape
  '{' -> CCBeginGroup
  '}' -> CCEndGroup
  '$' -> CCMathShift
  '&' -> CCAlignmentTab
  '\n' -> CCEndLine
  '#' -> CCParam
  '^' -> CCSup
  '_' -> CCSub
  '\0' -> CCIgnored
  ' ' -> CCSpace
  '\t' -> CCSpace -- LaTeX
  '~' -> CCActive
  '%' -> CCComment
  '\127' -> CCInvalid
  c | isLetter c -> CCLetter
  _ -> CCOther

data SpacingState = SSNewLine
                  | SSSkipSpaces
                  | SSMiddleOfLine
                  deriving (Eq,Show)

data TeXTokenState = TeXTokenState { ttInput :: !String
                                   , ttCategoryCodeOf :: Char -> CatCode
                                   , ttSpacingState :: !SpacingState
                                   -- currentfile, currentline, currentcolumn
                                   }

initialTeXTokenState :: String -> TeXTokenState
initialTeXTokenState s = TeXTokenState { ttInput = s
                                       , ttCategoryCodeOf = defaultCategoryCode
                                       , ttSpacingState = SSNewLine
                                       }

maybeNextChar :: MonadState TeXTokenState m => m (Maybe Char)
maybeNextChar = do input <- gets ttInput
                   case input of
                     [] -> return Nothing
                     x:xs -> do
                       modify (\s -> s { ttInput = xs })
                       return (Just x)

nextChar :: MonadState TeXTokenState m => MaybeT m Char
nextChar = MaybeT maybeNextChar

peekChar :: MonadState TeXTokenState m => m (Maybe Char)
peekChar = do input <- gets ttInput
              case input of
                [] -> return Nothing
                x:_ -> return (Just x)

unreadChar :: MonadState TeXTokenState m => Char -> m ()
unreadChar c = modify (\s -> s { ttInput = c : ttInput s })

categoryCodeOf :: MonadState TeXTokenState m => Char -> m CatCode
categoryCodeOf c = gets ttCategoryCodeOf <*> pure c

readControlWord :: MonadState TeXTokenState m => m String
readControlWord = do
  c <- maybeNextChar
  case c of
    Nothing -> return ""
    Just c -> do
      cc <- categoryCodeOf c
      if cc == CCLetter
        then (c:) <$> readControlWord
        else do unreadChar c
                return ""

setNewLineMode, setSkipSpacesMode, setMiddleOfLineMode :: MonadState TeXTokenState m => m ()
setNewLineMode = modify (\s -> s { ttSpacingState = SSNewLine })
setSkipSpacesMode = modify (\s -> s { ttSpacingState = SSSkipSpaces })
setMiddleOfLineMode = modify (\s -> s { ttSpacingState = SSMiddleOfLine })

skipComment :: MonadState TeXTokenState m => m ()
skipComment = do c <- maybeNextChar
                 if c == Just '\n' || c == Nothing
                   then return ()
                   else skipComment

assertMoreInput :: (MonadState TeXTokenState m, MonadError String m) => String -> m ()
assertMoreInput s = do input <- gets ttInput
                       case input of
                         [] -> throwError s
                         _:_ -> return ()

nextToken :: (MonadState TeXTokenState m, MonadError String m) => m (Maybe TeXToken)
nextToken = do
  c <- maybeNextChar
  case c of
    Nothing -> return Nothing
    Just c -> do
      ss <- gets ttSpacingState
      cc <- categoryCodeOf c
      case cc of
        CCEscape -> do
          assertMoreInput "control sequence"
          w <- readControlWord
          if null w
            then do -- control symbol
              Just d <- maybeNextChar
              cc <- categoryCodeOf d
              if cc == CCSpace
                then setSkipSpacesMode
                else setMiddleOfLineMode
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
        CCParam -> do c <- maybeNextChar
                      setMiddleOfLineMode
                      case c of
                        Just c | isDigit c && c /= '0' -> return $ Just $ TTParameter (digitToInt c)
                               | otherwise -> return $ Just $ TTCharacter c cc
                        Nothing -> return Nothing
        CCSup -> do d <- maybeNextChar
                    case d of
                      Just d | d == c -> do
                                 assertMoreInput "double superscript"
                                 Just e <- maybeNextChar
                                 if ord e <= 0x7f
                                   then do unreadChar (chr (ord e `xor` 0x40))
                                           nextToken
                                   else throwError "invalid double superscript notation for >= U+0080"
                             | otherwise -> do
                                 unreadChar d
                                 setMiddleOfLineMode
                                 return $ Just $ TTCharacter c cc
                      Nothing -> return $ Just $ TTCharacter c cc
        _ -> do
          -- CCBeginGroup, CCEndGroup, CCMathShift, CCAlignmentTab
          -- CCSub, CCLetter, CCOther, CCActive
          setMiddleOfLineMode
          return $ Just $ TTCharacter c cc
