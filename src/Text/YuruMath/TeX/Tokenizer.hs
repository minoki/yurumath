{-# LANGUAGE FlexibleContexts #-}
module Text.YuruMath.TeX.Tokenizer where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.State (defaultCategoryCodeOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Bits (xor)
import Data.Char
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Lens.Getter (use)
import Control.Lens.Setter (assign)
import qualified Data.Map.Strict as Map
import Numeric (readHex)

data TokenizerContext = TokenizerContext { tcCatcodeMap :: !(Map.Map Char CatCode)
                                         , tcEndlineChar :: !Int
                                         , tcCurrentFile :: !Text
                                         }

defaultTokenizerContext :: TokenizerContext
defaultTokenizerContext = TokenizerContext
                          { tcCatcodeMap = Map.empty
                          , tcEndlineChar = ord '\r'
                          , tcCurrentFile = ""
                          }

latexInternalTokenizerContext :: TokenizerContext
latexInternalTokenizerContext = TokenizerContext
                                { tcCatcodeMap = Map.singleton '@' CCLetter
                                , tcEndlineChar = ord '\r'
                                , tcCurrentFile = ""
                                }

type TokenizerM a = ReaderT TokenizerContext (StateT TokenizerState (Except String)) a

runTokenizer :: (MonadState s m, IsState s, MonadError String m) => TokenizerM a -> m a
runTokenizer m = do
  ccmap <- use (localState . catcodeMap)
  elc <- use (localState . endlinechar)
  let context = TokenizerContext
                { tcCatcodeMap = ccmap
                , tcEndlineChar = elc
                , tcCurrentFile = ""
                }
  initialState <- use tokenizerState
  case runExcept (runStateT (runReaderT m context) initialState) of
    Left err -> throwError err
    Right (result,state) -> do
      assign tokenizerState state
      return result

tokenizeString :: TokenizerContext -> String -> Either String [TeXToken]
tokenizeString context input = runExcept (evalStateT (runReaderT doAll context) initialState)
  where
    initialState = TokenizerState { tsInputLines = lines input
                                  , tsSpacingState = SSNewLine
                                  }
    doAll = do t <- doNextTokenM
               case t of
                 Nothing -> return []
                 Just t -> (t :) <$> doAll

nextToken :: (MonadState s m, IsState s, MonadError String m) => m (Maybe TeXToken)
nextToken = runTokenizer doNextTokenM

doNextTokenM :: TokenizerM (Maybe TeXToken)
doNextTokenM = ReaderT (\ctx -> StateT (doNextToken ctx))

appendEndlinechar :: Int -> String -> String
appendEndlinechar !elchar line | isUnicodeScalarValue elchar = line ++ [chr elchar]
                               | otherwise = line

appendEndlinecharOnNextLine :: Int -> [String] -> [String]
appendEndlinecharOnNextLine !_ [] = []
appendEndlinecharOnNextLine !elchar (l:ls) = appendEndlinechar elchar l : ls

data LineTokenizationResult = LineEnded !(Maybe TeXToken)
                            | MiddleOfLine !TeXToken !SpacingState !String

doNextToken :: TokenizerContext -> TokenizerState -> Except String (Maybe TeXToken, TokenizerState)
doNextToken (TokenizerContext { tcCatcodeMap = !ccmap, tcEndlineChar = !elchar })
  (TokenizerState { tsInputLines = inputLines, tsSpacingState = ss }) = doNextTokenLoop inputLines ss
  where
    catCodeOf !c = Map.findWithDefault (defaultCategoryCodeOf c) c ccmap
    isTeXLetter !c = catCodeOf c == CCLetter
    isSup !c = catCodeOf c == CCSup

    doNextTokenLoop :: [String] -> SpacingState -> Except String (Maybe TeXToken, TokenizerState)
    doNextTokenLoop [] _ = return (Nothing, TokenizerState { tsInputLines = [], tsSpacingState = ss }) -- end of input
    doNextTokenLoop (currentLine : nextLines) ss = do
      result <- doNextTokenInLine False currentLine ss
      let nextLines' = appendEndlinecharOnNextLine elchar nextLines
      case result of
        Nothing ->
          doNextTokenLoop nextLines' SSNewLine -- move to new line

        Just (tok, _ {-should be empty-}, SSNewLine) ->
          return (Just tok, TokenizerState { tsInputLines = nextLines', tsSpacingState = SSNewLine }) -- move to new line

        Just (tok, restOfLine, ss) ->
          return (Just tok, TokenizerState { tsInputLines = restOfLine : nextLines, tsSpacingState = ss }) -- middle of line

    doNextTokenInLine :: Bool -> String -> SpacingState -> Except String (Maybe (TeXToken, String, SpacingState))

    -- reached end of line without an end-of-line character
    doNextTokenInLine _ "" !_ss = return Nothing

    doNextTokenInLine !_synthesized (c:cs) !ss
      = case catCodeOf c of
        { CCEscape -> case cs of
            [] -> return $ Just (TTCommandName $ NControlSeq "", "", SSNewLine)

            -- ^^ notation
            c1:c2:css | isSup c1, c1 == c2, Just res <- parseSuperscriptNotation c2 css ->
                        case res of
                          Left err -> throwError err
                          Right (ch,csss)
                            | isTeXLetter ch -> throwError "^^ notation in control word is not supported"
                            | otherwise -> doEscapeChar ch csss

            c1:css -> doEscapeChar c1 css

        ; CCEndLine -> case ss of
                         SSNewLine ->      return $ Just (TTCommandName (NControlSeq "par"), "", SSNewLine)
                         SSSkipSpaces ->   return Nothing -- generate no token at line end
                         SSMiddleOfLine -> return $ Just (TTCharacter ' ' CCSpace, "", SSNewLine)

        ; CCSpace -> case ss of
            SSNewLine -> doNextTokenInLine False cs SSNewLine
            SSSkipSpaces -> doNextTokenInLine False cs SSSkipSpaces
            SSMiddleOfLine ->
               -- always produce a token with character code 32
              return $ Just (TTCharacter ' ' CCSpace, cs, SSSkipSpaces)

        ; CCComment -> return Nothing -- move to next line without producting a token

        ; CCIgnored -> doNextTokenInLine False cs ss

        ; CCSup -- ^^ notation
          | c2:css <- cs
          , c2 == c
          , Just res <- parseSuperscriptNotation c2 css ->
              case res of
                Left err -> throwError err
                Right (ch,csss) -> doNextTokenInLine True (ch:csss) ss

        ; CCActive -> return $ Just (TTCommandName (NActiveChar c), cs, SSMiddleOfLine)

        ; cc ->
            -- CCBeginGroup, CCEndGroup, CCMathShift, CCAlignmentTab
            -- CCParam, CCSup, CCSub, CCLetter, CCOther
            return $ Just (TTCharacter c cc, cs, SSMiddleOfLine)
        }

    doEscapeChar :: Char -> String -> Except String (Maybe (TeXToken, String, SpacingState))
    doEscapeChar !c1 css = case catCodeOf c1 of
      { CCLetter -> do -- control word
          let (w,csss) = span isTeXLetter css

          -- check that no letter with ^^ notation follows
          case csss of
            s1:s2:cssss | s1 == s2 && isSup s1
                        , Just (Right (ch,_)) <- parseSuperscriptNotation s2 cssss
                        , isTeXLetter ch -> throwError "^^ notation in control word is not supported"
            _ -> return $ Just (TTCommandName $ NControlSeq $ T.pack $ c1:w, csss, SSSkipSpaces)

      ; CCSpace -> -- control space: always produce a token with character code 32
          return $ Just (TTCommandName $ NControlSeq " ", css, SSSkipSpaces)

      ; _ -> -- control symbol
          return $ Just (TTCommandName $ NControlSeq $ T.singleton c1, css, SSMiddleOfLine)
      }

parseSuperscriptNotation :: Char -> String -> Maybe (Either String (Char,String))
-- LuaTeX and XeTeX's extension: ^^^^^^xxxxxx (six hex digits)
parseSuperscriptNotation !c2 (c3:c4:c5:c6:css)
  | c2 == c3 && c2 == c4 && c2 == c5 && c2 == c6 = Just $ case css of
      x1:x2:x3:x4:x5:x6:csss
        | all isLowerHexDigit [x1,x2,x3,x4,x5,x6]
        , [(value,"")] <- readHex [x1,x2,x3,x4,x5,x6] ->
            if isUnicodeScalarValue value
            then return (chr value, csss)
            else throwError "Invalid Unicode character"
      _ -> throwError "^^^^^^ needs six hex digits"

-- LuaTeX and XeTeX's extension: ^^^^xxxx (four hex digits)
parseSuperscriptNotation !c2 (c3:c4:css)
  | c2 == c3 && c2 == c4 = Just $ case css of
      x1:x2:x3:x4:csss
        | all isLowerHexDigit [x1,x2,x3,x4]
        , [(value,"")] <- readHex [x1,x2,x3,x4] ->
            if isUnicodeScalarValue value
            then return (chr value, csss)
            else throwError "Invalid Unicode character"
      _ -> throwError "^^^^ needs four hex digits"

-- TeX3: ^^xx (two hex digits)
parseSuperscriptNotation _c2 (x1:x2:css)
  | isLowerHexDigit x1 && isLowerHexDigit x2 =
      Just $ Right (chr (16 * digitToInt x1 + digitToInt x2), css)

-- ^^X, like ^^M (carriage return), ^^I (tab), ^^? (delete)
parseSuperscriptNotation _c2 (d:css)
  | ord d < 0x80 = Just $ Right (chr (ord d `xor` 0x40), css)

parseSuperscriptNotation _c2 _ = Nothing

doComment :: String -> String
doComment [] = []
doComment ('\n':xs) = xs -- strip end line character
doComment (_:xs) = doComment xs

isLowerHexDigit :: Char -> Bool
isLowerHexDigit c = isHexDigit c && isLower c
