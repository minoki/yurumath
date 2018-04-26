{-# LANGUAGE FlexibleContexts #-}
module Text.YuruMath.TeX.State where
import Text.YuruMath.TeX.Types
import Data.Bits
import Data.Int
import Data.Word
import Data.Char
import Data.Text (Text)
import Data.Map (Map)
import Control.Monad.State.Class
import Control.Monad.Error.Class
import qualified Data.Map as Map
import Control.Lens.Cons (_head)
import Control.Lens.Getter (use)
import Control.Lens.Setter (assign,modifying)

initialState :: String -> TeXState a
initialState input = TeXState
                     { _ttInput = input
                     , _ttSpacingState = SSNewLine
                     , _esMaxDepth = 100
                     , _esMaxPendingToken = 100
                     , _esPendingTokenList = []
                     , _localStates = [initialLocalState]
                     , _mode = VerticalMode
                     , _conditionals = []
                     }
  where initialLocalState = LocalState
                            { _tsDefinitions = Map.empty
                            , _tsActiveDefinitions = Map.empty
                            , _ttCategoryCodeOf = Map.empty
                            , _lcCodes = Map.empty
                            , _ucCodes = Map.empty
                            , _mathCodes = Map.empty
                            , _delCodes = Map.empty
                            , _mathStyle = MathDisplayStyle -- nonsense in non-math mode
                            }

defaultCategoryCodeOf :: Char -> CatCode
defaultCategoryCodeOf c = case c of
  '\\' -> CCEscape
  '{'  -> CCBeginGroup
  '}'  -> CCEndGroup
  '$'  -> CCMathShift
  '&'  -> CCAlignmentTab
  '\n' -> CCEndLine
  '#'  -> CCParam
  '^'  -> CCSup
  '_'  -> CCSub
  '\0' -> CCIgnored
  ' '  -> CCSpace
  '\t' -> CCSpace -- LaTeX
  '~'  -> CCActive
  '%'  -> CCComment
  '\127' -> CCInvalid
  c | isLetter c -> CCLetter
  _ -> CCOther

categoryCodeOf :: MonadTeXState a m => Char -> m CatCode
categoryCodeOf c = do
  m <- use (localStates . _head . ttCategoryCodeOf)
  pure (Map.findWithDefault (defaultCategoryCodeOf c) c m)

categoryCodeFn :: MonadTeXState a m => m (Char -> CatCode)
categoryCodeFn = do
  m <- use (localStates . _head . ttCategoryCodeOf)
  pure (\c -> Map.findWithDefault (defaultCategoryCodeOf c) c m)

setCategoryCodeOf :: MonadTeXState a m => Char -> CatCode -> m ()
setCategoryCodeOf c cc = do
  modifying (localStates . _head . ttCategoryCodeOf) (Map.insert c cc)

lcCodeOf :: MonadTeXState a m => Char -> m Char
lcCodeOf c = do
  m <- use (localStates . _head . lcCodes)
  pure (Map.findWithDefault (if isAlpha c then toLower c else '\0') c m)

ucCodeOf :: MonadTeXState a m => Char -> m Char
ucCodeOf c = do
  m <- use (localStates . _head . lcCodes)
  pure (Map.findWithDefault (if isAlpha c then toUpper c else '\0') c m)

mkMathCode :: MathClass -> Word8 -> Char -> MathCode
mkMathCode cls fam code = MathCode
                          $ (fromIntegral (fromEnum cls) `shiftL` 12)
                          .|. (fromIntegral fam `shiftL` 8)
                          .|. fromIntegral (ord code)

mkUMathCode :: MathClass -> Word8 -> Char -> MathCode
mkUMathCode cls fam code = UMathCode
                           $ (fromIntegral (fromIntegral fam :: Int8) `shiftL` 24)
                           .|. (fromIntegral (fromEnum cls) `shiftL` 21)
                           .|. fromIntegral (ord code)

defaultMathCodeOf :: Char -> MathCode
defaultMathCodeOf c = case c of
{-  -- plain TeX:
  '.'  -> MathCode 0x013A -- ord, fam 1, 3A
  '/'  -> MathCode 0x013D -- ord, fam 1, 3D
  '\\' -> MathCode 0x026E -- ord, fam 2, 6E
  '|'  -> MathCode 0x026A -- ord, fam 2, 6A
  '+'  -> MathCode 0x202B
  -- '-'  -> MathCode 0x2200
  -- '*'  -> MathCode 0x2203
  -- ':'  -> MathCode 0x303A
  '='  -> MathCode 0x303D
  '<'  -> MathCode 0x313C
  '>'  -> MathCode 0x313E
  '('  -> MathCode 0x4028
  '['  -> MathCode 0x405B
  '{'  -> MathCode 0x4266
  '!'  -> MathCode 0x5021
  ')'  -> MathCode 0x5029
  '?'  -> MathCode 0x503F
  ']'  -> MathCode 0x505D
  '}'  -> MathCode 0x5267
  ';'  -> MathCode 0x603B
  ','  -> MathCode 0x613B
  ' '  -> MathCode 0x8000 -- active
  '\'' -> MathCode 0x8000 -- active
  '_'  -> MathCode 0x8000 -- active
-}
  -- LaTeX
  '!' -> mkMathCode MathClose operators '\x21'
  --'*'->mkMathCode MathBin   symbols   '\x03'
  '+' -> mkMathCode MathBin   operators '\x2B'
  ',' -> mkMathCode MathPunct letters   '\x3B'
  --'-'->mkMathCode MathBin   symbols   '\x00'
  '.' -> mkMathCode MathOrd   letters   '\x3A'
  --':'->mkMathCode MathRel   operators '\x3A'
  ';' -> mkMathCode MathPunct operators '\x3B'
  '=' -> mkMathCode MathRel   operators '\x3D'
  '?' -> mkMathCode MathClose operators '\x3F'
  '(' -> mkMathCode MathOpen  operators '\x28'
  ')' -> mkMathCode MathClose operators '\x29'
  '/' -> mkMathCode MathOrd   letters   '\x3D'
  '[' -> mkMathCode MathOpen  operators '\x5B'
  ']' -> mkMathCode MathClose operators '\x5D'
  '|' -> mkMathCode MathOrd   symbols   '\x6A'
  '<' -> mkMathCode MathRel   letters   '\x3C'
  '>' -> mkMathCode MathRel   letters   '\x3E'
  ' '  -> MathCode 0x8000 -- active
  '\'' -> MathCode 0x8000 -- active
  '_'  -> MathCode 0x8000 -- active

  -- unicode-math:
  '-'  -> mkUMathCode MathBin 0 '\x2212'
  '*'  -> mkUMathCode MathBin 0 '\x2217'
  ':'  -> mkUMathCode MathRel 0 '\x2236' -- ?

  _ | isAscii c && isLetter c -> MathCode (fromIntegral (0x7100 + ord c)) -- variable family
    | isDigit c -> MathCode (fromIntegral (0x7000 + ord c)) -- variable family
    | isAscii c -> MathCode (fromIntegral (ord c)) -- ord, family 0 (roman)
    | otherwise -> UMathCode (fromIntegral (ord c)) -- ???

  where
    letters = 1
    operators = 0
    symbols = 2 -- ?

mathCodeOf :: MonadTeXState a m => Char -> m MathCode
mathCodeOf c = do
  m <- use (localStates . _head . mathCodes)
  pure (Map.findWithDefault (defaultMathCodeOf c) c m)

isMathActive :: MonadTeXState a m => Char -> m Bool
isMathActive c = do
  mc <- mathCodeOf c
  return (mc == MathCode 0x8000)

mkUDelCode :: Word8 -> Char -> DelimiterCode
mkUDelCode fam code = UDelimiterCode
                      $ (fromIntegral fam `shiftL` 21)
                      .|. fromIntegral (ord code)

defaultDelimiterCodeOf :: Char -> DelimiterCode
defaultDelimiterCodeOf c = case c of
  '.' -> DelimiterCode 0 -- IniTeX

  -- plain TeX:
  '(' -> DelimiterCode 0x028300
  ')' -> DelimiterCode 0x029301
  '[' -> DelimiterCode 0x05B302
  ']' -> DelimiterCode 0x05D303
  -- '<' -> DelimiterCode 0x26830A
  -- '>' -> DelimiterCode 0x26930B
  '/' -> DelimiterCode 0x02F30E
  '|' -> DelimiterCode 0x26A30C
  '\\' -> DelimiterCode 0x26E30F

  -- unicode-math:
  '<' -> mkUDelCode 0 '\x27E8'
  '>' -> mkUDelCode 0 '\x27E9'

  _ -> DelimiterCode (-1) -- IniTeX

delimiterCodeOf :: MonadTeXState a m => Char -> m DelimiterCode
delimiterCodeOf c = do
  m <- use (localStates . _head . delCodes)
  pure (Map.findWithDefault (defaultDelimiterCodeOf c) c m)

enterGroup :: MonadTeXState a m => m ()
enterGroup = do
  modifying localStates (\ss -> head ss : ss)

leaveGroup :: (MonadTeXState a m, MonadError String m) => m ()
leaveGroup = do
  ss <- use localStates
  case ss of
    [] -> throwError "Cannot leave global scope"
    _:ss -> assign localStates ss
