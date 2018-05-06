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
import Control.Lens.Getter (use)
import Control.Lens.Setter (assign,modifying)

initialState :: String -> CommonState (CommonLocalState e v)
initialState input = CommonState
                     { _ttInput = input
                     , _ttSpacingState = SSNewLine
                     , _esMaxDepth = 100
                     , _esMaxPendingToken = 100
                     , _esPendingTokenList = []
                     , _localStates = [initialLocalState]
                     , _mode = VerticalMode
                     , _conditionals = []
                     }
  where initialLocalState = CommonLocalState
                            { _tsDefinitions = Map.empty
                            , _tsActiveDefinitions = Map.empty
                            , _catcodeMap = Map.empty
                            , _lccodeMap = Map.empty
                            , _uccodeMap = Map.empty
                            , _mathcodeMap = Map.empty
                            , _delcodeMap = Map.empty
                            , _mathStyle = DisplayStyle -- nonsense in non-math mode
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
  m <- use (localState . catcodeMap)
  pure (Map.findWithDefault (defaultCategoryCodeOf c) c m)

categoryCodeFn :: MonadTeXState a m => m (Char -> CatCode)
categoryCodeFn = do
  m <- use (localState . catcodeMap)
  pure (\c -> Map.findWithDefault (defaultCategoryCodeOf c) c m)

setCategoryCodeOf :: MonadTeXState a m => Char -> CatCode -> m ()
setCategoryCodeOf c cc = do
  modifying (localState . catcodeMap) (Map.insert c cc)

defaultLCCodeOf :: Char -> Char
defaultLCCodeOf c | isAlpha c = toLower c
                  | otherwise = '\0'

lcCodeOf :: MonadTeXState a m => Char -> m Char
lcCodeOf c = do
  m <- use (localState . lccodeMap)
  pure (Map.findWithDefault (defaultLCCodeOf c) c m)

lcCodeFn :: MonadTeXState a m => m (Char -> Char)
lcCodeFn = do
  m <- use (localState . lccodeMap)
  pure (\c -> Map.findWithDefault (defaultLCCodeOf c) c m)

defaultUCCodeOf :: Char -> Char
defaultUCCodeOf c | isAlpha c = toUpper c
                  | otherwise = '\0'

ucCodeOf :: MonadTeXState a m => Char -> m Char
ucCodeOf c = do
  m <- use (localState . uccodeMap)
  pure (Map.findWithDefault (defaultUCCodeOf c) c m)

ucCodeFn :: MonadTeXState a m => m (Char -> Char)
ucCodeFn = do
  m <- use (localState . uccodeMap)
  pure (\c -> Map.findWithDefault (defaultUCCodeOf c) c m)

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
  -- plain TeX & LaTeX
  '!' -> mkMathCode MathClose operators '!'    -- "21
  --'*'->mkMathCode MathBin   symbols   '\x03' -- "03
  '+' -> mkMathCode MathBin   operators '+'    -- "2B
  ',' -> mkMathCode MathPunct letters   '\x3B' -- "3B
  --'-'->mkMathCode MathBin   symbols   '\x00' -- "00
  '.' -> mkMathCode MathOrd   letters   '\x3A' -- "3A
  --':'->mkMathCode MathRel   operators ':'    -- "3A
  ';' -> mkMathCode MathPunct operators ';'    -- "3B
  '=' -> mkMathCode MathRel   operators '='    -- "3D
  '?' -> mkMathCode MathClose operators '?'    -- "3F
  '(' -> mkMathCode MathOpen  operators '('    -- "28
  ')' -> mkMathCode MathClose operators ')'    -- "29
  '/' -> mkMathCode MathOrd   letters   '\x3D' -- "3D
  '[' -> mkMathCode MathOpen  operators '['    -- "5B
  ']' -> mkMathCode MathClose operators ']'    -- "5D
  '|' -> mkMathCode MathOrd   symbols   '\x6A' -- "6A
  '<' -> mkMathCode MathRel   letters   '<'    -- "3C
  '>' -> mkMathCode MathRel   letters   '>'    -- "3E
  '\\'-> mkMathCode MathOrd   symbols   '\x6E' -- "6E
  '{' -> mkMathCode MathOpen  symbols   '\x66' -- "66 (plain TeX only)
  '}' -> mkMathCode MathClose symbols   '\x67' -- "67 (plain TeX only)
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
    operators = 0
    letters = 1
    symbols = 2 -- ?

mathCodeOf :: MonadTeXState a m => Char -> m MathCode
mathCodeOf c = do
  m <- use (localState . mathcodeMap)
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
  m <- use (localState . delcodeMap)
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
