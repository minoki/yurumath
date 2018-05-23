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
import Control.Lens.Getter (view,use)
import Control.Lens.Setter (set,assign,modifying)

initialState :: String -> CommonState (CommonLocalState e v)
initialState input = CommonState
                     { _tokenizerState = TokenizerState
                                         { tsInput         = input
                                         , tsSpacingState  = SSNewLine
                                         -- , tsCurrentLine   = 0
                                         -- , tsCurrentColumn = 0
                                         }
                     , _esMaxDepth = 100
                     , _esMaxPendingToken = 100
                     , _esPendingTokenList = []
                     , _localStates = [initialLocalState]
                     , _mode = VerticalMode
                     , _conditionals = []
                     }
  where initialLocalState = CommonLocalState
                            { _scopeType = GlobalScope
                            , _tsDefinitions = Map.empty
                            , _tsActiveDefinitions = Map.empty
                            , _catcodeMap = Map.empty
                            , _lccodeMap = Map.empty
                            , _uccodeMap = Map.empty
                            , _mathcodeMap = Map.empty
                            , _delcodeMap = Map.empty
                            , _endlinechar = ord '\r'
                            , _escapechar = ord '\\'
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
mkMathCode cls fam code
  | code <= '\xFF' = MathCode
                     $ (fromIntegral (fromEnum cls) `shiftL` 12)
                     .|. (fromIntegral fam `shiftL` 8)
                     .|. fromIntegral (ord code)
  | otherwise = error "Use mkUMathCode for code points beyond U+0100 "

mkUMathCode :: MathClass -> Word8 -> Char -> MathCode
mkUMathCode cls fam code = UMathCode
                           $ (fromIntegral (fromIntegral fam :: Int8) `shiftL` 24)
                           .|. (fromIntegral (fromEnum cls) `shiftL` 21)
                           .|. fromIntegral (ord code)

mathcharClass :: MathCode -> MathClass
mathcharClass (MathCode x) = toEnum $ fromIntegral $ 7 .&. (x `shiftR` 12)
mathcharClass (UMathCode x) = toEnum $ fromIntegral $ 7 .&. ((fromIntegral x :: Word32) `shiftR` 21)

mathcharFamily :: MathCode -> Word8
mathcharFamily (MathCode x) = toEnum $ fromIntegral $ 0xF .&. (x `shiftR` 8)
mathcharFamily (UMathCode x) = toEnum $ fromIntegral $ 0xFF .&. ((fromIntegral x :: Word32) `shiftR` 24)

mathcharSlot :: MathCode -> Char
mathcharSlot (MathCode x) = toEnum $ fromIntegral $ 0xFF .&. x
mathcharSlot (UMathCode x) = toEnum $ fromIntegral $ 0x1FFFFF .&. (fromIntegral x :: Word32)

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

delimiterFamilySmall :: DelimiterCode -> Word8
delimiterFamilySmall (DelimiterCode x) = fromIntegral (0xF .&. (x `shiftR` 20))
delimiterFamilySmall (UDelimiterCode x) = let u = fromIntegral x :: Word32
                                          in fromIntegral (u `shiftR` 24)

delimiterSlotSmall :: DelimiterCode -> Char
delimiterSlotSmall (DelimiterCode x) = chr $ fromIntegral (0xFF .&. (x `shiftR` 12))
delimiterSlotSmall (UDelimiterCode x) = let u = fromIntegral x :: Word32
                                        in chr $ fromIntegral (0x1FFFFF .&. u)

delimiterFamilyLarge :: DelimiterCode -> Word8
delimiterFamilyLarge (DelimiterCode x) = fromIntegral (0xF .&. (x `shiftR` 8))
delimiterFamilyLarge (UDelimiterCode x) = let u = fromIntegral x :: Word32
                                          in fromIntegral (u `shiftR` 24)

delimiterSlotLarge :: DelimiterCode -> Char
delimiterSlotLarge (DelimiterCode x) = chr $ fromIntegral (0xFF .&. x)
delimiterSlotLarge (UDelimiterCode x) = let u = fromIntegral x :: Word32
                                        in chr $ fromIntegral (0x1FFFFF .&. u)

defaultDelimiterCodeOf :: Char -> DelimiterCode
defaultDelimiterCodeOf c = case c of
  '.' -> DelimiterCode 0 -- IniTeX

  {-
  -- plain TeX:
  '(' -> DelimiterCode 0x028300
  ')' -> DelimiterCode 0x029301
  '[' -> DelimiterCode 0x05B302
  ']' -> DelimiterCode 0x05D303
  '<' -> DelimiterCode 0x26830A
  '>' -> DelimiterCode 0x26930B
  '/' -> DelimiterCode 0x02F30E
  '|' -> DelimiterCode 0x26A30C
  '\\' -> DelimiterCode 0x26E30F
  -}

  -- unicode-math:
  '('  -> mkUDelCode 0 '('      -- \mathopen
  ')'  -> mkUDelCode 0 ')'      -- \mathclose
  '['  -> mkUDelCode 0 '['      -- \mathopen
  ']'  -> mkUDelCode 0 ']'      -- \mathclose
  '<'  -> mkUDelCode 0 '\x27E8' -- set in \@@_setup_delcodes
  '>'  -> mkUDelCode 0 '\x27E9' -- set in \@@_setup_delcodes
  '/'  -> mkUDelCode 0 '/'      -- set in \@@_setup_delcodes, subject to slash-delimiter option
  '|'  -> mkUDelCode 0 '|'      -- \mathfence
  '\\' -> mkUDelCode 0 '\\'     -- set in \@@_setup_delcodes

  _ -> DelimiterCode (-1) -- IniTeX

delimiterCodeOf :: MonadTeXState a m => Char -> m DelimiterCode
delimiterCodeOf c = do
  m <- use (localState . delcodeMap)
  pure (Map.findWithDefault (defaultDelimiterCodeOf c) c m)

enterGroup :: MonadTeXState a m => ScopeType -> m ()
enterGroup !st = do
  modifying localStates (\ss -> set scopeType st (head ss) : ss)

leaveGroup :: (MonadTeXState a m, MonadError String m) => ScopeType -> m ()
leaveGroup !st = do
  ss <- use localStates
  case ss of
    [] -> error "local state stack is empty"
    s:ss | view scopeType s == st -> assign localStates ss
         | otherwise -> throwError $ "Mismatched braces: begun by " ++ beginning (view scopeType s) ++ ", ended by " ++ ending st
  where beginning ScopeByBrace = "left brace `{'"
        beginning ScopeByBeginGroup = "\\begingroup"
        beginning ScopeByLeftRight = "\\left" -- or \middle
        beginning GlobalScope = "<beginning of input>"
        ending ScopeByBrace = "right brace `}'"
        ending ScopeByBeginGroup = "\\endgroup"
        ending ScopeByLeftRight = "\\right" -- or \middle
        ending GlobalScope = "<end of input>"
