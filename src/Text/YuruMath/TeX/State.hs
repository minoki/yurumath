{-# LANGUAGE FlexibleContexts #-}
module Text.YuruMath.TeX.State where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Quantity
import Data.Char
import Control.Monad.State.Class
import Control.Monad.Error.Class
import qualified Data.Map.Strict as Map
import Control.Lens.Getter (view,use)
import Control.Lens.Setter (set,assign,modifying)

initialLocalState :: CommonLocalState e v
initialLocalState = CommonLocalState
                            { _scopeType = GlobalScope
                            , _controlSeqDef = Map.empty
                            , _activeDef = Map.empty
                            , _catcodeMap = Map.empty
                            , _lccodeMap = Map.empty
                            , _uccodeMap = Map.empty
                            , _mathcodeMap = Map.empty
                            , _delcodeMap = Map.empty
                            , _endlinechar = ord '\r'
                            , _escapechar = ord '\\'
                            , _countReg = Map.empty
                            , _dimenReg = Map.empty
                            , _skipReg = Map.empty
                            , _muskipReg = Map.empty
                            , _toksReg = Map.empty
                            , _thinmuskip = Glue { glueSpace = mu 3, glueStretch = zeroQ, glueShrink = zeroQ } -- 3mu
                            , _medmuskip = Glue { glueSpace = mu 4, glueStretch = FixedSS (mu 2), glueShrink = FixedSS (mu 4) } -- 4mu plus 2mu minus 4mu
                            , _thickmuskip = Glue { glueSpace = mu 5, glueStretch = FixedSS (mu 5), glueShrink = zeroQ } -- 5mu plus 5mu
                            }

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

initialStateWithLocalState :: l -> String -> CommonState l
initialStateWithLocalState localState input = CommonState
  { _tokenizerState = TokenizerState
                      { tsInput         = input
                      , tsSpacingState  = SSNewLine
                      -- , tsCurrentLine   = 0
                      -- , tsCurrentColumn = 0
                      }
  , _esMaxDepth = 100
  , _esMaxPendingToken = 100
  , _esPendingTokenList = []
  , _localStates = [localState]
  , _mode = VerticalMode
  , _conditionals = []
  }

defaultCategoryCodeOf :: Char -> CatCode
defaultCategoryCodeOf c = case c of
  '\\' -> CCEscape
  '{'  -> CCBeginGroup
  '}'  -> CCEndGroup
  '$'  -> CCMathShift
  '&'  -> CCAlignmentTab
  '\r' -> CCEndLine
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

categoryCodeOf :: (MonadState s m, IsState s) => Char -> m CatCode
categoryCodeOf c = do
  m <- use (localState . catcodeMap)
  pure (Map.findWithDefault (defaultCategoryCodeOf c) c m)

categoryCodeFn :: (MonadState s m, IsState s) => m (Char -> CatCode)
categoryCodeFn = do
  m <- use (localState . catcodeMap)
  pure (\c -> Map.findWithDefault (defaultCategoryCodeOf c) c m)

defaultLCCodeOf :: Char -> Char
defaultLCCodeOf c | isAlpha c = toLower c
                  | otherwise = '\0'

lcCodeOf :: (MonadState s m, IsState s) => Char -> m Char
lcCodeOf c = do
  m <- use (localState . lccodeMap)
  pure (Map.findWithDefault (defaultLCCodeOf c) c m)

lcCodeFn :: (MonadState s m, IsState s) => m (Char -> Char)
lcCodeFn = do
  m <- use (localState . lccodeMap)
  pure (\c -> Map.findWithDefault (defaultLCCodeOf c) c m)

defaultUCCodeOf :: Char -> Char
defaultUCCodeOf c | isAlpha c = toUpper c
                  | otherwise = '\0'

ucCodeOf :: (MonadState s m, IsState s) => Char -> m Char
ucCodeOf c = do
  m <- use (localState . uccodeMap)
  pure (Map.findWithDefault (defaultUCCodeOf c) c m)

ucCodeFn :: (MonadState s m, IsState s) => m (Char -> Char)
ucCodeFn = do
  m <- use (localState . uccodeMap)
  pure (\c -> Map.findWithDefault (defaultUCCodeOf c) c m)

defaultMathCodeOf :: Char -> MathCode
defaultMathCodeOf c = case c of
  {-
  -- plain TeX & LaTeX
  '!' -> mkMathCode MathClose operators '!'    -- "21
  '*' -> mkMathCode MathBin   symbols   '\x03' -- "03
  '+' -> mkMathCode MathBin   operators '+'    -- "2B
  ',' -> mkMathCode MathPunct letters   '\x3B' -- "3B
  '-' -> mkMathCode MathBin   symbols   '\x00' -- "00
  '.' -> mkMathCode MathOrd   letters   '\x3A' -- "3A
  ':' -> mkMathCode MathRel   operators ':'    -- "3A
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
  -}
  ' '  -> mathActive
  '\'' -> mathActive
  '_'  -> mathActive

  -- unicode-math:
  '!'  -> mkUMathCode MathClose 0 '!' -- a postfix operator
  '+'  -> mkUMathCode MathBin   0 '+'
  ','  -> mkUMathCode MathPunct 0 ','
  '.'  -> mkUMathCode MathOrd   0 '.'
  ';'  -> mkUMathCode MathPunct 0 ';'
  '='  -> mkUMathCode MathRel   0 '='
  '?'  -> mkUMathCode MathClose 0 '?' -- a postfix operator
  '('  -> mkUMathCode MathOpen  0 '('
  ')'  -> mkUMathCode MathClose 0 ')'
  '/'  -> mkUMathCode MathOrd   0 '/'
  '['  -> mkUMathCode MathOpen  0 '['
  ']'  -> mkUMathCode MathClose 0 ']'
  '|'  -> mkUMathCode MathOrd   0 '|'
  '<'  -> mkUMathCode MathRel   0 '<'
  '>'  -> mkUMathCode MathRel   0 '>'
  '\\' -> mkUMathCode MathOrd   0 '\\'
  '{'  -> mkUMathCode MathOpen  0 '{'
  '}'  -> mkUMathCode MathClose 0 '}'
  '-'  -> mkUMathCode MathBin   0 '\x2212'
  '*'  -> mkUMathCode MathBin   0 '\x2217'
  ':'  -> mkUMathCode MathRel   0 '\x2236' -- ?

  _ | isAscii c && isLetter c -> MathCode (fromIntegral (0x7100 + ord c)) -- variable family
    | isDigit c -> MathCode (fromIntegral (0x7000 + ord c)) -- variable family
    | isAscii c -> MathCode (fromIntegral (ord c)) -- ord, family 0 (roman)
    | otherwise -> UMathCode (fromIntegral (ord c)) -- ???

  {-
  where
    operators = 0
    letters = 1
    symbols = 2 -- ?
  -}

mathCodeOf :: (MonadState s m, IsState s) => Char -> m MathCode
mathCodeOf c = do
  m <- use (localState . mathcodeMap)
  pure (Map.findWithDefault (defaultMathCodeOf c) c m)

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

delimiterCodeOf :: (MonadState s m, IsState s) => Char -> m DelimiterCode
delimiterCodeOf c = do
  m <- use (localState . delcodeMap)
  pure (Map.findWithDefault (defaultDelimiterCodeOf c) c m)

enterGroup :: (MonadState s m, IsState s) => ScopeType -> m ()
enterGroup !st = do
  modifying localStates (\ss -> set scopeType st (head ss) : ss)

leaveGroup :: (MonadState s m, IsState s, MonadError String m) => ScopeType -> m ()
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
        beginning ScopeByMath = "`$' or `$$'"
        ending ScopeByBrace = "right brace `}'"
        ending ScopeByBeginGroup = "\\endgroup"
        ending ScopeByLeftRight = "\\right" -- or \middle
        ending GlobalScope = "<end of input>"
        ending ScopeByMath = "`$' or `$$'"
