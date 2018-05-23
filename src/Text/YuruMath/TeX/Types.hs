{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.YuruMath.TeX.Types where
import Data.Int
import Data.Word
import Data.Text (Text)
import Control.Monad.State.Class
import Control.Monad.Error.Class
import qualified Data.Map as Map
import Control.Lens.Lens (Lens',lens)
import Data.OpenUnion
import Data.OpenUnion.Internal ((@!>))
import TypeFun.Data.List (Delete,Elem)
import Data.Typeable (Typeable)

data CatCode = CCEscape       -- 0
             | CCBeginGroup   -- 1
             | CCEndGroup     -- 2
             | CCMathShift    -- 3
             | CCAlignmentTab -- 4
             | CCEndLine      -- 5
             | CCParam        -- 6
             | CCSup          -- 7
             | CCSub          -- 8
             | CCIgnored      -- 9
             | CCSpace        -- 10
             | CCLetter       -- 11
             | CCOther        -- 12
             | CCActive       -- 13
             | CCComment      -- 14
             | CCInvalid      -- 15
             deriving (Eq,Show,Enum,Bounded)

data CommandName = NControlSeq !Text
                 | NActiveChar !Char
                 deriving (Eq,Show)

data TeXToken = TTCommandName !CommandName
              | TTCharacter !Char !CatCode -- not CCEscape, CCEndLine, CCIgnored, CCActive, CCComment, CCInvalid
              deriving (Eq,Show)

data ExpansionToken = ETCommandName {-noexpand-} !Bool !CommandName
                    | ETCharacter !Char !CatCode -- non-active character
                      deriving (Eq,Show)

-- Better name?
data ParamLong = ShortParam -- \par is not allowed
               | LongParam  -- \par is allowed
               deriving (Eq,Show)

data MathClass = MathOrd   -- \mathord,   ordinary object    (0)
               | MathOp    -- \mathop,    large operator     (1)
               | MathBin   -- \mathbin,   binary operation   (2)
               | MathRel   -- \mathrel,   relation           (3)
               | MathOpen  -- \mathopen,  opening symbol     (4)
               | MathClose -- \mathclose, closing symbol     (5)
               | MathPunct -- \mathpunct, punctuation symbol (6)
               | MathVar   --             variable family    (7)
               | MathInner -- \mathinner, inner formula
               deriving (Eq,Show,Enum,Bounded)

data MathCode = MathCode !Word16 -- "xyzz (15-bit number) or "8000 (math active)
              | UMathCode !Int32 -- 8 bits for the math family, 3 bits for the math class, 21 bits for the character code
              deriving (Eq,Show)

data DelimiterCode = DelimiterCode !Int32 -- "uvvxyy (24-bit number), where uvv: the small variant, xyy: the large variant
                   | UDelimiterCode !Int32
                   deriving (Eq,Show)

data LimitsSpec = Limits
                | NoLimits
                | DisplayLimits
                deriving (Eq,Show)

data ConditionalMarker = Eelse -- \else
                       | Efi -- \fi
                       | Eor -- \or
                       deriving (Eq)

class (Eq e) => IsExpandable e where
  isConditional       :: e -> Bool
  isIfCase            :: e -> Bool
  isConditionalMarker :: e -> Maybe ConditionalMarker

  -- default definitions:
  isIfCase _            = False
  isConditionalMarker _ = Nothing

instance IsExpandable (Union '[]) where
  isConditional       = typesExhausted
  isIfCase            = typesExhausted
  isConditionalMarker = typesExhausted

instance (IsExpandable e, IsExpandable (Union (Delete e es)), Typeable e) => IsExpandable (Union (e : es)) where
  isConditional       = (isConditional :: e -> Bool)
                        @> (isConditional :: Union (Delete e es) -> Bool)
  isIfCase            = (isIfCase :: e -> Bool)
                        @> (isIfCase :: Union (Delete e es) -> Bool)
  isConditionalMarker = (isConditionalMarker :: e -> Maybe ConditionalMarker)
                        @> (isConditionalMarker :: Union (Delete e es) -> Maybe ConditionalMarker)

data CommonValue = Character !Char !CatCode       -- character with category code
                 | DefinedCharacter !Char         -- defined with \chardef
                 | DefinedMathCharacter !MathCode -- defined with \mathchardef or \Umathchardef
                 | IntegerConstant !Int
                 | Relax                          -- \relax
                 | Unexpanded !CommandName        -- prefixed with \noexpand
                 | Undefined !CommandName
                 | Endcsname                      -- \endcsname
                 deriving (Show)

instance Eq CommonValue where
  Character c cc         == Character c' cc'        = c == c' && cc == cc'
  DefinedCharacter c     == DefinedCharacter c'     = c == c'
  DefinedMathCharacter c == DefinedMathCharacter c' = c == c'
  IntegerConstant x      == IntegerConstant x'      = x == x'
  Relax                  == Relax                   = True
  Unexpanded _           == Unexpanded _            = True
  Undefined _            == Undefined _             = True
  Endcsname              == Endcsname               = True
  _                      == _                       = False

class Eq value => IsValue value where
  injectCommonValue :: CommonValue -> value
  toCommonValue :: value -> Maybe CommonValue

instance IsValue CommonValue where
  injectCommonValue = id
  toCommonValue = Just

instance (Eq (Union s), Elem CommonValue s) => IsValue (Union s) where
  injectCommonValue = liftUnion
  toCommonValue = Just @!> \_ -> Nothing

data Mode = HorizontalMode
          | RestrictedHorizontalMode
          | VerticalMode
          | InternalVerticalMode
          | MathMode
          | DisplayMathMode
          deriving (Eq,Show)

isHMode, isVMode, isMMode, isInnerMode :: Mode -> Bool
isHMode m     = m == HorizontalMode || m == RestrictedHorizontalMode
isVMode m     = m == VerticalMode   || m == InternalVerticalMode
isMMode m     = m == MathMode       || m == DisplayMathMode
isInnerMode m = m == RestrictedHorizontalMode || m == InternalVerticalMode || m == MathMode

data SpacingState = SSNewLine
                  | SSSkipSpaces
                  | SSMiddleOfLine
                  deriving (Eq,Show)

data TokenizerState = TokenizerState
                      { tsInput :: String
                      , tsSpacingState :: !SpacingState
                      -- not implemented yet:
                      -- , tsCurrentLine :: !Int
                      -- , tsCurrentColumn :: !Int
                      }

data ScopeType = ScopeByBrace      -- { .. }
               | ScopeByBeginGroup -- \begingroup .. \endgroup
               | GlobalScope
               | ScopeByLeftRight  -- \left .. \right
               -- ScopeByEnvironment !Text -- \begin{xxx} .. \end{xxx}
               deriving (Eq,Show)

data CommonLocalState ecommand value
  = CommonLocalState
    { _scopeType           :: !ScopeType
    , _tsDefinitions       :: Map.Map Text (Either ecommand value) -- definitions of control sequences
    , _tsActiveDefinitions :: Map.Map Char (Either ecommand value) -- definitions of active characters
    , _catcodeMap          :: Map.Map Char CatCode
    , _lccodeMap           :: Map.Map Char Char
    , _uccodeMap           :: Map.Map Char Char
    , _mathcodeMap         :: Map.Map Char MathCode
    , _delcodeMap          :: Map.Map Char DelimiterCode
    -- sfcodeMap           :: Map.Map Char Int
    , _endlinechar         :: !Int
    , _escapechar          :: !Int
    }

data ConditionalKind = CondTruthy
                     | CondFalsy
                     | CondCase
                     | CondTest

data CommonState localstate
  = CommonState
    { _tokenizerState     :: !TokenizerState
    , _esMaxDepth         :: !Int
    , _esMaxPendingToken  :: !Int
    , _esPendingTokenList :: [(Int,ExpansionToken)]
    , _localStates        :: [localstate] -- must be non-empty
    , _mode               :: !Mode
    , _conditionals       :: [ConditionalKind]
    }

class (IsExpandable (ExpandableT localstate), IsValue (ValueT localstate)) => IsLocalState localstate where
  type ExpandableT localstate
  type ValueT localstate
  scopeType           :: Lens' localstate ScopeType
  tsDefinitions       :: Lens' localstate (Map.Map Text (Either (ExpandableT localstate) (ValueT localstate)))
  tsActiveDefinitions :: Lens' localstate (Map.Map Char (Either (ExpandableT localstate) (ValueT localstate)))
  catcodeMap          :: Lens' localstate (Map.Map Char CatCode)
  lccodeMap           :: Lens' localstate (Map.Map Char Char)
  uccodeMap           :: Lens' localstate (Map.Map Char Char)
  mathcodeMap         :: Lens' localstate (Map.Map Char MathCode)
  delcodeMap          :: Lens' localstate (Map.Map Char DelimiterCode)
  endlinechar         :: Lens' localstate Int
  escapechar          :: Lens' localstate Int

-- state -> localstate
class (IsLocalState (LocalState state)) => IsState state where
  type LocalState state

  -- tokenizer
  tokenizerState     :: Lens' state TokenizerState

  -- expansion processor
  esMaxDepth         :: Lens' state Int -- read-only?
  esMaxPendingToken  :: Lens' state Int -- read-only?
  esPendingTokenList :: Lens' state [(Int,ExpansionToken)]

  -- others
  localStates        :: Lens' state [LocalState state]
  mode               :: Lens' state Mode
  conditionals       :: Lens' state [ConditionalKind]

class (IsExpandable e, Monad m) => DoExpand e m where
  doExpand               :: e -> m [ExpansionToken]
  evalBooleanConditional :: e -> Maybe (m Bool)

instance (Monad m) => DoExpand (Union '[]) m where
  doExpand               = typesExhausted
  evalBooleanConditional = typesExhausted

instance (DoExpand e m, DoExpand (Union (Delete e es)) m, Typeable e) => DoExpand (Union (e : es)) m where
  doExpand               = (doExpand :: e -> m [ExpansionToken])
                           @> (doExpand :: Union (Delete e es) -> m [ExpansionToken])
  evalBooleanConditional = (evalBooleanConditional :: e -> Maybe (m Bool))
                           @> (evalBooleanConditional :: Union (Delete e es) -> Maybe (m Bool))

class (Eq c, Monad m) => DoExecute c m where
  doExecute :: c -> m ()
  getIntegerValue :: c -> Maybe (m Integer)

instance (Monad m) => DoExecute (Union '[]) m where
  doExecute = typesExhausted
  getIntegerValue = typesExhausted

instance (DoExecute c m, DoExecute (Union (Delete c cs)) m, Typeable c) => DoExecute (Union (c : cs)) m where
  doExecute       = (doExecute :: c -> m ())
                    @> (doExecute :: Union (Delete c cs) -> m ())
  getIntegerValue = (getIntegerValue :: c -> Maybe (m Integer))
                    @> (getIntegerValue :: Union (Delete c cs) -> Maybe (m Integer))

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute CommonValue m where
  doExecute (Character _ _)          = return () -- dummy
  doExecute (DefinedCharacter _)     = return () -- dummy
  doExecute (DefinedMathCharacter _) = return () -- dummy
  doExecute (IntegerConstant _)      = throwError $ "Unexpected integer constant."
  doExecute Relax                    = return () -- do nothing
  doExecute (Unexpanded _)           = return () -- do nothing
  doExecute (Undefined _)            = throwError $ "Undefined control sequence."
  doExecute Endcsname                = throwError "Extra \\endcsname"
  getIntegerValue _ = Nothing -- dummy

type Expandable s = ExpandableT (LocalState s)
type Value s = ValueT (LocalState s)

type MonadTeXState s m = (IsState s, MonadState s m, DoExpand (Expandable s) m, DoExecute (Value s) m)

instance Show ConditionalMarker where
  show Eelse = "\\else"
  show Efi = "\\fi"
  show Eor = "\\or"

instance (IsExpandable ecommand, IsValue value) => IsLocalState (CommonLocalState ecommand value) where
  type ExpandableT (CommonLocalState ecommand value) = ecommand
  type ValueT (CommonLocalState ecommand value) = value
  scopeType           = lens _scopeType           (\s v -> s { _scopeType = v})
  tsDefinitions       = lens _tsDefinitions       (\s v -> s { _tsDefinitions = v })
  tsActiveDefinitions = lens _tsActiveDefinitions (\s v -> s { _tsActiveDefinitions = v })
  catcodeMap          = lens _catcodeMap          (\s v -> s { _catcodeMap = v })
  lccodeMap           = lens _lccodeMap           (\s v -> s { _lccodeMap = v })
  uccodeMap           = lens _uccodeMap           (\s v -> s { _uccodeMap = v })
  mathcodeMap         = lens _mathcodeMap         (\s v -> s { _mathcodeMap = v })
  delcodeMap          = lens _delcodeMap          (\s v -> s { _delcodeMap = v })
  endlinechar         = lens _endlinechar         (\s v -> s { _endlinechar = v })
  escapechar          = lens _escapechar          (\s v -> s { _escapechar = v })

instance IsLocalState localstate => IsState (CommonState localstate) where
  type LocalState (CommonState localstate) = localstate
  tokenizerState     = lens _tokenizerState     (\s v -> s { _tokenizerState = v })
  esMaxDepth         = lens _esMaxDepth         (\s v -> s { _esMaxDepth = v })
  esMaxPendingToken  = lens _esMaxPendingToken  (\s v -> s { _esMaxPendingToken = v })
  esPendingTokenList = lens _esPendingTokenList (\s v -> s { _esPendingTokenList = v })
  localStates        = lens _localStates        (\s v -> s { _localStates = v })
  mode               = lens _mode               (\s v -> s { _mode = v })
  conditionals       = lens _conditionals       (\s v -> s { _conditionals = v })

definitionAt :: IsLocalState localstate => CommandName -> Lens' localstate (Either (ExpandableT localstate) (ValueT localstate))
definitionAt cn@(NControlSeq name) = tsDefinitions . lens getter setter
  where getter = Map.findWithDefault (Right (injectCommonValue $ Undefined cn)) name
        setter s v = Map.insert name v s
definitionAt cn@(NActiveChar c) = tsActiveDefinitions . lens getter setter
  where getter = Map.findWithDefault (Right (injectCommonValue $ Undefined cn)) c
        setter s v = Map.insert c v s

localState :: IsState s => Lens' s (LocalState s)
localState = localStates . lens head setter
  where setter [] x = error "Invalid local state"
        setter (_:xs) x = x:xs

-- TODO: Better place?
isUnicodeScalarValue :: (Integral a) => a -> Bool
isUnicodeScalarValue x = 0 <= x && x <= 0x10FFFF && not (0xD800 <= x && x <= 0xDFFF)
