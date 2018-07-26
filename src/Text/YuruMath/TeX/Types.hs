{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
module Text.YuruMath.TeX.Types where
import Data.Int
import Data.Word
import Data.Bits
import Data.Char
import Data.Text (Text)
import Control.Monad.State.Class
import Control.Monad.Error.Class
import qualified Data.Map.Strict as Map
import Control.Lens.Lens (Lens',lens)
import Control.Lens.At (at)
import Data.OpenUnion
import Data.OpenUnion.Internal ((@!>))
import TypeFun.Data.List (Delete,Elem)
import Data.Typeable (Typeable)
import Text.YuruMath.TeX.Quantity

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
-- TODO: 'frozen' control sequences?

data TeXToken = TTCommandName !CommandName
              | TTCharacter !Char !CatCode -- not CCEscape, CCEndLine, CCIgnored, CCActive, CCComment, CCInvalid
              deriving (Eq,Show)

data ExpansionCommandNameFlavor = ECNFPlain
                                | ECNFIsRelax  -- inserted \relax by \else, \fi
                                | ECNFNoexpand -- prefixed by \noexpand
                                deriving (Eq,Show)

data ExpansionToken = ETCommandName
                      { etDepth :: !Int
                      , etFlavor :: !ExpansionCommandNameFlavor
                      , etName :: !CommandName
                      }
                    | ETCharacter -- non-active character
                      { etDepth :: !Int
                      , etChar :: !Char
                      , etCatCode :: !CatCode
                      }
                      deriving (Show)

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
mathcharClass (UMathCode x) = toEnum $ fromIntegral $ 7 .&. (int32ToWord32 x `shiftR` 21)

mathcharFamily :: MathCode -> Word8
mathcharFamily (MathCode x) = fromIntegral $ 0xF .&. (x `shiftR` 8)
mathcharFamily (UMathCode x) = fromIntegral $ 0xFF .&. (int32ToWord32 x `shiftR` 24)

mathcharSlot :: MathCode -> Char
mathcharSlot (MathCode x) = toEnum $ fromIntegral $ 0xFF .&. x
mathcharSlot (UMathCode x) = toEnum $ fromIntegral $ 0x1FFFFF .&. int32ToWord32 x

mathActive :: MathCode
mathActive = MathCode 0x8000

data DelimiterCode = DelimiterCode !Int32 -- "uvvxyy (24-bit number), where uvv: the small variant, xyy: the large variant
                   | UDelimiterCode !Int32
                   deriving (Eq,Show)

mkUDelCode :: Word8 -> Char -> DelimiterCode
mkUDelCode fam code = UDelimiterCode
                      $ (fromIntegral fam `shiftL` 21)
                      .|. fromIntegral (ord code)

delimiterFamilySmall :: DelimiterCode -> Word8
delimiterFamilySmall (DelimiterCode x) = fromIntegral (0xF .&. (x `shiftR` 20))
delimiterFamilySmall (UDelimiterCode x) = fromIntegral (int32ToWord32 x `shiftR` 24)

delimiterSlotSmall :: DelimiterCode -> Char
delimiterSlotSmall (DelimiterCode x) = chr $ fromIntegral (0xFF .&. (x `shiftR` 12))
delimiterSlotSmall (UDelimiterCode x) = chr $ fromIntegral (0x1FFFFF .&. int32ToWord32 x)

delimiterFamilyLarge :: DelimiterCode -> Word8
delimiterFamilyLarge (DelimiterCode x) = fromIntegral (0xF .&. (x `shiftR` 8))
delimiterFamilyLarge (UDelimiterCode x) = fromIntegral (int32ToWord32 x `shiftR` 24)

delimiterSlotLarge :: DelimiterCode -> Char
delimiterSlotLarge (DelimiterCode x) = chr $ fromIntegral (0xFF .&. x)
delimiterSlotLarge (UDelimiterCode x) = chr $ fromIntegral (0x1FFFFF .&. int32ToWord32 x)

data LimitsSpec = Limits
                | NoLimits
                | DisplayLimits
                deriving (Eq,Show)

data ConditionalMarker = Eelse -- \else
                       | Efi -- \fi
                       | Eor -- \or
                       deriving (Eq,Enum,Bounded)

class (Eq e) => IsExpandable e where
  isConditional       :: e -> Bool -- Used by \if.., \or, \else to skip conditionals
  isConditionalMarker :: e -> Maybe ConditionalMarker -- Used by \if.., \or, \else to skip conditionals

  -- default definitions:
  isConditionalMarker _ = Nothing

instance IsExpandable (Union '[]) where
  isConditional       = typesExhausted
  isConditionalMarker = typesExhausted

instance (IsExpandable e, IsExpandable (Union (Delete e es)), Typeable e) => IsExpandable (Union (e : es)) where
  isConditional       = (isConditional :: e -> Bool)
                        @> (isConditional :: Union (Delete e es) -> Bool)
  isConditionalMarker = (isConditionalMarker :: e -> Maybe ConditionalMarker)
                        @> (isConditionalMarker :: Union (Delete e es) -> Maybe ConditionalMarker)

data CommonValue = Character !Char !CatCode       -- character with category code
                 | DefinedCharacter !Char         -- defined with \chardef
                 | DefinedMathCharacter !MathCode -- defined with \mathchardef or \Umathchardef
                 | IntegerConstant !Int32
                 | Relax                          -- \relax
                 | Endcsname                      -- \endcsname
                 deriving (Show)

instance Eq CommonValue where
  Character c cc         == Character c' cc'        = c == c' && cc == cc'
  DefinedCharacter c     == DefinedCharacter c'     = c == c'
  DefinedMathCharacter c == DefinedMathCharacter c' = c == c'
  IntegerConstant x      == IntegerConstant x'      = x == x'
  Relax                  == Relax                   = True
  Endcsname              == Endcsname               = True
  _                      == _                       = False

class Eq value => IsNValue value where
  injectCommonValue :: CommonValue -> value
  toCommonValue :: value -> Maybe CommonValue

instance IsNValue CommonValue where
  injectCommonValue = id
  toCommonValue = Just

instance (Eq (Union s), Elem CommonValue s) => IsNValue (Union s) where
  injectCommonValue = liftUnion
  toCommonValue = Just @!> \_ -> Nothing

-- explicit space or implicit space
isImplicitSpace :: (IsNValue v) => v -> Bool
isImplicitSpace v = case toCommonValue v of
  Just (Character _ CCSpace) -> True
  _ -> False

class IsPrimitive a where
  primitiveName :: a -> Text
  primitivesOfThisType :: [a]
  default primitivesOfThisType :: (Bounded a,Enum a) => [a]
  primitivesOfThisType = [minBound..maxBound]

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

data InputState = InputState
                  { _inputTokenizerState :: !TokenizerState
                  , _inputPendingTokenList :: [ExpansionToken]
                  }

inputTokenizerState :: Lens' InputState TokenizerState
inputTokenizerState = lens _inputTokenizerState (\s v -> s { _inputTokenizerState = v })

inputPendingTokenList :: Lens' InputState [ExpansionToken]
inputPendingTokenList = lens _inputPendingTokenList (\s v -> s { _inputPendingTokenList = v })

data ScopeType = ScopeByBrace      -- { .. }
               | ScopeByBeginGroup -- \begingroup .. \endgroup
               | GlobalScope
               | ScopeByLeftRight  -- \left .. \right
               | ScopeByMath       -- $ .. $ or $$ .. $$
               -- ScopeByEnvironment !Text -- \begin{xxx} .. \end{xxx}
               deriving (Eq,Show)

data CommonLocalState ecommand value
  = CommonLocalState
    { _scopeType     :: !ScopeType
    , _controlSeqDef :: !(Map.Map Text (Either (Union ecommand) (Union value))) -- definitions of control sequences
    , _activeDef     :: !(Map.Map Char (Either (Union ecommand) (Union value))) -- definitions of active characters
    , _catcodeMap    :: !(Map.Map Char CatCode)
    , _lccodeMap     :: !(Map.Map Char Char)
    , _uccodeMap     :: !(Map.Map Char Char)
    , _mathcodeMap   :: !(Map.Map Char MathCode)
    , _delcodeMap    :: !(Map.Map Char DelimiterCode)
    -- sfcodeMap     :: !(Map.Map Char Int)
    , _endlinechar   :: !Int
    , _escapechar    :: !Int
    , _countReg      :: !(Map.Map Int Integer)
    , _dimenReg      :: !(Map.Map Int Dimen)
    , _skipReg       :: !(Map.Map Int (Glue Dimen))
    , _muskipReg     :: !(Map.Map Int (Glue MuDimen))
    , _toksReg       :: !(Map.Map Int [TeXToken])
    -- TODO: box registers
    , _thinmuskip    :: !(Glue MuDimen)
    , _medmuskip     :: !(Glue MuDimen)
    , _thickmuskip   :: !(Glue MuDimen)
    -- TODO: Use extensible records?
    }

data ConditionalKind = CondTruthy
                     | CondFalsy
                     | CondCase
                     | CondTest

data CommonState localstate
  = CommonState
    { _inputStateStack    :: ![InputState] -- must be non-empty
    , _esMaxDepth         :: !Int
    , _esMaxPendingToken  :: !Int
    , _conditionals       :: [ConditionalKind]
    , _nameInProgress     :: !Bool
    , _localStates        :: [localstate] -- must be non-empty
    , _mode               :: !Mode
    }

class (IsExpandable (Union (ExpandableSetT localstate)), IsNValue (Union (NValueSetT localstate)), Elem CommonValue (NValueSetT localstate)) => IsLocalState localstate where
  type ExpandableSetT localstate :: [*]
  type NValueSetT localstate :: [*] -- the set of non-expandable values
  commonLocalState :: Lens' localstate (CommonLocalState (ExpandableSetT localstate) (NValueSetT localstate))

type ExpandableT localstate = Union (ExpandableSetT localstate)
type NValueT localstate = Union (NValueSetT localstate) -- the type of non-expandable values
type DefinedValueT localstate = Either (ExpandableT localstate) (NValueT localstate)
type ValueT localstate = Maybe (DefinedValueT localstate)

expandableToValue :: (Elem a e, Typeable a) => a -> Maybe (Either (Union e) (Union n))
expandableToValue x = Just (Left (liftUnion x))
nonexpandableToValue :: (Elem a n, Typeable a) => a -> Maybe (Either (Union e) (Union n))
nonexpandableToValue x = Just (Right (liftUnion x))

definitionAt  :: (IsLocalState localstate) => CommandName -> Lens' localstate (ValueT localstate)
scopeType     :: (IsLocalState localstate) => Lens' localstate ScopeType
controlSeqDef :: (IsLocalState localstate) => Lens' localstate (Map.Map Text (DefinedValueT localstate))
activeDef     :: (IsLocalState localstate) => Lens' localstate (Map.Map Char (DefinedValueT localstate))
catcodeMap    :: (IsLocalState localstate) => Lens' localstate (Map.Map Char CatCode)
lccodeMap, uccodeMap :: (IsLocalState localstate) => Lens' localstate (Map.Map Char Char)
mathcodeMap   :: (IsLocalState localstate) => Lens' localstate (Map.Map Char MathCode)
delcodeMap    :: (IsLocalState localstate) => Lens' localstate (Map.Map Char DelimiterCode)
endlinechar, escapechar :: (IsLocalState localstate) => Lens' localstate Int
countReg      :: (IsLocalState localstate) => Lens' localstate (Map.Map Int Integer)
dimenReg      :: (IsLocalState localstate) => Lens' localstate (Map.Map Int Dimen)
skipReg       :: (IsLocalState localstate) => Lens' localstate (Map.Map Int (Glue Dimen))
muskipReg     :: (IsLocalState localstate) => Lens' localstate (Map.Map Int (Glue MuDimen))
toksReg       :: (IsLocalState localstate) => Lens' localstate (Map.Map Int [TeXToken])
thinmuskip, medmuskip, thickmuskip :: IsLocalState localstate => Lens' localstate (Glue MuDimen)

scopeType     = commonLocalState . lens _scopeType   (\s v -> s { _scopeType = v})
controlSeqDef = commonLocalState . lens _controlSeqDef (\s v -> s { _controlSeqDef = v })
activeDef     = commonLocalState . lens _activeDef   (\s v -> s { _activeDef = v })
catcodeMap    = commonLocalState . lens _catcodeMap  (\s v -> s { _catcodeMap = v })
lccodeMap     = commonLocalState . lens _lccodeMap   (\s v -> s { _lccodeMap = v })
uccodeMap     = commonLocalState . lens _uccodeMap   (\s v -> s { _uccodeMap = v })
mathcodeMap   = commonLocalState . lens _mathcodeMap (\s v -> s { _mathcodeMap = v })
delcodeMap    = commonLocalState . lens _delcodeMap  (\s v -> s { _delcodeMap = v })
endlinechar   = commonLocalState . lens _endlinechar (\s v -> s { _endlinechar = v })
escapechar    = commonLocalState . lens _escapechar  (\s v -> s { _escapechar = v })
countReg      = commonLocalState . lens _countReg    (\s v -> s { _countReg = v })
dimenReg      = commonLocalState . lens _dimenReg    (\s v -> s { _dimenReg = v })
skipReg       = commonLocalState . lens _skipReg     (\s v -> s { _skipReg = v })
muskipReg     = commonLocalState . lens _muskipReg   (\s v -> s { _muskipReg = v })
toksReg       = commonLocalState . lens _toksReg     (\s v -> s { _toksReg = v })
thinmuskip    = commonLocalState . lens _thinmuskip  (\s v -> s { _thinmuskip = v })
medmuskip     = commonLocalState . lens _medmuskip   (\s v -> s { _medmuskip = v })
thickmuskip   = commonLocalState . lens _thickmuskip (\s v -> s { _thickmuskip = v })

definitionAt (NControlSeq name) = controlSeqDef . at name
definitionAt (NActiveChar c) = activeDef . at c

class (IsLocalState (LocalState state)) => IsState state where
  type LocalState state
  commonState        :: Lens' state (CommonState (LocalState state))

-- tokenizer
inputStateStack    :: (IsState state) => Lens' state [InputState]
inputStateStack    = commonState . lens _inputStateStack (\s v -> s { _inputStateStack = v })
inputState         :: (IsState state) => Lens' state InputState
inputState         = inputStateStack . lens head setter
  where setter [] _ = error "Invalid input state stack"
        setter (_:xs) x = x:xs
tokenizerState     :: (IsState state) => Lens' state TokenizerState
tokenizerState     = inputState . inputTokenizerState

-- expansion processor
esMaxDepth         :: (IsState state) => Lens' state Int -- read-only?
esMaxPendingToken  :: (IsState state) => Lens' state Int -- read-only?
esPendingTokenList :: (IsState state) => Lens' state [ExpansionToken]
conditionals       :: (IsState state) => Lens' state [ConditionalKind]
nameInProgress     :: (IsState state) => Lens' state Bool
esMaxDepth         = commonState . lens _esMaxDepth         (\s v -> s { _esMaxDepth = v })
esMaxPendingToken  = commonState . lens _esMaxPendingToken  (\s v -> s { _esMaxPendingToken = v })
esPendingTokenList = commonState . inputState . inputPendingTokenList
conditionals       = commonState . lens _conditionals       (\s v -> s { _conditionals = v })
nameInProgress     = commonState . lens _nameInProgress     (\s v -> s { _nameInProgress = v })

-- others
localStates        :: (IsState state) => Lens' state [LocalState state]
localState         :: (IsState state) => Lens' state (LocalState state)
mode               :: (IsState state) => Lens' state Mode
localStates        = commonState . lens _localStates        (\s v -> s { _localStates = v })
mode               = commonState . lens _mode               (\s v -> s { _mode = v })
localState = localStates . lens head setter
  where setter [] _ = error "Invalid local state"
        setter (_:xs) x = x:xs

class (IsExpandable e, Monad m) => DoExpand e m where
  doExpand               :: e -> ExpansionToken -> m [ExpansionToken] -- normal expansion
  doExpandInEdef         :: e -> Maybe (ExpansionToken -> m [ExpansionToken]) -- expansion in the context of \edef, including \message, \expanded, etc
  evalBooleanConditional :: e -> Maybe (m Bool) -- used by \unless
  doExpandInEdef _ = Nothing

instance (Monad m) => DoExpand (Union '[]) m where
  doExpand               = typesExhausted
  doExpandInEdef         = typesExhausted
  evalBooleanConditional = typesExhausted

instance (DoExpand e m, DoExpand (Union (Delete e es)) m, Typeable e) => DoExpand (Union (e : es)) m where
  doExpand               = (doExpand :: e -> ExpansionToken -> m [ExpansionToken])
                           @> (doExpand :: Union (Delete e es) -> ExpansionToken -> m [ExpansionToken])
  doExpandInEdef         = (doExpandInEdef :: e -> Maybe (ExpansionToken -> m [ExpansionToken]))
                           @> (doExpandInEdef :: Union (Delete e es) -> Maybe (ExpansionToken -> m [ExpansionToken]))
  evalBooleanConditional = (evalBooleanConditional :: e -> Maybe (m Bool))
                           @> (evalBooleanConditional :: Union (Delete e es) -> Maybe (m Bool))

data QuantityGetter f
  = QInteger (f Integer)       -- <internal integer>
  | QDimension (f Dimen)       -- <internal dimen>
  | QGlue (f (Glue Dimen))     -- <internal glue>
  | QMuGlue (f (Glue MuDimen)) -- <internal muglue>
  -- Note: there is no <internal mudimen>
  | QToks (f [TeXToken])
  | NotQuantity

data Arithmetic m = Arithmetic { arithmeticLocalAdvance   :: m ()
                               , arithmeticGlobalAdvance  :: m ()
                               , arithmeticLocalMultiply  :: m ()
                               , arithmeticGlobalMultiply :: m ()
                               , arithmeticLocalDivide    :: m ()
                               , arithmeticGlobalDivide   :: m ()
                               }

class (Eq c, Monad m) => DoExecute c m where
  doExecute    :: c -> m () -- normal execution
  doGlobal     :: c -> Maybe (m ()) -- prefixed by \global
  doArithmetic :: c -> Maybe (m (Arithmetic m)) -- prefixed by \advance, \multiply, or \divide
  getQuantity  :: c -> QuantityGetter m -- get <internal integer/dimen/glue/muglue>, or prefixed by \the
  doGlobal _ = Nothing
  doArithmetic _ = Nothing

instance (Monad m) => DoExecute (Union '[]) m where
  doExecute    = typesExhausted
  doGlobal     = typesExhausted
  doArithmetic = typesExhausted
  getQuantity  = typesExhausted

instance (DoExecute c m, DoExecute (Union (Delete c cs)) m, Typeable c) => DoExecute (Union (c : cs)) m where
  doExecute    = (doExecute :: c -> m ())
                 @> (doExecute :: Union (Delete c cs) -> m ())
  doGlobal     = (doGlobal :: c -> Maybe (m ()))
                 @> (doGlobal :: Union (Delete c cs) -> Maybe (m ()))
  doArithmetic = (doArithmetic :: c -> Maybe (m (Arithmetic m)))
                 @> (doArithmetic :: Union (Delete c cs) -> Maybe (m (Arithmetic m)))
  getQuantity  = (getQuantity :: c -> QuantityGetter m)
                 @> (getQuantity :: Union (Delete c cs) -> QuantityGetter m)

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute CommonValue m where
  doExecute (Character _ _)          = return () -- dummy
  doExecute (DefinedCharacter _)     = return () -- dummy
  doExecute (DefinedMathCharacter _) = return () -- dummy
  doExecute (IntegerConstant _)      = throwError $ "Unexpected integer constant."
  doExecute Relax                    = return () -- do nothing
  doExecute Endcsname                = throwError "Extra \\endcsname"
  getQuantity (DefinedCharacter x) = QInteger (return $ fromIntegral $ ord x)
  getQuantity (DefinedMathCharacter m) = case m of
    MathCode x -> QInteger (return $ fromIntegral x)
    UMathCode x -> QInteger (return $ fromIntegral x)
  getQuantity (IntegerConstant x) = QInteger (return $ fromIntegral x)
  getQuantity _ = NotQuantity

type ExpandableSet s = ExpandableSetT (LocalState s)
type NValueSet s     = NValueSetT (LocalState s)
type Expandable s    = ExpandableT (LocalState s)
type NValue s        = NValueT (LocalState s)
type Value s         = ValueT (LocalState s)

type MonadTeXState s m = (IsState s, MonadState s m, DoExpand (Expandable s) m, DoExecute (NValue s) m)

instance Show ConditionalMarker where
  show Eelse = "\\else"
  show Efi = "\\fi"
  show Eor = "\\or"

instance (IsExpandable (Union ecommand), IsNValue (Union value), Elem CommonValue value) => IsLocalState (CommonLocalState ecommand value) where
  type ExpandableSetT (CommonLocalState ecommand value) = ecommand
  type NValueSetT (CommonLocalState ecommand value) = value
  commonLocalState = id

instance IsLocalState localstate => IsState (CommonState localstate) where
  type LocalState (CommonState localstate) = localstate
  commonState        = id

-- TODO: Better place?
isUnicodeScalarValue :: (Integral a) => a -> Bool
isUnicodeScalarValue x = 0 <= x && x <= 0x10FFFF && not (0xD800 <= x && x <= 0xDFFF)

-- Integral with or without bounds
class (Integral a) => IntegralB a where
  maybeFromInteger :: Integer -> Maybe a
  default maybeFromInteger :: (Bounded a) => Integer -> Maybe a
  maybeFromInteger x
    | fromIntegral (minBound :: a) <= x && x <= fromIntegral (maxBound :: a) = Just (fromIntegral x)
    | otherwise = Nothing

instance IntegralB Integer where maybeFromInteger = Just
instance IntegralB Int
instance IntegralB Int32
instance IntegralB Word
instance IntegralB Word32

int32ToWord32 :: Int32 -> Word32
int32ToWord32 = fromIntegral

word32ToInt32 :: Word32 -> Int32
word32ToInt32 = fromIntegral
