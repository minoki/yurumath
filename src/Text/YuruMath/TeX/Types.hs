{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Text.YuruMath.TeX.Types where
import Data.Int
import Data.Word
import Data.Text (Text)
import Control.Monad.State.Class
import Control.Monad.Error.Class
import qualified Data.Map as Map
import Control.Lens.TH
import Control.Lens.Lens (Lens',lens)

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

data TeXToken = TTControlSeq !Text
              | TTCharacter !Char !CatCode
              deriving (Eq,Show)

data CommandName = NControlSeq !Text
                 | NActiveChar !Char
                 deriving (Eq,Show)

data SpacingState = SSNewLine
                  | SSSkipSpaces
                  | SSMiddleOfLine
                  deriving (Eq,Show)

data MathStyle = DisplayStyle             -- \displaystyle, 0
               | CrampedDisplayStyle      -- \crampeddisplaystyle, 1
               | TextStyle                -- \textstyle, 2
               | CrampedTextStyle         -- \crampedtextstyle, 3
               | ScriptStyle              -- \scriptstyle, 4
               | CrampedScriptStyle       -- \crampedscriptstyle, 5
               | ScriptScriptStyle        -- \scriptscriptstyle, 6
               | CrampedScriptScriptStyle -- \crampedscriptscriptstyle, 7
               deriving (Eq,Show,Enum,Bounded)

data MathClass = MathOrd   -- \mathord,   ordinary object    (0)
               | MathOp    -- \mathop,    large operator     (1)
               | MathBin   -- \mathbin,   binary operation   (2)
               | MathRel   -- \mathrel,   relation           (3)
               | MathOpen  -- \mathopen,  opening symbol     (4)
               | MathClose -- \mathclose, closing symbol     (5)
               | MathPunct -- \mathpunct, punctuation symbol (6)
               -- variable family? (7)
               | MathInner -- \mathinner, inner formula
               deriving (Eq,Show,Enum,Bounded)

data ParamSpec = Undelimited
               | DelimitedBy [TeXToken]
               | DelimitedByBrace

-- xparse
data DocumentCommandParamSpec = StandardMandatory
                              | DelimitedByLBrace
                              | DelimitedBy2 !Char !Char
                              | DelimitedByWithDefault
                              | Until [TeXToken]
                              | Verbatim
                              | StandardOptional
                              | OptionalDelimitedBy !Char !Char
                              | StandardOptionalWithDefault [TeXToken]
                              | OptionalDelimitedByWithDefault !Char !Char [TeXToken]
                              | OptionalStar -- s
                              | OptionalChar !Char -- t
                              | OptionalGroup -- g
                              | OptionalGroupWithDefault [TeXToken] -- G

type MathFamily = Word8

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

data ExpansionToken = ExpansionToken { etNoexpand :: !Bool -- True if prefixed by \noexpand
                                     , etToken :: !TeXToken
                                     }
                      deriving (Eq,Show)
-- ETCommand Bool CommandName | ETChar Char CatCode

data ExpandableValue = Eelse -- \else
                     | Efi -- \fi
                     | Eor -- \or
                     | Eendcsname -- \endcsname
                     deriving (Eq)

data ExpandableCommand a = MkExpandableCommand (forall m. (MonadState (TeXState a) m, MonadError String m) => m [ExpansionToken])
                         | BooleanConditionalCommand (forall m. (MonadState (TeXState a) m, MonadError String m) => m Bool)
                         | IfCase -- \ifcase
--Macro {-isLong-} !Bool [ParamSpec] [TeXToken]

data Expandable a = ExpandableCommand !(ExpandableCommand a)
                  | ExpandableValue !ExpandableValue

data Value a = Character !Char !CatCode -- character with category code
             | DefinedCharacter !Char -- defined with \chardef
             | DefinedMathCharacter !MathCode -- defined with \mathchardef or \Umathchardef
             | IntegerConstant !Int
             | Relax
             | Unexpanded !CommandName -- prefixed with \noexpand
             | Undefined !CommandName
             | ExtraValue a
             deriving (Eq,Show)

data Mode = HorizontalMode
          | RestrictedHorizontalMode
          | VerticalMode
          | InternalVerticalMode
          | MathMode
          | DisplayMathMode
          deriving (Eq,Show)

isHMode, isVMode, isMMode, isInnerMode :: Mode -> Bool
isHMode m = m == HorizontalMode || m == RestrictedHorizontalMode
isVMode m = m == VerticalMode || m == InternalVerticalMode
isMMode m = m == MathMode || m == DisplayMathMode
isInnerMode m = m == RestrictedHorizontalMode || m == InternalVerticalMode || m == MathMode

data LocalState a = LocalState
                    { _tsDefinitions :: Map.Map Text (Either (Expandable a) (Value a)) -- definitions of control sequences
                    , _tsActiveDefinitions :: Map.Map Char (Either (Expandable a) (Value a)) -- definitions of active characters
                    , _catcodeMap  :: Map.Map Char CatCode
                    , _lccodeMap   :: Map.Map Char Char
                    , _uccodeMap   :: Map.Map Char Char
                    , _mathcodeMap :: Map.Map Char MathCode
                    , _delcodeMap  :: Map.Map Char DelimiterCode
                    -- sfcodeMap   :: Map.Map Char Int
                    , _mathStyle :: !MathStyle
                    -- make extensible?
                    }

data ConditionalKind = CondTruthy
                     | CondFalsy
                     | CondCase

data TeXState a = TeXState
                  { _ttInput :: String
                  -- currentfile, currentline, currentcolumn
                  , _ttSpacingState :: !SpacingState
                  , _esMaxDepth :: !Int
                  , _esMaxPendingToken :: !Int
                  , _esPendingTokenList :: [(Int,ExpansionToken)]
                  , _localStates :: [LocalState a]
                  , _mode :: !Mode
                  , _conditionals :: [ConditionalKind]
                  }

type MonadTeXState a m = MonadState (TeXState a) m

instance Show ExpandableValue where
  show Eelse = "\\else"
  show Efi = "\\fi"
  show Eendcsname = "\\endcsname"

makeLenses ''LocalState
makeLenses ''TeXState

definitionAt :: CommandName -> Lens' (LocalState a) (Either (Expandable a) (Value a))
definitionAt cn@(NControlSeq name) = tsDefinitions . lens getter setter
  where getter = Map.findWithDefault (Right (Undefined cn)) name
        setter s v = Map.insert name v s
definitionAt cn@(NActiveChar c) = tsActiveDefinitions . lens getter setter
  where getter = Map.findWithDefault (Right (Undefined cn)) c
        setter s v = Map.insert c v s
