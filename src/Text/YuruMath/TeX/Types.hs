{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Text.YuruMath.TeX.Types where
import Data.Word
import Data.Text (Text)
import Control.Monad.State.Class
import Control.Monad.Error.Class
import qualified Data.Map as Map
import Control.Lens.TH

data CatCode = CCEscape -- 0
             | CCBeginGroup -- 1
             | CCEndGroup -- 2
             | CCMathShift -- 3
             | CCAlignmentTab -- 4
             | CCEndLine -- 5
             | CCParam -- 6
             | CCSup -- 7
             | CCSub -- 8
             | CCIgnored -- 9
             | CCSpace -- 10
             | CCLetter -- 11
             | CCOther -- 12
             | CCActive -- 13
             | CCComment -- 14
             | CCInvalid -- 15
             deriving (Eq,Show,Enum,Bounded)

data TeXToken = TTControlSeq !Text
              | TTCharacter !Char !CatCode
--              | TTParameter !Int
              deriving (Eq,Show)

data CommandName = NControlSeq !Text
                 | NActiveChar !Char
                 deriving (Eq,Show)

data SpacingState = SSNewLine
                  | SSSkipSpaces
                  | SSMiddleOfLine
                  deriving (Eq,Show)

data MathStyle = MathDisplayStyle
               | MathDisplayStyleCramped
               | MathTextStyle
               | MathTextStyleCramped
               | MathScriptStyle
               | MathScriptStyleCramped
               | MathScriptScriptStyle
               | MathScriptScriptStyleCramped
               deriving (Eq,Show,Enum,Bounded)

data MathClass = MathOrd   -- \mathord, ordinary object (1)
               | MathOp    -- \mathop, large operator (2)
               | MathBin   -- \mathbin, binary operation (3)
               | MathRel   -- \mathrel, relation (4)
               | MathOpen  -- \mathopen, opening symbol (5)
               | MathClose -- \mathclose, closing symbol (6)
               | MathPunct -- \mathpunct, punctuation symbol (7)
               -- variable family? (8)
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

data MathChar = MathChar !MathClass !MathFamily !Char
              | MathActive
              deriving (Eq,Show)
-- "xyzz (15-bit number)

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
             | DefinedMathCharacter !MathChar -- Word16? -- defined with \mathchardef
             --  IntegerConstant !Integer
             | Relax
             | Unexpanded !CommandName -- prefixed with \noexpand
             | Undefined !CommandName
             | ExtraValue a
             deriving (Eq,Show)

data LocalState a = LocalState
                    { _ttCategoryCodeOf :: Map.Map Char CatCode -- Char -> CatCode
                    , _tsDefinitions :: Map.Map Text (Either (Expandable a) (Value a)) -- definitions of control sequences
                    , _tsActiveDefinitions :: Map.Map Char (Either (Expandable a) (Value a)) -- definitions of active characters
                    , _mathCodes :: Map.Map Char MathChar
                    -- delcode, sfcode, lccode, upcode
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
                  , _isMathMode :: !Bool
                  , _conditionals :: [ConditionalKind]
                  }

type MonadTeXState a m = MonadState (TeXState a) m

instance Show ExpandableValue where
  show Eelse = "\\else"
  show Efi = "\\fi"
  show Eendcsname = "\\endcsname"

makeLenses ''LocalState
makeLenses ''TeXState
