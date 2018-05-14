{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Text.YuruMath.TeX.Math where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Tokenizer
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Expansion
import Text.YuruMath.TeX.Execution
import qualified Data.Map.Strict as Map
import Data.Word
import Data.Bits
import Data.Text (Text)
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Identity
import Control.Lens.Getter (use,uses)
import Control.Lens.Setter (assign,modifying)
import Control.Lens.TH
import Data.OpenUnion
import TypeFun.Data.List (Elem,Substract,SubList,Delete,(:++:))

-- sqrt, overline
makeCramped :: MathStyle -> MathStyle
makeCramped DisplayStyle      = CrampedDisplayStyle
makeCramped TextStyle         = CrampedTextStyle
makeCramped ScriptStyle       = CrampedScriptStyle
makeCramped ScriptScriptStyle = CrampedScriptScriptStyle
makeCramped crampedstyle = crampedstyle

-- superscript
superscriptStyle :: MathStyle -> MathStyle
superscriptStyle        DisplayStyle =        ScriptStyle
superscriptStyle CrampedDisplayStyle = CrampedScriptStyle
superscriptStyle        TextStyle    =        ScriptStyle
superscriptStyle CrampedTextStyle    = CrampedScriptStyle
superscriptStyle        ScriptStyle  =        ScriptScriptStyle
superscriptStyle CrampedScriptStyle  = CrampedScriptScriptStyle
superscriptStyle scriptscriptstyle = scriptscriptstyle

-- subscript
subscriptStyle :: MathStyle -> MathStyle
subscriptStyle = makeCramped . superscriptStyle

-- numerator
smallerStyle :: MathStyle -> MathStyle
smallerStyle        DisplayStyle =        TextStyle
smallerStyle CrampedDisplayStyle = CrampedTextStyle
smallerStyle        TextStyle    =        ScriptStyle
smallerStyle CrampedTextStyle    = CrampedScriptStyle
smallerStyle        ScriptStyle  =        ScriptScriptStyle
smallerStyle CrampedScriptStyle  = CrampedScriptScriptStyle
smallerStyle scriptscriptstyle = scriptscriptstyle

{-
-- denominator
denominatorStyle :: MathStyle -> MathStyle
denominatorStyle = makeCramped . smallerStyle
-}

data AtomType = AOrd   -- ordinary
              | AOp    -- large operator
              | ABin   -- binary operation
              | ARel   -- relation
              | AOpen  -- opening
              | AClose -- closing
              | APunct -- punctuation
              | AInner -- inner
              | AOver  -- overline
              | AUnder -- underline
              | AAcc   -- accented
              | ARad   -- radical
              | AVcent -- vcenter
              deriving (Eq,Show)

mathclassToAtomType :: MathClass -> AtomType
mathclassToAtomType MathOrd   = AOrd
mathclassToAtomType MathOp    = AOp
mathclassToAtomType MathBin   = ABin
mathclassToAtomType MathRel   = ARel
mathclassToAtomType MathOpen  = AOpen
mathclassToAtomType MathClose = AClose
mathclassToAtomType MathPunct = APunct
mathclassToAtomType MathVar   = AOrd
mathclassToAtomType MathInner = AInner

data MathField = MFEmpty
               | MFSymbol {-family number-} !Word {-position number-} !Char
               | MFBox
               | MFSubList MathList
               deriving (Eq,Show)

data Atom = OrdAtom   { atomNucleus     :: !MathField
                      , atomSuperscript :: !MathField
                      , atomSubscript   :: !MathField
                      }
          | OpAtom    { atomNucleus     :: !MathField
                      , atomSuperscript :: !MathField
                      , atomSubscript   :: !MathField
                      , atomLimits      :: !LimitsSpec -- specific to Op atom
                      }
          | BinAtom   { atomNucleus     :: !MathField
                      , atomSuperscript :: !MathField
                      , atomSubscript   :: !MathField
                      }
          | RelAtom   { atomNucleus     :: !MathField
                      , atomSuperscript :: !MathField
                      , atomSubscript   :: !MathField
                      }
          | OpenAtom  { atomNucleus     :: !MathField
                      , atomSuperscript :: !MathField
                      , atomSubscript   :: !MathField
                      , atomIsDelimiter :: !Bool      -- specific to Open and Close atom
                      }
          | CloseAtom { atomNucleus     :: !MathField
                      , atomSuperscript :: !MathField
                      , atomSubscript   :: !MathField
                      , atomIsDelimiter :: !Bool      -- specific to Open and Close atom
                      }
          | PunctAtom { atomNucleus     :: !MathField
                      , atomSuperscript :: !MathField
                      , atomSubscript   :: !MathField
                      }
          | InnerAtom { atomNucleus     :: !MathField
                      , atomSuperscript :: !MathField
                      , atomSubscript   :: !MathField
                      }
          | OverAtom  { atomNucleus     :: !MathField
                      , atomSuperscript :: !MathField
                      , atomSubscript   :: !MathField
                      }
          | UnderAtom { atomNucleus     :: !MathField
                      , atomSuperscript :: !MathField
                      , atomSubscript   :: !MathField
                      }
          | AccAtom   { atomNucleus     :: !MathField
                      , atomSuperscript :: !MathField
                      , atomSubscript   :: !MathField
                      , atomAccentCharacter :: !MathCode -- specific to Acc atom
                      }
          | RadAtom   { atomNucleus     :: !MathField
                      , atomSuperscript :: !MathField
                      , atomSubscript   :: !MathField
                      , atomDelimiter   :: !DelimiterCode -- specific to Rad atom
                      }
          | VcentAtom { atomNucleus     :: !MathField
                      , atomSuperscript :: !MathField
                      , atomSubscript   :: !MathField
                      }
            deriving (Eq,Show)

atomType :: Atom -> AtomType
atomType (OrdAtom   {}) = AOrd
atomType (OpAtom    {}) = AOp
atomType (BinAtom   {}) = ABin
atomType (RelAtom   {}) = ARel
atomType (OpenAtom  {}) = AOpen
atomType (CloseAtom {}) = AClose
atomType (PunctAtom {}) = APunct
atomType (InnerAtom {}) = AInner
atomType (OverAtom  {}) = AOver
atomType (UnderAtom {}) = AUnder
atomType (AccAtom   {}) = AAcc
atomType (RadAtom   {}) = ARad
atomType (VcentAtom {}) = AVcent

emptyAtom :: Atom
emptyAtom = OrdAtom { atomNucleus     = MFEmpty
                    , atomSuperscript = MFEmpty
                    , atomSubscript   = MFEmpty
                    }

mkAtom :: AtomType -> MathField -> Atom
mkAtom AOrd   !nucleus = OrdAtom   { atomNucleus     = nucleus
                                   , atomSuperscript = MFEmpty
                                   , atomSubscript   = MFEmpty
                                   }
mkAtom AOp    !nucleus = OpAtom    { atomNucleus     = nucleus
                                   , atomSuperscript = MFEmpty
                                   , atomSubscript   = MFEmpty
                                   , atomLimits      = DisplayLimits -- specific to Op taom
                                   }
mkAtom ABin   !nucleus = BinAtom   { atomNucleus     = nucleus
                                   , atomSuperscript = MFEmpty
                                   , atomSubscript   = MFEmpty
                                   }
mkAtom ARel   !nucleus = RelAtom   { atomNucleus     = nucleus
                                   , atomSuperscript = MFEmpty
                                   , atomSubscript   = MFEmpty
                                   }
mkAtom AOpen  !nucleus = OpenAtom  { atomNucleus     = nucleus
                                   , atomSuperscript = MFEmpty
                                   , atomSubscript   = MFEmpty
                                   , atomIsDelimiter = False   -- specific to Open and Close atom
                                   }
mkAtom AClose !nucleus = CloseAtom { atomNucleus     = nucleus
                                   , atomSuperscript = MFEmpty
                                   , atomSubscript   = MFEmpty
                                   , atomIsDelimiter = False   -- specific to Open and Close atom
                                   }
mkAtom APunct !nucleus = PunctAtom { atomNucleus     = nucleus
                                   , atomSuperscript = MFEmpty
                                   , atomSubscript   = MFEmpty
                                   }
mkAtom AInner !nucleus = InnerAtom { atomNucleus     = nucleus
                                   , atomSuperscript = MFEmpty
                                   , atomSubscript   = MFEmpty
                                   }
mkAtom AOver  !nucleus = OverAtom  { atomNucleus     = nucleus
                                   , atomSuperscript = MFEmpty
                                   , atomSubscript   = MFEmpty
                                   }
mkAtom AUnder !nucleus = UnderAtom { atomNucleus     = nucleus
                                   , atomSuperscript = MFEmpty
                                   , atomSubscript   = MFEmpty
                                   }
mkAtom AAcc   !nucleus = AccAtom   { atomNucleus     = nucleus
                                   , atomSuperscript = MFEmpty
                                   , atomSubscript   = MFEmpty
                                   , atomAccentCharacter = MathCode 0 -- specific to Acc atom
                                   }
mkAtom ARad   !nucleus = RadAtom   { atomNucleus     = nucleus
                                   , atomSuperscript = MFEmpty
                                   , atomSubscript   = MFEmpty
                                   , atomDelimiter   = DelimiterCode (-1) -- specific to Rad atom
                                   }
mkAtom AVcent !nucleus = VcentAtom { atomNucleus     = nucleus
                                   , atomSuperscript = MFEmpty
                                   , atomSubscript   = MFEmpty
                                   }

markAtomAsDelimiter :: Atom -> Atom
markAtomAsDelimiter atom@(OpenAtom {}) = atom { atomIsDelimiter = True }
markAtomAsDelimiter atom@(CloseAtom {}) = atom { atomIsDelimiter = True }
markAtomAsDelimiter atom = atom

-- generalized fraction
data GenFrac = GFOver
             | GFAtop
             | GFAbove {-dimen-}
             | GFSkewed !DelimiterCode {- options: noaxis, exact -}
             | GFOverWithDelims !DelimiterCode !DelimiterCode
             | GFAtopWithDelims !DelimiterCode !DelimiterCode
             | GFAboveWithDelims !DelimiterCode !DelimiterCode {-dimen-} {- option: exact -}
             | GFSkewedWithDelims !DelimiterCode !DelimiterCode !DelimiterCode {- options: noaxis, exact -}
             deriving (Eq,Show)

data BoundaryType = BoundaryLeft
                  | BoundaryRight
                  | BoundaryMiddle
                  deriving (Eq,Show)

data MathItem = IAtom !Atom
              | IHorizontalMaterial
              | IVerticalMaterial -- \mark or \insert or \vadjust
              | IGlue -- \hskip or \mskip or \nonscript
              | IKern -- \kern or \mkern
              | IStyleChange !MathStyle -- \displaystyle, \textstyle, etc
              | IGenFrac !GenFrac MathList MathList -- \above, \over, etc
              | IBoundary !BoundaryType !DelimiterCode -- \left, \middle, or \right
              | IChoice MathList MathList MathList MathList -- \mathchoice
              deriving (Eq,Show)

type MathList = [MathItem] -- Use Data.Sequence?

data MathState localstate
  = MathState
    { _commonState :: !(CommonState localstate)
    , _currentMathStyle :: !MathStyle
    }

initialMathState :: Bool -> CommonState localstate -> MathState localstate
initialMathState !isDisplay !commonState
  = MathState { _commonState = commonState { _mode = if isDisplay then DisplayMathMode else MathMode }
              , _currentMathStyle = if isDisplay then DisplayStyle else TextStyle
              }

makeLenses ''MathState

data MathToken m where
  MTChar       :: !Char -> MathToken m -- letter, other, \char or \chardef-ed
  MTMathChar   :: !MathCode -> MathToken m -- \mathchar or \mathchardef-ed
  MTDelimiter  :: !MathClass -> !DelimiterCode -> MathToken m -- \delimiter
  MTLBrace     :: MathToken m
  MTRBrace     :: MathToken m
  MTAtomSpec   :: !AtomType -> MathToken m -- \mathord, ..., \overline
  MTSup        :: MathToken m
  MTSub        :: MathToken m
  MTRadical    :: !DelimiterCode -> MathToken m
  MTRoot       :: !DelimiterCode -> MathToken m
  MTAccent     :: !MathCode -> MathToken m
  MTLimitsSpec :: !LimitsSpec -> MathToken m
  MTSetStyle   :: !MathStyle -> MathToken m -- \displaystyle, \textstyle, etc
  MTLeft       :: !DelimiterCode -> MathToken m
  MTMiddle     :: !DelimiterCode -> MathToken m
  MTRight      :: !DelimiterCode -> MathToken m
  MTGenFrac    :: !GenFrac -> MathToken m -- \over, \atop, \above<dimen>, ..withdelims<delim><delim>
  MTUstack     :: MathToken m -- \Ustack
  MTChoice     :: MathToken m -- \mathchoice
  MTOther      :: (DoExecute v m, Show v) => v -> MathToken m

deriving instance Show (MathToken m)

instance (IsLocalState localstate) => IsState (MathState localstate) where
  type LocalState (MathState localstate) = localstate
  ttInput            = commonState . ttInput
  ttSpacingState     = commonState . ttSpacingState
  esMaxDepth         = commonState . esMaxDepth
  esMaxPendingToken  = commonState . esMaxPendingToken
  esPendingTokenList = commonState . esPendingTokenList
  localStates        = commonState . localStates
  mode               = commonState . mode
  conditionals       = commonState . conditionals

{-
<character> ::= <letter> | <otherchar> | \char<8-bit number> | <chardef token>
<math character> ::= \mathchar<15-bit number> | <mathchardef token> | \delimiter<27-bit number>
<math symbol> ::= <character> | <math character>
<math field> ::= <math symbol> | <filler>{<math mode material>}
<delim> ::= <filler>\delimiter<27-bit number> | <filler><letter> | <filler><otherchar>
-}

type MathValueList = '[CommonValue,MathStyleSet,MathAtomCommand,MathCommands]

type MonadMathState localstate set m
  = (MonadTeXState (MathState localstate) m
    ,ValueT localstate ~ Union (MathValueList :++: set)
    ,Show (Union set)
    ,DoExecute (Union set) m
    ,Delete MathCommands (Delete MathAtomCommand (Delete MathStyleSet (Delete CommonValue set))) ~ set -- ugly hack
    )

-- constraint on set: SubList MathValue
readMathToken :: forall m localstate set. (MonadMathState localstate set m, MonadError String m) => m (Maybe (MathToken m))
readMathToken = do
  v <- evalToValue
  case v of
    Nothing -> return Nothing -- end of input
    Just v -> doMathToken v
  where
    doMathToken :: Union (MathValueList :++: set) -> m (Maybe (MathToken m))
    doMathToken = doCommonValue
                  @> ((Just <$>) . doMathStyleSet :: MathStyleSet -> m (Maybe (MathToken m)))
                  @> ((Just <$>) . doMathAtom :: MathAtomCommand -> m (Maybe (MathToken m)))
                  @> ((Just <$>) . doOtherMathCommand :: MathCommands -> m (Maybe (MathToken m)))
                  @> ((\v -> return $ Just $ MTOther v) :: Union set -> m (Maybe (MathToken m)) ) -- other assignments, etc
    doCommonValue :: CommonValue -> m (Maybe (MathToken m))
    doCommonValue v = case v of
      Character c CCBeginGroup   -> return $ Just MTLBrace
      Character c CCEndGroup     -> return $ Just MTRBrace
      Character c CCMathShift    -> throwError "math shift is not supported"
      Character c CCAlignmentTab -> throwError "alignment tab: not implemented yet"
      Character c CCSup          -> return $ Just MTSup
      Character c CCSub          -> return $ Just MTSub
      Character c CCSpace        -> readMathToken -- do nothing
      Character c CCLetter       -> return $ Just $ MTChar c -- TODO: process 'math active'
      Character c CCOther        -> return $ Just $ MTChar c -- TODO: process 'math active'
      Character _ cc             -> throwError $ "Unexpected " ++ show cc ++ " character" -- endline, param, ignored, active, comment, invalid
      DefinedCharacter c         -> return $ Just $ MTChar c
      DefinedMathCharacter c     -> return $ Just $ MTMathChar c
      v -> doExecute v >> readMathToken -- \relax, etc
    doMathStyleSet (MathStyleSet style) = return $ MTSetStyle style
    doMathAtom (MathAtomCommand atomType) = return $ MTAtomSpec atomType
    doOtherMathCommand :: MathCommands -> m (MathToken m)
    doOtherMathCommand v = case v of

      -- \char<0-"10FFFF>
      Mchar -> MTChar <$> readUnicodeScalarValue

      -- \mathchar<15-bit integer>
      Mmathchar -> do
        (MTMathChar . MathCode . fromIntegral) <$> readIntBetween 0 0x7FFF

      -- \Umathchar<0-7><0-"FF><0-"10FFFF>
      MUmathchar -> do
        mathclass <- readIntBetween 0 7
        fam <- readIntBetween 0 0xFF
        c <- readUnicodeScalarValue
        return $ MTMathChar (mkUMathCode (toEnum mathclass) (fromIntegral fam) c)

      -- \Umathcharnum<signed 32-bit integer>
      MUmathcharnum -> do
        x <- readInt32
        let slot = 0x1FFFFF .&. (fromIntegral x :: Word32)
        if isUnicodeScalarValue slot
          then return $ MTMathChar (UMathCode x)
          else throwError "\\Umathcharnum: Invalid math code"

      -- \delimiter<27-bit integer>
      Mdelimiter -> do
        x <- readIntBetween 0 (2^(27::Int)-1)
        let mathclass = toEnum (x `shiftR` 24) :: MathClass
            value = fromIntegral (0xFFFFFF .&. x)
        return $ MTDelimiter mathclass (DelimiterCode value)

      -- \Udelimiter<0-7><0-"FF"><0-"10FFFF>
      MUdelimiter -> do
        mathclass <- toEnum <$> readIntBetween 0 7
        fam <- fromIntegral <$> readIntBetween 0 0xFF
        slot <- readUnicodeScalarValue
        return $ MTDelimiter mathclass $ mkUDelCode fam slot

      -- \radical<24-bit integer><math field>
      Mradical -> do
        x <- readIntBetween 0 (2^(24::Int)-1)
        -- x = <12-bit: small variant><12-bit: large variant>
        return $ MTRadical $ DelimiterCode $ fromIntegral x

      -- \Uradical<0-"FF><0-"10FFFF><math field>
      MUradical -> do
        fam <- readIntBetween 0 0xFF
        slot <- readUnicodeScalarValue
        return $ MTRadical $ mkUDelCode (fromIntegral fam) slot

      -- \Uroot<0-"FF><0-"10FFFF><math field><math field>
      MUroot -> do
        fam <- readIntBetween 0 0xFF
        slot <- readUnicodeScalarValue
        return $ MTRoot $ mkUDelCode (fromIntegral fam) slot

      -- \mathaccent<15-bit integer><math field>
      Mmathaccent -> do
        x <- readIntBetween 0 0x7FFF
        let value = fromIntegral $ x .&. 0x0FFF
        return $ MTAccent $ MathCode value

      -- \Umathaccent<0-7><0-"FF"><0-"10FFFF><math field>
      MUmathaccent -> do
        -- TODO: handle optional keywords
        mathclass <- readIntBetween 0 7
        fam <- readIntBetween 0 0xFF
        slot <- readUnicodeScalarValue
        return $ MTAccent $ mkUMathCode (toEnum mathclass) (fromIntegral fam) slot

      MUsuperscript -> return MTSup
      MUsubscript -> return MTSub

      -- \left<delim>, \right<delim>, \middle<delim>
      Mleft   -> MTLeft   <$> readDelimiter
      Mright  -> MTRight  <$> readDelimiter
      Mmiddle -> MTMiddle <$> readDelimiter

      -- \over, \atop, \above<dimen>, \Uskewed<delim>, \overwithdelims<delim><delim>, \atopwithdelims<delim><delim>, \abovewithdelims<delim><delim><dimen>, \Uskewedwithdelims<delim><delim><delim>
      Mover    -> return $ MTGenFrac GFOver
      Matop    -> return $ MTGenFrac GFAtop
      Mabove   -> throwError "\\above: not implemented yet"
      MUskewed -> (MTGenFrac . GFSkewed) <$> readDelimiter -- TODO: Handle keywords
      Moverwithdelims    -> MTGenFrac <$> (GFOverWithDelims <$> readDelimiter <*> readDelimiter)
      Matopwithdelims    -> MTGenFrac <$> (GFAtopWithDelims <$> readDelimiter <*> readDelimiter)
      Mabovewithdelims   -> throwError "\\abovewithdelims: not implemented yet"
      MUskewedwithdelims -> MTGenFrac <$> (GFSkewedWithDelims <$> readDelimiter <*> readDelimiter <*> readDelimiter) -- TODO: Handle keywords

      Mlimits        -> return $ MTLimitsSpec Limits
      Mnolimits      -> return $ MTLimitsSpec NoLimits
      Mdisplaylimits -> return $ MTLimitsSpec DisplayLimits

      -- \mathchoice<general text><general text><general text><general text>
      Mmathchoice -> return MTChoice

      _ -> throwError $ show v ++ ": not implemented yet"

readDelimiter :: (MonadMathState localstate set m, MonadError String m) => m DelimiterCode
readDelimiter = do
  t <- readMathToken
  case t of
    Nothing -> throwError "Missing delimiter: got end of input"
    Just (MTChar c) -> do
      delimiterCodeOf c
    Just (MTDelimiter _ code) -> do
      return code
    Just t -> throwError $ "Missing delimiter: got " ++ show t

withMathStyle :: (MonadTeXState (MathState localstate) m) => (MathStyle -> MathStyle) -> m a -> m a
withMathStyle f m = do
  oldStyle <- use currentMathStyle
  assign currentMathStyle (f oldStyle)
  x <- m
  assign currentMathStyle oldStyle
  return x

class (Functor f) => MathMaterialEnding f where
  onEndOfInput :: MonadError String m => m MathList -> m (f MathList)
  onRightBrace :: MonadError String m => m MathList -> m (f MathList)
  onMiddleDelim :: MonadError String m => DelimiterCode -> m MathList -> m (f MathList)
  onRightDelim :: MonadError String m => DelimiterCode -> m MathList -> m (f MathList)

newtype MMDGlobal a = MMDGlobal { runMMDGlobal :: a } deriving (Functor)
instance MathMaterialEnding MMDGlobal where
  onEndOfInput m = MMDGlobal <$> m
  onRightBrace _ = throwError "Unexpected `}': expected end of input"
  onMiddleDelim _ _ = throwError "Unexpected \\middle: expected end of input"
  onRightDelim _ _ = throwError "Unexpected \\right: expected end of input"

newtype MMDBrace a = MMDBrace { runMMDBrace :: a } deriving (Functor)
instance MathMaterialEnding MMDBrace where
  onEndOfInput _ = throwError "Unexpected end of input: expected `}'"
  onRightBrace m = MMDBrace <$> m
  onMiddleDelim _ _ = throwError "Unexpected \\middle: expected `}'"
  onRightDelim _ _ = throwError "Unexpected \\right: expected `}'"

data MMDLeftRight a = MMDRight !DelimiterCode a
                    | MMDMiddle !DelimiterCode a
                    deriving (Functor)
instance MathMaterialEnding MMDLeftRight where
  onEndOfInput _ = throwError "Unexpected end of input: expected \\right"
  onRightBrace _ = throwError "Unexpected `}': expected \\right"
  onMiddleDelim delim m = MMDMiddle delim <$> m
  onRightDelim delim m = MMDRight delim <$> m

data FractionPosition = NotInFraction
                      | FractionNumerator -- after \Ustack
                      | FractionDenominator -- after \over-like commands
                      deriving (Eq)

newtype MathMaterialContext = MathMaterialContext { mmcFractionPosition :: FractionPosition
                                                  }

defaultMathMaterialContext :: MathMaterialContext
defaultMathMaterialContext = MathMaterialContext { mmcFractionPosition = NotInFraction
                                                 }

readMathMaterial :: (MathMaterialEnding f, MonadMathState localstate set m, MonadError String m) => MathMaterialContext -> m (f MathList)
readMathMaterial !ctx = loop []
  where
    loop revList = do
      t <- readMathToken
      let doAtom atom = loop (IAtom atom : revList)
          modifyLastAtom f = case revList of
            IAtom atom : revList' -> do
              atom' <- f atom
              loop (IAtom atom' : revList')
            _ -> do
              atom <- f emptyAtom
              loop (IAtom atom : revList)
      case t of
        Nothing -> onEndOfInput (return (reverse revList))
        Just t -> case t of
          -- <character>
          MTChar c -> do
            mc <- mathCodeOf c
            let atomType = mathclassToAtomType $ mathcharClass mc
                fam = fromIntegral $ mathcharFamily mc
                slot = mathcharSlot mc
                atom = mkAtom atomType (MFSymbol fam slot)
            delcode <- delimiterCodeOf c
            doAtom $ if delcode /= DelimiterCode (-1)
                     then markAtomAsDelimiter atom
                     else atom

          -- <math symbol>
          MTMathChar mc -> do -- \mathchar or \mathchardef-ed
            let atomType = mathclassToAtomType $ mathcharClass mc
                fam = fromIntegral $ mathcharFamily mc
                slot = mathcharSlot mc
            doAtom (mkAtom atomType (MFSymbol fam slot))

          -- <math symbol>
          MTDelimiter mathclass del -> do -- \delimiter
            let atomType = mathclassToAtomType mathclass
                fam = fromIntegral $ delimiterFamilySmall del
                slot = delimiterSlotSmall del
            doAtom (markAtomAsDelimiter (mkAtom atomType (MFSymbol fam slot)))

          -- { <math mode material> }
          MTLBrace -> do
            oldStyle <- use currentMathStyle
            enterGroup ScopeByBrace
            content <- runMMDBrace <$> readMathMaterial (ctx { mmcFractionPosition = NotInFraction }) -- MMDBrace
            assign currentMathStyle oldStyle
            case content of
              [item@(IAtom (AccAtom {}))] -> loop (item : revList) -- single Acc atom
              _ -> doAtom (mkAtom AOrd (MFSubList content))

          -- `}'
          MTRBrace -> onRightBrace $ do
            leaveGroup ScopeByBrace
            return (reverse revList)

          -- <math atom><math field>
          MTAtomSpec atomType -> do
            x <- if atomType == AOver
                 then withMathStyle makeCramped readMathField
                 else readMathField
            doAtom (mkAtom atomType x)

          -- <superscript><math field>
          MTSup -> do
            x <- withMathStyle superscriptStyle readMathField
            modifyLastAtom $ \atom ->
              if atomSuperscript atom == MFEmpty
              then return (atom { atomSuperscript = x })
              else throwError "Double superscript"

          -- <subscript><math field>
          MTSub -> do
            x <- withMathStyle subscriptStyle readMathField
            modifyLastAtom $ \atom ->
              if atomSubscript atom == MFEmpty
              then return (atom { atomSubscript = x })
              else throwError "Double subscript"

          -- \mathaccent<15-bit number><math field> or \Umathaccent<...><math field>
          MTAccent code -> do
            content <- withMathStyle makeCramped readMathField
            doAtom (mkAtom AAcc content)

          -- \radical<27-bit number><math field> or \Uradical<...><math field>
          MTRadical code -> do
            content <- withMathStyle makeCramped readMathField
            doAtom (mkAtom ARad content)

          -- \Uroot<...><math field><math field>
          MTRoot code -> do
            degree <- withMathStyle (const ScriptScriptStyle) readMathField
            content <- withMathStyle makeCramped readMathField
            doAtom (mkAtom ARad content)

          -- \limits, \nolimits, \displaylimits
          MTLimitsSpec spec -> do
            case revList of
              IAtom (atom@(OpAtom {})) : xs -> loop (IAtom (atom { atomLimits = spec }) : xs)
              _ -> throwError "Limit controls must follow a math operator."

          -- \displaystyle, \textstyle, etc
          MTSetStyle newStyle -> do
            -- Remove this check to allow \mathchoice and \mathstyle to be different
            if mmcFractionPosition ctx == FractionNumerator
              then throwError "Numerator of a fraction must be surrounded by { and }"
              else do assign currentMathStyle newStyle
                      loop (IStyleChange newStyle : revList)

          -- \left<delim> <math mode material> (\middle<delim><math mode material>)* \right<delim>
          MTLeft leftDelim -> do
            let readUntilRight revContentList = do
                  enterGroup ScopeByLeftRight
                  result <- readMathMaterial (ctx { mmcFractionPosition = NotInFraction }) -- MMDLeftRight
                  case result of
                    MMDMiddle delim content -> do
                      readUntilRight ([IBoundary BoundaryMiddle delim] : content : revContentList)
                    MMDRight delim content -> do
                      let content' = concat $ reverse $ [IBoundary BoundaryRight delim] : content : revContentList
                      loop (IAtom (mkAtom AInner (MFSubList content')) : revList)
            readUntilRight [[IBoundary BoundaryLeft leftDelim]]

          -- \middle<delim>
          MTMiddle delim -> onMiddleDelim delim $ do
            leaveGroup ScopeByLeftRight
            return (reverse revList)

          -- \right<delim>
          MTRight delim -> onRightDelim delim $ do
            leaveGroup ScopeByLeftRight
            return (reverse revList)

          -- <generalized fraction command>
          MTGenFrac gf
            | mmcFractionPosition ctx == FractionDenominator ->
                throwError "Ambiguous: you need another { and }"

            -- Remove this check to allow classic \over-like commands
            | mmcFractionPosition ctx == NotInFraction ->
                throwError "Fraction must be preceded by \\Ustack"

            | otherwise -> do
                let numerator = reverse revList
                result <- withMathStyle makeCramped $ readMathMaterial (ctx { mmcFractionPosition = FractionDenominator })
                return ((\denominator -> [IGenFrac gf numerator denominator]) <$> result)

          -- \Ustack { <math mode material> <generalized fraction command> <math mode material> }
          MTUstack -> do
            readLBrace
            content <- runMMDBrace <$> withMathStyle smallerStyle (readMathMaterial (ctx { mmcFractionPosition = FractionNumerator }))
            case content of
              [item@(IGenFrac _ _ _)] -> loop (item : revList)
              _ -> throwError "No fraction after \\Ustack"

          -- \mathchoice<general text><general text><general text><general text>
          MTChoice -> do
            -- TODO: Set \mathstyle to 'cramped'?
            readLBrace
            d  <- runMMDBrace <$> withMathStyle (const DisplayStyle) (readMathMaterial defaultMathMaterialContext)
            readLBrace
            t  <- runMMDBrace <$> withMathStyle (const TextStyle) (readMathMaterial defaultMathMaterialContext)
            readLBrace
            s  <- runMMDBrace <$> withMathStyle (const ScriptStyle) (readMathMaterial defaultMathMaterialContext)
            readLBrace
            ss <- runMMDBrace <$> withMathStyle (const ScriptScriptStyle) (readMathMaterial defaultMathMaterialContext)
            loop (IChoice d t s ss : revList)

          -- assignments, etc
          MTOther v -> do
            doExecute v
            loop revList

readLBrace :: (MonadMathState localstate set m, MonadError String m) => m ()
readLBrace = do
  t <- readMathToken
  case t of
    Just MTLBrace -> do
      enterGroup ScopeByBrace
    _ -> throwError "Expected `{'"

-- <math symbol> ::= <character> | <math character>
-- <math field> ::= <math symbol> | {<math mode material>}
readMathField :: (MonadMathState localstate set m, MonadError String m) => m MathField
readMathField = do
  t <- readMathToken
  case t of
    Nothing -> throwError "Unexpected end of input: expected a math field"
    Just t -> case t of
      MTChar c -> do
        mc <- mathCodeOf c
        let fam = fromIntegral $ mathcharFamily mc
            slot = mathcharSlot mc
        return (MFSymbol fam slot)
      MTMathChar mc -> do -- \mathchar or \mathchardef-ed
        let fam = fromIntegral $ mathcharFamily mc
            slot = mathcharSlot mc
        return (MFSymbol fam slot)
      MTDelimiter mathclass del -> do -- \delimiter
        let fam = fromIntegral $ delimiterFamilySmall del
            slot = delimiterSlotSmall del
        return (MFSymbol fam slot)
      MTLBrace -> do
        enterGroup ScopeByBrace
        content <- runMMDBrace <$> readMathMaterial defaultMathMaterialContext
        return $ case content of
          [IAtom (OrdAtom { atomNucleus = nucleus, atomSuperscript = MFEmpty, atomSubscript = MFEmpty })] -> nucleus
          _ -> MFSubList content
      _ -> throwError $ "Unexpeced " ++ show t ++ "; expected a symbol or `{'"

--
-- Setting math style
--

newtype MathStyleSet = MathStyleSet MathStyle
                     deriving (Eq,Show)

instance (Monad m, MonadTeXState (MathState localstate) m, MonadError String m) => DoExecute MathStyleSet m where
  doExecute (MathStyleSet s) = return () -- dummy
  getIntegerValue (MathStyleSet v) = Just $ return $ fromIntegral $ fromEnum v -- LuaTeX extension

--
-- Math atom command (like \mathord)
--

newtype MathAtomCommand = MathAtomCommand AtomType deriving (Eq,Show)

instance (Monad m, MonadTeXState (MathState localstate) m, MonadError String m) => DoExecute MathAtomCommand m where
  doExecute _ = return () -- dummy
  getIntegerValue _ = Nothing

--
-- Expandable math commands
--

data MathExpandable = Mmathstyle -- LuaTeX extension
                    deriving (Eq,Show)

-- LuaTeX extension: \mathstyle
mathstyleCommand :: (MonadTeXState (MathState localstate) m, MonadError String m) => m [ExpansionToken]
mathstyleCommand = do
  style <- use currentMathStyle
  stringToEToken $ show $ fromEnum style -- 0..7

instance IsExpandable MathExpandable where
  isConditional _ = False

instance (Monad m, MonadTeXState (MathState localstate) m, MonadError String m) => DoExpand MathExpandable m where
  doExpand Mmathstyle = mathstyleCommand
  evalBooleanConditional _ = Nothing

--
-- Other math commands
--

data MathCommands
  = Mchar -- ?
  | Mmathchar
  | Mmathaccent
  | Mdelimiter
  | Mradical
  | Mdisplaylimits
  | Mlimits
  | Mnolimits
  | Mdiscretionaly
  | Mmathchoice
  | Mleft
  | Mright
  | Mover
  | Matop
  | Mabove
  | Moverwithdelims
  | Matopwithdelims
  | Mabovewithdelims

    -- e-TeX extension:
  | Mmiddle

    -- LuaTeX extensions:
  | MUmathchar
  | MUmathaccent
  | MUdelimiter
  | MUradical
  | MUmathcharnum
  | MUroot
  | MUoverdelimiter
  | MUunderdelimiter
  | MUdelimiterover
  | MUdelimiterunder
  | MUhextensible
  | MUskewed
  | MUskewedwithdelims
  | MUstack
  | MUsuperscript
  | MUsubscript
  | MUnosuperscript
  | MUnosubscript
  | MUleft
  | MUmiddle
  | MUright
  -- \Ustartmath, \Ustopmath, \Ustartdisplaymath, \Ustopdisplaymath

  deriving (Eq,Show)

instance (Monad m, MonadTeXState (MathState localstate) m, MonadError String m) => DoExecute MathCommands m where
  doExecute _ = return () -- dummy
  getIntegerValue _ = Nothing

--
-- List of commands
--

mathDefinitionsE :: (Elem MathExpandable set) => Map.Map Text (Union set)
mathDefinitionsE = Map.fromList
  [("mathstyle", liftUnion Mmathstyle) -- LuaTeX extension
  ]

mathDefinitions :: (SubList '[MathCommands,MathAtomCommand,MathStyleSet] set) => Map.Map Text (Union set)
mathDefinitions = Map.fromList
  [("char",         liftUnion Mchar)
  ,("mathchar",     liftUnion Mmathchar)
  ,("delimiter",    liftUnion Mdelimiter)
  ,("mathord",      liftUnion (MathAtomCommand AOrd))
  ,("mathop",       liftUnion (MathAtomCommand AOp))
  ,("mathbin",      liftUnion (MathAtomCommand ABin))
  ,("mathrel",      liftUnion (MathAtomCommand ARel))
  ,("mathopen",     liftUnion (MathAtomCommand AOpen))
  ,("mathclose",    liftUnion (MathAtomCommand AClose))
  ,("mathpunct",    liftUnion (MathAtomCommand APunct))
  ,("mathinner",    liftUnion (MathAtomCommand AInner))
  ,("underline",    liftUnion (MathAtomCommand AUnder))
  ,("overline",     liftUnion (MathAtomCommand AOver))
  ,("mathaccent",   liftUnion Mmathaccent)
  ,("radical",      liftUnion Mradical)
  ,("displaylimits",liftUnion Mdisplaylimits)
  ,("limits",       liftUnion Mlimits)
  ,("nolimits",     liftUnion Mnolimits)
  ,("discretionaly",liftUnion Mdiscretionaly)
  ,("mathchoice",   liftUnion Mmathchoice)
  ,("left",         liftUnion Mleft)
  ,("right",        liftUnion Mright)
  ,("over",         liftUnion Mover)
  ,("atop",         liftUnion Matop)
  ,("above",        liftUnion Matop)
  ,("overwithdelims",liftUnion Mover)
  ,("atopwithdelims",liftUnion Matop)
  ,("abovewithdelims",liftUnion Matop)

  -- e-TeX extension:
  ,("middle",       liftUnion Mmiddle)

  -- LuaTeX extension:
  ,("Umathchar",      liftUnion MUmathchar)
  ,("Umathaccent",    liftUnion MUmathaccent)
  ,("Udelimiter",     liftUnion MUdelimiter)
  ,("Uradical",       liftUnion MUradical)
  ,("Umathcharnum",   liftUnion MUmathcharnum)
  ,("Uroot",          liftUnion MUroot)
  ,("Uoverdelimiter", liftUnion MUoverdelimiter)
  ,("Uunderdelimiter",liftUnion MUunderdelimiter)
  ,("Udelimiterover", liftUnion MUdelimiterover)
  ,("Udelimiterunder",liftUnion MUdelimiterunder)
  ,("Uhextensible",   liftUnion MUhextensible)
  ,("Uskewed",        liftUnion MUskewed)
  ,("Uskewedwithdelims",liftUnion MUskewedwithdelims)
  ,("Ustack",         liftUnion MUstack)
  ,("Usuperscript",   liftUnion MUsuperscript)
  ,("Usubscript",     liftUnion MUsubscript)
  ,("Unosuperscript", liftUnion MUnosuperscript)
  ,("Unosubscript",   liftUnion MUnosubscript)
  ,("Uleft",          liftUnion MUleft)
  ,("Umiddle",        liftUnion MUmiddle)
  ,("Uright",         liftUnion MUright)

  ,("displaystyle",            liftUnion (MathStyleSet DisplayStyle))
  ,("textstyle",               liftUnion (MathStyleSet TextStyle))
  ,("scriptstyle",             liftUnion (MathStyleSet ScriptStyle))
  ,("scriptscriptstyle",       liftUnion (MathStyleSet ScriptScriptStyle))

  -- LuaTeX extensions:
  ,("crampeddisplaystyle",     liftUnion (MathStyleSet CrampedDisplayStyle))
  ,("crampedtextstyle",        liftUnion (MathStyleSet CrampedTextStyle))
  ,("crampedscriptstyle",      liftUnion (MathStyleSet CrampedScriptStyle))
  ,("crampedscriptscriptstyle",liftUnion (MathStyleSet CrampedScriptScriptStyle))
  ]

{-
'{' ... '}'
   '{' -> new math list
   '}' -> new Ord atom or single Acc atom
<math symbol>
  -> new atom
<math atom><math field>
  (<math atom> = \mathord | \mathop | \mathbin | \mathrel | \mathopen
                   | \mathclose | \mathpunct | \mathinner | \underline | \overline)
  -> new atom
\mathaccent<15-bit number><math field>
  -> new Acc atom
\radical<27-bit number><math field>
  -> new Rad atom
<superscript><math field>
  -> new Ord with empty field if the current list does not end with an atom
     the superscript field of this atom is filled by <math field>
<subscript><math field>
  -> like <superscript> but with subscript field
\displaylimits, \limits, \nolimits
  -> the current list must end with an Op atom
     modify a special field in that Op atom
\/
  -> ...
\discretionaly<general text><general text><general text>
  -> ...
\- = \discretionary{ - }{}{}
\mathchoice<general text><general text><general text><general text>
  -> ....
\displaystyle, \textstyle, \scriptstyle, \scriptscriptstyle
[LuaTeX: \crampeddisplaystyle, \crampedtextstyle, \crampedscriptstyle, \crampedscriptscriptstyle]
  -> style-change item
\left<delim><math mode material>\right<delim>
  -> ...
<generalized fraction command>
[LuaTeX: \Ustack {... <generalized fraction command> ...}]
  -> ...
-}
