{-# LANGUAGE DeriveFunctor #-}
module Text.YuruMath.TeX.Math.List
  (module Text.YuruMath.TeX.Math.Style
  ,AtomType(..)
  ,mathclassToAtomType
  ,nucleusStyle
  ,MathVariant(..)
  ,SymbolMode(..)
  ,MathField(..)
  ,isEmptyField
  ,BinForm(..)
  ,AtomNucleus(..)
  ,AtomWithScripts(..)
  ,withEmptyScripts
  ,atomNucleusType
  ,atomType
  ,emptyAtomNucleus
  ,emptyAtom
  ,mkAtomNucleus
  ,mkAtom
  ,markAtomAsDelimiter
  ,GenFracLine(..)
  ,WithDelims(..)
  ,GenFrac
  ,BoundaryType(..)
  ,MathGlue(..)
  ,MathKern(..)
  ,MathItem(..)
  ,DelimiterOptions(..)
  ,defaultDelimiterOptions
  ,MathList
  ) where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Quantity
import Text.YuruMath.TeX.Math.Style
import Data.Text (Text)

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
              | ARoot  -- root
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

nucleusStyle :: AtomType -> MathStyle -> MathStyle
nucleusStyle AOver = makeCramped
nucleusStyle AAcc = makeCramped
nucleusStyle ARad = makeCramped
nucleusStyle ARoot = makeCramped
nucleusStyle _ = id

data MathVariant = MVNormal
                 | MVBold
                 | MVItalic
                 | MVBoldItalic
                 | MVDoubleStruck
                 | MVBoldFraktur
                 | MVScript
                 | MVBoldScript
                 | MVFraktur
                 | MVSansSerif
                 | MVBoldSansSerif
                 | MVSansSerifItalic
                 | MVSansSerifBoldItalic
                 | MVMonospace
                 | MVFunctionName
                 deriving (Eq,Show)

data SymbolMode = SMSymbol
                | SMText
                deriving (Eq,Show)

data MathField a = MFEmpty
                 | MFSymbol { symbolFamily  :: !Word
                            , symbolVariant :: !MathVariant
                            , symbolMode    :: !SymbolMode
                            , symbolContent :: !Text
                            }
                 | MFBox a
                 | MFSubList (MathList a)
                 deriving (Eq,Show,Functor)

isEmptyField :: MathField a -> Bool
isEmptyField MFEmpty = True
isEmptyField _ = False

data BinForm = BinInfix
             | BinPrefix
             | BinPostfix
             deriving (Eq,Show)

data AtomNucleus a
  = OrdAtom   { nucleusField    :: !(MathField a) }
  | OpAtom    { nucleusField    :: !(MathField a)
              , atomLimits      :: !LimitsSpec -- specific to Op atom
              }
  | BinAtom   { nucleusField    :: !(MathField a)
              , atomBinForm     :: !BinForm   -- specific to Bin atom
              }
  | RelAtom   { nucleusField    :: !(MathField a) }
  | OpenAtom  { nucleusField    :: !(MathField a)
              , atomIsDelimiter :: !Bool      -- specific to Open and Close atom
              }
  | CloseAtom { nucleusField    :: !(MathField a)
              , atomIsDelimiter :: !Bool      -- specific to Open and Close atom
              }
  | PunctAtom { nucleusField    :: !(MathField a) }
  | InnerAtom { nucleusField    :: !(MathField a) }
  | OverAtom  { nucleusField    :: !(MathField a) }
  | UnderAtom { nucleusField    :: !(MathField a) }
  | AccAtom   { nucleusField    :: !(MathField a)
              , atomAccentCharacter :: !MathCode -- specific to Acc atom
              }
  | RadAtom   { nucleusField    :: !(MathField a)
              , atomDelimiter   :: !DelimiterCode -- specific to Rad atom
              }
  | VcentAtom { nucleusField    :: !(MathField a) }
  | RootAtom  { nucleusField    :: !(MathField a)
              , atomDelimiter   :: !DelimiterCode -- specific to Root atom
              , atomRootDegree  :: !(MathField a) -- specific to Root atom
              }
  deriving (Eq,Show,Functor)

data AtomWithScripts a
  = AtomWithScripts { atomNucleus     :: !(AtomNucleus a)
                    , atomSuperscript :: !(MathField a)
                    , atomSubscript   :: !(MathField a)
                    }
  deriving (Eq,Show,Functor)

withEmptyScripts :: AtomNucleus a -> AtomWithScripts a
withEmptyScripts a = AtomWithScripts { atomNucleus = a, atomSuperscript = MFEmpty, atomSubscript = MFEmpty }

atomNucleusType :: AtomNucleus a -> AtomType
atomNucleusType (OrdAtom   {}) = AOrd
atomNucleusType (OpAtom    {}) = AOp
atomNucleusType (BinAtom   {}) = ABin
atomNucleusType (RelAtom   {}) = ARel
atomNucleusType (OpenAtom  {}) = AOpen
atomNucleusType (CloseAtom {}) = AClose
atomNucleusType (PunctAtom {}) = APunct
atomNucleusType (InnerAtom {}) = AInner
atomNucleusType (OverAtom  {}) = AOver
atomNucleusType (UnderAtom {}) = AUnder
atomNucleusType (AccAtom   {}) = AAcc
atomNucleusType (RadAtom   {}) = ARad
atomNucleusType (VcentAtom {}) = AVcent
atomNucleusType (RootAtom  {}) = ARoot

atomType :: AtomWithScripts a -> AtomType
atomType = atomNucleusType . atomNucleus

emptyAtomNucleus :: AtomNucleus a
emptyAtomNucleus = OrdAtom { nucleusField = MFEmpty }

emptyAtom :: AtomWithScripts a
emptyAtom  = withEmptyScripts emptyAtomNucleus

mkAtomNucleus :: AtomType -> MathField a -> AtomNucleus a
mkAtomNucleus !atomType !nucleus = case atomType of
  AOrd   -> OrdAtom   { nucleusField    = nucleus }
  AOp    -> OpAtom    { nucleusField    = nucleus
                      , atomLimits      = DisplayLimits -- specific to Op taom
                      }
  ABin   -> BinAtom   { nucleusField    = nucleus
                      , atomBinForm     = BinInfix
                      }
  ARel   -> RelAtom   { nucleusField    = nucleus }
  AOpen  -> OpenAtom  { nucleusField    = nucleus
                      , atomIsDelimiter = False   -- specific to Open and Close atom
                      }
  AClose -> CloseAtom { nucleusField    = nucleus
                      , atomIsDelimiter = False   -- specific to Open and Close atom
                      }
  APunct -> PunctAtom { nucleusField    = nucleus }
  AInner -> InnerAtom { nucleusField    = nucleus }
  AOver  -> OverAtom  { nucleusField    = nucleus }
  AUnder -> UnderAtom { nucleusField    = nucleus }
  AAcc   -> AccAtom   { nucleusField    = nucleus
                      , atomAccentCharacter = MathCode 0 -- specific to Acc atom
                      }
  ARad   -> RadAtom   { nucleusField    = nucleus
                      , atomDelimiter   = DelimiterCode (-1) -- specific to Rad atom
                      }
  AVcent -> VcentAtom { nucleusField    = nucleus }
  ARoot  -> RootAtom  { nucleusField    = nucleus
                      , atomDelimiter   = DelimiterCode (-1) -- specific to Rad atom
                      , atomRootDegree  = MFEmpty -- specific to Root atom
                      }

mkAtom :: AtomType -> MathField a -> AtomWithScripts a
mkAtom !atomType !nucleus = withEmptyScripts (mkAtomNucleus atomType nucleus)

markAtomAsDelimiter :: AtomWithScripts a -> AtomWithScripts a
markAtomAsDelimiter atom@(AtomWithScripts { atomNucleus = nucleus@(OpenAtom {}) }) = atom { atomNucleus = nucleus { atomIsDelimiter = True } }
markAtomAsDelimiter atom@(AtomWithScripts { atomNucleus = nucleus@(CloseAtom {}) }) = atom { atomNucleus = nucleus { atomIsDelimiter = True } }
markAtomAsDelimiter atom = atom

-- generalized fraction
data GenFracLine
  = GFOver
  | GFAtop
  | GFAbove !Dimen
  | GFSkewed !DelimiterCode {- options: noaxis, exact -}
  deriving (Eq,Show)

data WithDelims a
  = WithoutDelims !a
  | WithDelims {-left-} !DelimiterCode {-right-} !DelimiterCode !a
  deriving (Eq,Show)

type GenFrac = WithDelims GenFracLine

data BoundaryType = BoundaryLeft
                  | BoundaryRight
                  | BoundaryMiddle
                  deriving (Eq,Show)

data MathGlue = MGHSkip !(Glue Dimen)
              | MGMSkip !(Glue MuDimen)
              | MGNonscript
              deriving (Eq,Show)

data MathKern = MKKern !Dimen
              | MKMKern !MuDimen
              deriving (Eq,Show)

-- See The TeXbook, Chapter 17
data MathItem a = IAtom !(AtomWithScripts a)
                | IHorizontalMaterial -- a rule or discretionary or penalty or "whatsit"
                | IVerticalMaterial -- \mark or \insert or \vadjust
                | IGlue !MathGlue -- \hskip or \mskip or \nonscript
                | IKern !MathKern -- \kern or \mkern
                | IStyleChange !MathStyle -- \displaystyle, \textstyle, etc
                | IGenFrac !GenFrac (MathList a) (MathList a) -- \above, \over, etc
                | IBoundary !BoundaryType !DelimiterOptions !DelimiterCode -- \left, \middle, or \right
                | ISizedDelimiter !Dimen !DelimiterCode -- \big, \Big, \bigg, \Bigg
                | IChoice (MathList a) (MathList a) (MathList a) (MathList a) -- \mathchoice
                deriving (Eq,Show,Functor)

-- Options for \Uleft: "height"<dimen> | "depth"<dimen> | "exact" | "axis" | "noaxis" | "class"<number>
data DelimiterOptions = DelimiterOptions
                        { delimiterHeight :: !(Maybe Dimen)
                        , delimiterDepth :: !(Maybe Dimen)
                        , delimiterExact :: !Bool
                        , delimiterAxis :: !(Maybe Bool)
                        , delimiterClass :: !(Maybe MathClass)
                        }
                      deriving (Eq,Show)

defaultDelimiterOptions :: DelimiterOptions
defaultDelimiterOptions = DelimiterOptions Nothing Nothing False Nothing Nothing

type MathList a = [MathItem a]
