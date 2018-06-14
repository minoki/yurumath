module Text.YuruMath.TeX.Math.List
  (module Text.YuruMath.TeX.Math.Style
  ,AtomType(..)
  ,mathclassToAtomType
  ,nucleusStyle
  ,MathVariant(..)
  ,SymbolMode(..)
  ,MathField(..)
  ,BinForm(..)
  ,Atom(..)
  ,atomType
  ,emptyAtom
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
import Data.Word
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

data MathField = MFEmpty
               | MFSymbol { symbolFamily  :: !Word
                          , symbolVariant :: !MathVariant
                          , symbolMode    :: !SymbolMode
                          , symbolContent :: !Text
                          }
               | MFBox
               | MFSubList MathList
               deriving (Eq,Show)

data BinForm = BinInfix
             | BinPrefix
             | BinPostfix
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
                      , atomBinForm     :: !BinForm   -- specific to Bin atom
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
                                   , atomBinForm     = BinInfix
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
data MathItem = IAtom !Atom
              | IHorizontalMaterial -- a rule or discretionary or penalty or "whatsit"
              | IVerticalMaterial -- \mark or \insert or \vadjust
              | IGlue !MathGlue -- \hskip or \mskip or \nonscript
              | IKern !MathKern -- \kern or \mkern
              | IStyleChange !MathStyle -- \displaystyle, \textstyle, etc
              | IGenFrac !GenFrac MathList MathList -- \above, \over, etc
              | IBoundary !BoundaryType !DelimiterOptions !DelimiterCode -- \left, \middle, or \right
              | IChoice MathList MathList MathList MathList -- \mathchoice
              deriving (Eq,Show)

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

type MathList = [MathItem]
