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

data Atom a = OrdAtom   { atomNucleus     :: !(MathField a)
                        , atomSuperscript :: !(MathField a)
                        , atomSubscript   :: !(MathField a)
                        }
            | OpAtom    { atomNucleus     :: !(MathField a)
                        , atomSuperscript :: !(MathField a)
                        , atomSubscript   :: !(MathField a)
                        , atomLimits      :: !LimitsSpec -- specific to Op atom
                        }
            | BinAtom   { atomNucleus     :: !(MathField a)
                        , atomSuperscript :: !(MathField a)
                        , atomSubscript   :: !(MathField a)
                        , atomBinForm     :: !BinForm   -- specific to Bin atom
                        }
            | RelAtom   { atomNucleus     :: !(MathField a)
                        , atomSuperscript :: !(MathField a)
                        , atomSubscript   :: !(MathField a)
                        }
            | OpenAtom  { atomNucleus     :: !(MathField a)
                        , atomSuperscript :: !(MathField a)
                        , atomSubscript   :: !(MathField a)
                        , atomIsDelimiter :: !Bool      -- specific to Open and Close atom
                        }
            | CloseAtom { atomNucleus     :: !(MathField a)
                        , atomSuperscript :: !(MathField a)
                        , atomSubscript   :: !(MathField a)
                        , atomIsDelimiter :: !Bool      -- specific to Open and Close atom
                        }
            | PunctAtom { atomNucleus     :: !(MathField a)
                        , atomSuperscript :: !(MathField a)
                        , atomSubscript   :: !(MathField a)
                        }
            | InnerAtom { atomNucleus     :: !(MathField a)
                        , atomSuperscript :: !(MathField a)
                        , atomSubscript   :: !(MathField a)
                        }
            | OverAtom  { atomNucleus     :: !(MathField a)
                        , atomSuperscript :: !(MathField a)
                        , atomSubscript   :: !(MathField a)
                        }
            | UnderAtom { atomNucleus     :: !(MathField a)
                        , atomSuperscript :: !(MathField a)
                        , atomSubscript   :: !(MathField a)
                        }
            | AccAtom   { atomNucleus     :: !(MathField a)
                        , atomSuperscript :: !(MathField a)
                        , atomSubscript   :: !(MathField a)
                        , atomAccentCharacter :: !MathCode -- specific to Acc atom
                        }
            | RadAtom   { atomNucleus     :: !(MathField a)
                        , atomSuperscript :: !(MathField a)
                        , atomSubscript   :: !(MathField a)
                        , atomDelimiter   :: !DelimiterCode -- specific to Rad atom
                        }
            | VcentAtom { atomNucleus     :: !(MathField a)
                        , atomSuperscript :: !(MathField a)
                        , atomSubscript   :: !(MathField a)
                        }
            | RootAtom  { atomNucleus     :: !(MathField a)
                        , atomSuperscript :: !(MathField a)
                        , atomSubscript   :: !(MathField a)
                        , atomDelimiter   :: !DelimiterCode -- specific to Root atom
                        , atomRootDegree  :: !(MathField a) -- specific to Root atom
                        }
            deriving (Eq,Show,Functor)

atomType :: Atom a -> AtomType
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
atomType (RootAtom  {}) = ARoot

emptyAtom :: Atom a
emptyAtom = OrdAtom { atomNucleus     = MFEmpty
                    , atomSuperscript = MFEmpty
                    , atomSubscript   = MFEmpty
                    }

mkAtom :: AtomType -> MathField a -> Atom a
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
mkAtom ARoot  !nucleus = RootAtom  { atomNucleus     = nucleus
                                   , atomSuperscript = MFEmpty
                                   , atomSubscript   = MFEmpty
                                   , atomDelimiter   = DelimiterCode (-1) -- specific to Rad atom
                                   , atomRootDegree  = MFEmpty -- specific to Root atom
                                   }

markAtomAsDelimiter :: Atom a -> Atom a
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
data MathItem a = IAtom !(Atom a)
                | IHorizontalMaterial -- a rule or discretionary or penalty or "whatsit"
                | IVerticalMaterial -- \mark or \insert or \vadjust
                | IGlue !MathGlue -- \hskip or \mskip or \nonscript
                | IKern !MathKern -- \kern or \mkern
                | IStyleChange !MathStyle -- \displaystyle, \textstyle, etc
                | IGenFrac !GenFrac (MathList a) (MathList a) -- \above, \over, etc
                | IBoundary !BoundaryType !DelimiterOptions !DelimiterCode -- \left, \middle, or \right
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
