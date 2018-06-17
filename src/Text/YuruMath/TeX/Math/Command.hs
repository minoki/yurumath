{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
module Text.YuruMath.TeX.Math.Command
  (MathStyleSet(..)
  ,MathAtomCommand(..)
  ,MathVariantSet(..)
  ,MathSymbolModeSet(..)
  ,MathCommands(..)
  ,MathExpandableList
  ,MathNonExpandablePrimitiveList
  ,mathDefinitions
  ) where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Quantity
import Text.YuruMath.TeX.Meaning
import Text.YuruMath.TeX.Expansion
import Text.YuruMath.TeX.Execution
import Text.YuruMath.TeX.Math.List
import Text.YuruMath.TeX.Math.State
import Control.Monad.Except (MonadError,throwError)
import Control.Lens.Lens (Lens')
import Control.Lens.Getter (use,uses)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.OpenUnion (Union,liftUnion)
import TypeFun.Data.List (SubList)

famSet :: (MonadTeXState state m, MonadError String m, IsMathState state) => m (Assignment state)
famSet = do
  val <- readIntBetween 0 255
  texAssign famParam val

famGet :: (MonadTeXState state m, IsMathState state) => m Integer
famGet = uses (localState . famParam) fromIntegral

muskipParamSet :: (MonadTeXState s m, MonadError String m) => Lens' (LocalState s) (Glue MuDimen) -> m (Assignment s)
muskipParamSet muskip = readMuGlue >>= texAssign muskip

--
-- Setting math style
--

newtype MathStyleSet = MathStyleSet MathStyle
                     deriving (Eq,Show)

instance Meaning MathStyleSet where
  meaningString (MathStyleSet DisplayStyle) = controlSequence "displaystyle"
  meaningString (MathStyleSet CrampedDisplayStyle) = controlSequence "crampeddisplaystyle"
  meaningString (MathStyleSet TextStyle) = controlSequence "textstyle"
  meaningString (MathStyleSet CrampedTextStyle) = controlSequence "crampedtextstyle"
  meaningString (MathStyleSet ScriptStyle) = controlSequence "scriptstyle"
  meaningString (MathStyleSet CrampedScriptStyle) = controlSequence "crampedscriptstyle"
  meaningString (MathStyleSet ScriptScriptStyle) = controlSequence "scriptscriptstyle"
  meaningString (MathStyleSet CrampedScriptScriptStyle) = controlSequence "crampedscriptscriptstyle"

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute MathStyleSet m where
  doExecute = can'tUseThisCommandInCurrentMode
  getQuantity (MathStyleSet v) = QInteger $ return $ fromIntegral $ fromEnum v -- LuaTeX extension

--
-- Math atom command (like \mathord)
--

newtype MathAtomCommand = MathAtomCommand AtomType deriving (Eq,Show)

instance Meaning MathAtomCommand where
  meaningString (MathAtomCommand AOrd) = controlSequence "mathord"
  meaningString (MathAtomCommand AOp) = controlSequence "mathop"
  meaningString (MathAtomCommand ABin) = controlSequence "mathbin"
  meaningString (MathAtomCommand ARel) = controlSequence "mathrel"
  meaningString (MathAtomCommand AOpen) = controlSequence "mathopen"
  meaningString (MathAtomCommand AClose) = controlSequence "mathclose"
  meaningString (MathAtomCommand APunct) = controlSequence "mathpunct"
  meaningString (MathAtomCommand AInner) = controlSequence "mathinner"
  meaningString (MathAtomCommand AUnder) = controlSequence "underline"
  meaningString (MathAtomCommand AOver) = controlSequence "overline"
  meaningString (MathAtomCommand AAcc) = pure "<<accented atom>>"
  meaningString (MathAtomCommand ARad) = pure "<<radical>>"
  meaningString (MathAtomCommand AVcent) = pure "<<vcenter>>"
  meaningString (MathAtomCommand ARoot) = pure "<<root>>"

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute MathAtomCommand m where
  doExecute = can'tUseThisCommandInCurrentMode
  getQuantity _ = NotQuantity

--
-- Setting math variant
--

newtype MathVariantSet = MathVariantSet MathVariant deriving (Eq,Show)

instance Meaning MathVariantSet where
  meaningString (MathVariantSet MVNormal) = controlSequence "YuruMathSetNormal"
  meaningString (MathVariantSet MVBold) = controlSequence "YuruMathSetBold"
  meaningString (MathVariantSet MVItalic) = controlSequence "YuruMathSetItalic"
  meaningString (MathVariantSet MVBoldItalic) = controlSequence "YuruMathSetBoldItalic"
  meaningString (MathVariantSet MVDoubleStruck) = controlSequence "YuruMathSetDoubleStruck"
  meaningString (MathVariantSet MVBoldFraktur) = controlSequence "YuruMathSetBoldFraktur"
  meaningString (MathVariantSet MVScript) = controlSequence "YuruMathSetScript"
  meaningString (MathVariantSet MVBoldScript) = controlSequence "YuruMathSetBoldScript"
  meaningString (MathVariantSet MVFraktur) = controlSequence "YuruMathSetFraktur"
  meaningString (MathVariantSet MVSansSerif) = controlSequence "YuruMathSetSansSerif"
  meaningString (MathVariantSet MVBoldSansSerif) = controlSequence "YuruMathSetBoldSansSerif"
  meaningString (MathVariantSet MVSansSerifItalic) = controlSequence "YuruMathSetSansSerifItalic"
  meaningString (MathVariantSet MVSansSerifBoldItalic) = controlSequence "YuruMathSetSansSerifBoldItalic"
  meaningString (MathVariantSet MVMonospace) = controlSequence "YuruMathSetMonospace"
  meaningString (MathVariantSet MVFunctionName) = controlSequence "YuruMathSetFunctionName"

instance (Monad m, MonadError String m) => DoExecute MathVariantSet m where
  doExecute _ = throwError "You can't set math variant in this mode"
  getQuantity _ = NotQuantity

--
-- Setting symbol mode
--

newtype MathSymbolModeSet = MathSymbolModeSet SymbolMode deriving (Eq,Show)

instance Meaning MathSymbolModeSet where
  meaningString (MathSymbolModeSet SMSymbol) = controlSequence "YuruMathSetSymbol"
  meaningString (MathSymbolModeSet SMText) = controlSequence "YuruMathSetText"

instance (Monad m, MonadError String m) => DoExecute MathSymbolModeSet m where
  doExecute _ = throwError "You can't set math symbol mode in this mode"
  getQuantity _ = NotQuantity

--
-- Expandable math commands
--

data MathExpandable = Mmathstyle -- LuaTeX extension
                    deriving (Eq,Show)

-- LuaTeX extension: \mathstyle
mathstyleCommand :: (MonadTeXState state m, MonadError String m, IsMathState state) => m [ExpansionToken]
mathstyleCommand = do
  value <- use currentMathStyle
  case value of
    Just style -> stringToEToken $ show $ fromEnum style -- 0..7
    Nothing -> stringToEToken "-1" -- not in math mode

instance Meaning MathExpandable where
  meaningString Mmathstyle = controlSequence "mathstyle"

instance IsExpandable MathExpandable where
  isConditional _ = False

instance (Monad m, MonadTeXState state m, MonadError String m, IsMathState state) => DoExpand MathExpandable m where
  doExpand Mmathstyle = mathstyleCommand
  evalBooleanConditional _ = Nothing

--
-- Other math commands
--

data MathCommands
  = Mmathchar
  | Mmathaccent
  | Mdelimiter
  | Mradical
  | Mdisplaylimits
  | Mlimits
  | Mnolimits
  | Mmathchoice
  | Mleft
  | Mright
  | Mover
  | Matop
  | Mabove
  | Moverwithdelims
  | Matopwithdelims
  | Mabovewithdelims
  | Mfam
  | Mthinmuskip
  | Mmedmuskip
  | Mthickmuskip
  | Mmkern
  | Mmskip
  | Mnonscript
  | Mvcenter

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
  | MUstopmath
  | MUstopdisplaymath
  -- \Ustartmath, \Ustartdisplaymath: not really math commands...

  | MYuruMathSizedDelimiter -- for \big, \Big, \bigg, \Bigg

  deriving (Eq,Show)

instance Meaning MathCommands where
  meaningString Mmathchar = controlSequence "mathchar"
  meaningString Mmathaccent = controlSequence "mathaccent"
  meaningString Mdelimiter = controlSequence "delimiter"
  meaningString Mradical = controlSequence "radical"
  meaningString Mdisplaylimits = controlSequence "displaylimits"
  meaningString Mlimits = controlSequence "limits"
  meaningString Mnolimits = controlSequence "nolimits"
  meaningString Mmathchoice = controlSequence "mathchoice"
  meaningString Mleft = controlSequence "left"
  meaningString Mright = controlSequence "right"
  meaningString Mover = controlSequence "over"
  meaningString Matop = controlSequence "atop"
  meaningString Mabove = controlSequence "above"
  meaningString Moverwithdelims = controlSequence "overwithdelims"
  meaningString Matopwithdelims = controlSequence "atopwithdelims"
  meaningString Mabovewithdelims = controlSequence "abovewithdelims"
  meaningString Mfam = controlSequence "fam"
  meaningString Mthinmuskip = controlSequence "thinmuskip"
  meaningString Mmedmuskip = controlSequence "medmuskip"
  meaningString Mthickmuskip = controlSequence "thickmuskip"
  meaningString Mmkern = controlSequence "mkern"
  meaningString Mmskip = controlSequence "mskip"
  meaningString Mnonscript = controlSequence "nonscript"
  meaningString Mvcenter = controlSequence "vcenter"
  meaningString Mmiddle = controlSequence "middle"
  meaningString MUmathchar = controlSequence "Umathchar"
  meaningString MUmathcharnum = controlSequence "Umathcharnum"
  meaningString MUmathaccent = controlSequence "Umathaccent"
  meaningString MUdelimiter = controlSequence "Udelimiter"
  meaningString MUradical = controlSequence "Uradical"
  meaningString MUroot = controlSequence "Uroot"
  meaningString MUoverdelimiter = controlSequence "Uoverdelimiter"
  meaningString MUunderdelimiter = controlSequence "Uunderdelimiter"
  meaningString MUdelimiterover = controlSequence "Udelimiterover"
  meaningString MUdelimiterunder = controlSequence "Udelimiterunder"
  meaningString MUhextensible = controlSequence "Uhextensible"
  meaningString MUskewed = controlSequence "Uskewed"
  meaningString MUskewedwithdelims = controlSequence "Uskewedwithdelims"
  meaningString MUstack = controlSequence "Ustack"
  meaningString MUsuperscript = controlSequence "Usuperscript"
  meaningString MUsubscript = controlSequence "Usubscript"
  meaningString MUnosuperscript = controlSequence "Unosuperscript"
  meaningString MUnosubscript = controlSequence "Unosubscript"
  meaningString MUleft = controlSequence "Uleft"
  meaningString MUmiddle = controlSequence "Umiddle"
  meaningString MUright = controlSequence "Uright"
  meaningString MUstopmath = controlSequence "Ustopmath"
  meaningString MUstopdisplaymath = controlSequence "Ustopdisplaymath"
  meaningString MYuruMathSizedDelimiter = controlSequence "YuruMathSizedDelimiter"

instance (Monad m, MonadTeXState state m, MonadError String m, IsMathState state) => DoExecute MathCommands m where
  doExecute Mfam           = runLocal famSet
  doExecute Mthinmuskip    = runLocal (muskipParamSet thinmuskip)
  doExecute Mmedmuskip     = runLocal (muskipParamSet medmuskip)
  doExecute Mthickmuskip   = runLocal (muskipParamSet thickmuskip)
  doExecute x              = can'tUseThisCommandInCurrentMode x
  doGlobal Mfam            = Just $ runGlobal famSet
  doGlobal Mthinmuskip     = Just $ runGlobal (muskipParamSet thinmuskip)
  doGlobal Mmedmuskip      = Just $ runGlobal (muskipParamSet medmuskip)
  doGlobal Mthickmuskip    = Just $ runGlobal (muskipParamSet thickmuskip)
  doGlobal _               = Nothing
  doAdvance Mfam           = Just $ runArithmetic $ advanceInt famParam
  doAdvance Mthinmuskip    = Just $ runArithmetic $ advanceQuantity thinmuskip
  doAdvance Mmedmuskip     = Just $ runArithmetic $ advanceQuantity medmuskip
  doAdvance Mthickmuskip   = Just $ runArithmetic $ advanceQuantity thickmuskip
  doAdvance _              = Nothing
  doMultiply Mfam          = Just $ runArithmetic $ multiplyInt famParam
  doMultiply Mthinmuskip   = Just $ runArithmetic $ multiplyQuantity thinmuskip
  doMultiply Mmedmuskip    = Just $ runArithmetic $ multiplyQuantity medmuskip
  doMultiply Mthickmuskip  = Just $ runArithmetic $ multiplyQuantity thickmuskip
  doMultiply _             = Nothing
  doDivide Mfam            = Just $ runArithmetic $ divideInt famParam
  doDivide Mthinmuskip     = Just $ runArithmetic $ divideQuantity thinmuskip
  doDivide Mmedmuskip      = Just $ runArithmetic $ divideQuantity medmuskip
  doDivide Mthickmuskip    = Just $ runArithmetic $ divideQuantity thickmuskip
  doDivide _               = Nothing
  getQuantity Mfam         = QInteger famGet
  getQuantity Mthinmuskip  = QMuGlue (use (localState . thinmuskip))
  getQuantity Mmedmuskip   = QMuGlue (use (localState . medmuskip))
  getQuantity Mthickmuskip = QMuGlue (use (localState . thickmuskip))
  getQuantity _            = NotQuantity

--
-- List of commands
--

type MathExpandableList = '[MathExpandable]
type MathNonExpandablePrimitiveList = '[MathCommands,MathAtomCommand,MathStyleSet,MathVariantSet,MathSymbolModeSet]

mathDefinitions :: (SubList MathExpandableList eset, SubList MathNonExpandablePrimitiveList vset) => Map.Map Text (Either (Union eset) (Union vset))
mathDefinitions = Map.fromList
  [("mathstyle", Left $ liftUnion Mmathstyle) -- LuaTeX extension
  ]
  <> fmap Right (Map.fromList
  [("mathchar",     liftUnion Mmathchar)
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
  ,("mathchoice",   liftUnion Mmathchoice)
  ,("left",         liftUnion Mleft)
  ,("right",        liftUnion Mright)
  ,("over",         liftUnion Mover)
  ,("atop",         liftUnion Matop)
  ,("above",        liftUnion Mabove)
  ,("overwithdelims",liftUnion Moverwithdelims)
  ,("atopwithdelims",liftUnion Matopwithdelims)
  ,("abovewithdelims",liftUnion Mabovewithdelims)
  ,("fam",          liftUnion Mfam)
  ,("thinmuskip",   liftUnion Mthinmuskip)
  ,("medmuskip",    liftUnion Mmedmuskip)
  ,("thickmuskip",  liftUnion Mthickmuskip)
  ,("mkern",        liftUnion Mmkern)
  ,("mskip",        liftUnion Mmskip)
  ,("nonscript",    liftUnion Mnonscript)
  ,("vcenter",      liftUnion Mvcenter)

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
  ,("Ustopmath",      liftUnion MUstopmath)
  ,("Ustopdisplaymath",liftUnion MUstopdisplaymath)

  ,("displaystyle",            liftUnion (MathStyleSet DisplayStyle))
  ,("textstyle",               liftUnion (MathStyleSet TextStyle))
  ,("scriptstyle",             liftUnion (MathStyleSet ScriptStyle))
  ,("scriptscriptstyle",       liftUnion (MathStyleSet ScriptScriptStyle))

  -- LuaTeX extensions:
  ,("crampeddisplaystyle",     liftUnion (MathStyleSet CrampedDisplayStyle))
  ,("crampedtextstyle",        liftUnion (MathStyleSet CrampedTextStyle))
  ,("crampedscriptstyle",      liftUnion (MathStyleSet CrampedScriptStyle))
  ,("crampedscriptscriptstyle",liftUnion (MathStyleSet CrampedScriptScriptStyle))

  ,("YuruMathSizedDelimiter",        liftUnion MYuruMathSizedDelimiter)
  ,("YuruMathSetNormal",             liftUnion (MathVariantSet MVNormal))
  ,("YuruMathSetBold",               liftUnion (MathVariantSet MVBold))
  ,("YuruMathSetItalic",             liftUnion (MathVariantSet MVItalic))
  ,("YuruMathSetBoldItalic",         liftUnion (MathVariantSet MVBoldItalic))
  ,("YuruMathSetDoubleStruck",       liftUnion (MathVariantSet MVDoubleStruck))
  ,("YuruMathSetBoldFraktur",        liftUnion (MathVariantSet MVBoldFraktur))
  ,("YuruMathSetScript",             liftUnion (MathVariantSet MVScript))
  ,("YuruMathSetBoldScript",         liftUnion (MathVariantSet MVBoldScript))
  ,("YuruMathSetFraktur",            liftUnion (MathVariantSet MVFraktur))
  ,("YuruMathSetSansSerif",          liftUnion (MathVariantSet MVSansSerif))
  ,("YuruMathSetBoldSansSerif",      liftUnion (MathVariantSet MVBoldSansSerif))
  ,("YuruMathSetSansSerifItalic",    liftUnion (MathVariantSet MVSansSerifItalic))
  ,("YuruMathSetSansSerifBoldItalic",liftUnion (MathVariantSet MVSansSerifBoldItalic))
  ,("YuruMathSetMonospace",          liftUnion (MathVariantSet MVMonospace))
  ,("YuruMathSetFunctionName",       liftUnion (MathVariantSet MVFunctionName))
  ,("YuruMathSetText",               liftUnion (MathSymbolModeSet SMText))
  ,("YuruMathSetSymbol",             liftUnion (MathSymbolModeSet SMSymbol))
  ])
