{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
                     deriving (Eq,Show,Enum,Bounded)

instance IsPrimitive MathStyleSet where
  primitiveName (MathStyleSet DisplayStyle) = "displaystyle"
  primitiveName (MathStyleSet CrampedDisplayStyle) = "crampeddisplaystyle"
  primitiveName (MathStyleSet TextStyle) = "textstyle"
  primitiveName (MathStyleSet CrampedTextStyle) = "crampedtextstyle"
  primitiveName (MathStyleSet ScriptStyle) = "scriptstyle"
  primitiveName (MathStyleSet CrampedScriptStyle) = "crampedscriptstyle"
  primitiveName (MathStyleSet ScriptScriptStyle) = "scriptscriptstyle"
  primitiveName (MathStyleSet CrampedScriptScriptStyle) = "crampedscriptscriptstyle"

instance Meaning MathStyleSet

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
  meaningString (MathAtomCommand AAcc) = "<<accented atom>>"
  meaningString (MathAtomCommand ARad) = "<<radical>>"
  meaningString (MathAtomCommand AVcent) = "<<vcenter>>"
  meaningString (MathAtomCommand ARoot) = "<<root>>"

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute MathAtomCommand m where
  doExecute = can'tUseThisCommandInCurrentMode
  getQuantity _ = NotQuantity

--
-- Setting math variant
--

newtype MathVariantSet = MathVariantSet MathVariant deriving (Eq,Show,Enum,Bounded)

instance IsPrimitive MathVariantSet where
  primitiveName (MathVariantSet MVNormal) = "YuruMathSetNormal"
  primitiveName (MathVariantSet MVBold) = "YuruMathSetBold"
  primitiveName (MathVariantSet MVItalic) = "YuruMathSetItalic"
  primitiveName (MathVariantSet MVBoldItalic) = "YuruMathSetBoldItalic"
  primitiveName (MathVariantSet MVDoubleStruck) = "YuruMathSetDoubleStruck"
  primitiveName (MathVariantSet MVBoldFraktur) = "YuruMathSetBoldFraktur"
  primitiveName (MathVariantSet MVScript) = "YuruMathSetScript"
  primitiveName (MathVariantSet MVBoldScript) = "YuruMathSetBoldScript"
  primitiveName (MathVariantSet MVFraktur) = "YuruMathSetFraktur"
  primitiveName (MathVariantSet MVSansSerif) = "YuruMathSetSansSerif"
  primitiveName (MathVariantSet MVBoldSansSerif) = "YuruMathSetBoldSansSerif"
  primitiveName (MathVariantSet MVSansSerifItalic) = "YuruMathSetSansSerifItalic"
  primitiveName (MathVariantSet MVSansSerifBoldItalic) = "YuruMathSetSansSerifBoldItalic"
  primitiveName (MathVariantSet MVMonospace) = "YuruMathSetMonospace"
  primitiveName (MathVariantSet MVFunctionName) = "YuruMathSetFunctionName"

instance Meaning MathVariantSet

instance (Monad m, MonadError String m) => DoExecute MathVariantSet m where
  doExecute _ = throwError "You can't set math variant in this mode"
  getQuantity _ = NotQuantity

--
-- Setting symbol mode
--

newtype MathSymbolModeSet = MathSymbolModeSet SymbolMode deriving (Eq,Show)

instance IsPrimitive MathSymbolModeSet where
  primitiveName (MathSymbolModeSet SMSymbol) = "YuruMathSetSymbol"
  primitiveName (MathSymbolModeSet SMText) = "YuruMathSetText"
  primitivesOfThisType = [MathSymbolModeSet SMSymbol, MathSymbolModeSet SMText]

instance Meaning MathSymbolModeSet

instance (Monad m, MonadError String m) => DoExecute MathSymbolModeSet m where
  doExecute _ = throwError "You can't set math symbol mode in this mode"
  getQuantity _ = NotQuantity

--
-- Expandable math commands
--

data MathExpandable = Mmathstyle -- LuaTeX extension
                    deriving (Eq,Show,Enum,Bounded)

-- LuaTeX extension: \mathstyle
mathstyleCommand :: (MonadTeXState state m, MonadError String m, IsMathState state) => m [ExpansionToken]
mathstyleCommand = do
  value <- use currentMathStyle
  return $ case value of
    Just style -> stringToEToken $ show $ fromEnum style -- 0..7
    Nothing -> stringToEToken "-1" -- not in math mode

instance IsPrimitive MathExpandable where
  primitiveName Mmathstyle = "mathstyle"

instance Meaning MathExpandable

instance IsExpandable MathExpandable where
  isConditional _ = False

instance (Monad m, MonadTeXState state m, MonadError String m, IsMathState state) => DoExpand MathExpandable m where
  doExpand Mmathstyle _ = mathstyleCommand
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
  | MYuruMathInternalMathStyle

  deriving (Eq,Show,Enum,Bounded)

instance IsPrimitive MathCommands where
  primitiveName Mmathchar = "mathchar"
  primitiveName Mmathaccent = "mathaccent"
  primitiveName Mdelimiter = "delimiter"
  primitiveName Mradical = "radical"
  primitiveName Mdisplaylimits = "displaylimits"
  primitiveName Mlimits = "limits"
  primitiveName Mnolimits = "nolimits"
  primitiveName Mmathchoice = "mathchoice"
  primitiveName Mleft = "left"
  primitiveName Mright = "right"
  primitiveName Mover = "over"
  primitiveName Matop = "atop"
  primitiveName Mabove = "above"
  primitiveName Moverwithdelims = "overwithdelims"
  primitiveName Matopwithdelims = "atopwithdelims"
  primitiveName Mabovewithdelims = "abovewithdelims"
  primitiveName Mfam = "fam"
  primitiveName Mthinmuskip = "thinmuskip"
  primitiveName Mmedmuskip = "medmuskip"
  primitiveName Mthickmuskip = "thickmuskip"
  primitiveName Mmkern = "mkern"
  primitiveName Mmskip = "mskip"
  primitiveName Mnonscript = "nonscript"
  primitiveName Mvcenter = "vcenter"
  primitiveName Mmiddle = "middle"
  primitiveName MUmathchar = "Umathchar"
  primitiveName MUmathcharnum = "Umathcharnum"
  primitiveName MUmathaccent = "Umathaccent"
  primitiveName MUdelimiter = "Udelimiter"
  primitiveName MUradical = "Uradical"
  primitiveName MUroot = "Uroot"
  primitiveName MUoverdelimiter = "Uoverdelimiter"
  primitiveName MUunderdelimiter = "Uunderdelimiter"
  primitiveName MUdelimiterover = "Udelimiterover"
  primitiveName MUdelimiterunder = "Udelimiterunder"
  primitiveName MUhextensible = "Uhextensible"
  primitiveName MUskewed = "Uskewed"
  primitiveName MUskewedwithdelims = "Uskewedwithdelims"
  primitiveName MUstack = "Ustack"
  primitiveName MUsuperscript = "Usuperscript"
  primitiveName MUsubscript = "Usubscript"
  primitiveName MUnosuperscript = "Unosuperscript"
  primitiveName MUnosubscript = "Unosubscript"
  primitiveName MUleft = "Uleft"
  primitiveName MUmiddle = "Umiddle"
  primitiveName MUright = "Uright"
  primitiveName MUstopmath = "Ustopmath"
  primitiveName MUstopdisplaymath = "Ustopdisplaymath"
  primitiveName MYuruMathSizedDelimiter = "YuruMathSizedDelimiter"
  primitiveName MYuruMathInternalMathStyle = "YuruMathInternalMathStyle"

instance Meaning MathCommands

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
  doArithmetic Mfam         = Just $ arithmeticInteger famParam
  doArithmetic Mthinmuskip  = Just $ arithmeticQuantity thinmuskip
  doArithmetic Mmedmuskip   = Just $ arithmeticQuantity medmuskip
  doArithmetic Mthickmuskip = Just $ arithmeticQuantity thickmuskip
  doArithmetic _            = Nothing
  getQuantity Mfam         = QInteger famGet
  getQuantity Mthinmuskip  = QMuGlue (use (localState . thinmuskip))
  getQuantity Mmedmuskip   = QMuGlue (use (localState . medmuskip))
  getQuantity Mthickmuskip = QMuGlue (use (localState . thickmuskip))
  getQuantity MYuruMathInternalMathStyle = QInteger $ (fromIntegral . maybe (-1) fromEnum) <$> use currentMathStyle
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
  ,("YuruMathInternalMathStyle",     liftUnion MYuruMathInternalMathStyle)
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
