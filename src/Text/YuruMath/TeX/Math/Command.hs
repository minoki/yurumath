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

instance (Monad m, MonadError String m) => DoExecute MathStyleSet m where
  doExecute (MathStyleSet s) = throwError "You can't use \\displaystyle in this mode" -- TODO: name
  getQuantity (MathStyleSet v) = QInteger $ return $ fromIntegral $ fromEnum v -- LuaTeX extension

--
-- Math atom command (like \mathord)
--

newtype MathAtomCommand = MathAtomCommand AtomType deriving (Eq,Show)

instance (Monad m, MonadError String m) => DoExecute MathAtomCommand m where
  doExecute _ = throwError "You can't use \\mathord in this mode" -- TODO: name
  getQuantity _ = NotQuantity

--
-- Setting math variant
--

newtype MathVariantSet = MathVariantSet MathVariant deriving (Eq,Show)

instance (Monad m, MonadError String m) => DoExecute MathVariantSet m where
  doExecute _ = throwError "You can't set math variant in this mode"
  getQuantity _ = NotQuantity

--
-- Setting symbol mode
--

newtype MathSymbolModeSet = MathSymbolModeSet SymbolMode deriving (Eq,Show)

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
  style <- use currentMathStyle
  stringToEToken $ show $ fromEnum style -- 0..7

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

  deriving (Eq,Show)

instance (Monad m, MonadTeXState state m, MonadError String m, IsMathState state) => DoExecute MathCommands m where
  doExecute Mfam           = runLocal famSet
  doExecute Mthinmuskip    = runLocal (muskipParamSet thinmuskip)
  doExecute Mmedmuskip     = runLocal (muskipParamSet medmuskip)
  doExecute Mthickmuskip   = runLocal (muskipParamSet thickmuskip)
  doExecute x              = throwError $ "You can't use " ++ show x ++ " in non-math mode"
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
