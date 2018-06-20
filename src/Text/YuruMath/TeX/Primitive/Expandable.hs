{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module Text.YuruMath.TeX.Primitive.Expandable
  (expandableDefinitions
  ,ConditionalMarkerCommand
  ,CommonExpandable
  ,CommonBoolean
  ) where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Meaning
import Text.YuruMath.TeX.Expansion
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.Error.Class
import Control.Lens.Getter (use,uses)
import Control.Lens.Setter (assign,modifying)
import Data.OpenUnion
import TypeFun.Data.List (SubList)

expandafterCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
expandafterCommand = do
  t1 <- required nextEToken
  t2 <- required nextEToken
  (t1:) <$> expandOnce t2

noexpandCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
noexpandCommand = do
  t <- required nextEToken
  case t of
    ETCommandName { etFlavor = ECNFPlain, etName = name } -> do
      m <- use (localState . definitionAt name)
      return $ case m of
        Left _ -> [ETCommandName { etDepth = 0, etFlavor = ECNFNoexpanded, etName = name }] -- expandable
        Right c | Just (Undefined _) <- toCommonValue c -> [ETCommandName { etDepth = 0, etFlavor = ECNFNoexpanded, etName = name }] -- undefined
        Right _ -> [t] -- not expandable
    _ -> return [t]

csnameCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
csnameCommand = do
  name <- readUntilEndcsname []
  let tname = T.pack name

  -- THE DREADED SIDE EFFECT OF \csname
  d <- use (localState . controlSeqDef)
  when (Map.notMember tname d)
    $ modifying (localState . controlSeqDef) (Map.insert tname (Right (injectCommonValue Relax)))

  return [ETCommandName { etDepth = 0, etFlavor = ECNFPlain, etName = NControlSeq tname }]

-- LuaTeX extension: \begincsname
begincsnameCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
begincsnameCommand = do
  name <- readUntilEndcsname []
  let tname = T.pack name
  return [ETCommandName { etDepth = 0, etFlavor = ECNFPlain, etName = NControlSeq tname }]

stringCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
stringCommand = do
  t <- required nextEToken
  case t of
    ETCommandName { etName = NControlSeq name } -> do
      ech <- use (localState . escapechar)
      return $ if isUnicodeScalarValue ech
               then stringToEToken (chr ech : T.unpack name)
               else stringToEToken (T.unpack name)
    ETCommandName { etName = NActiveChar c } ->
      return $ stringToEToken [c]
    ETCharacter { etChar = c } ->
      return $ stringToEToken [c]

-- LuaTeX extension: \csstring
csstringCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
csstringCommand = do
  t <- required nextEToken
  return $ case t of
    ETCommandName { etName = NControlSeq name } ->
      stringToEToken (T.unpack name)
    ETCommandName { etName = NActiveChar c } ->
      stringToEToken [c]
    ETCharacter { etChar = c } ->
      stringToEToken [c]

numberCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
numberCommand = do
  stringToEToken . show <$> readNumber

theCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
theCommand = stringToEToken <$> theString "\\the"

meaningCommand :: (MonadTeXState s m, MonadError String m, Meaning (Expandable s), Meaning (Value s)) => m [ExpansionToken]
meaningCommand = do
  e <- use (localState . escapechar)
  value <- required nextEToken >>= meaningWithoutExpansion
  return $ stringToEToken (meaningWithEscapecharAsInt e value)

romannumeralCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
romannumeralCommand = do
  -- Should guard against big input?
  stringToEToken . showRomannumeral <$> readGeneralInt

elseCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
elseCommand = do
  cs <- use conditionals
  case cs of
    CondTruthy:css -> do
      -- \iftrue ... >>>\else<<< ... \fi
      skipUntilFi 0
      assign conditionals css
      return []
    CondCase:css -> do
      -- \ifcase ... \or ... >>>\else<<< ... \fi
      skipUntilFi 0
      assign conditionals css
      return []
    CondTest:_ -> throwError "internal error: \\else expansion in conditional"
    _ -> throwError "Extra \\else"

fiCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
fiCommand = do
  cs <- use conditionals
  case cs of
    [] -> throwError "Extra \\fi"
    CondTest:_ -> throwError "internal error: \\fi expansion in conditional"
    _:css -> do
      -- \iftrue ... >>>\fi<<<
      -- OR
      -- \iffalse ... \else ... >>>\fi<<<
      assign conditionals css
      return []

orCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
orCommand = do
  cs <- use conditionals
  case cs of
    CondCase:css -> do
      -- \ifcase N ... >>>\or<<< ... \fi
      skipUntilFi 0
      assign conditionals css
      return []
    CondTest:_ -> throwError "internal error: \\or expansion in conditional"
    _ -> throwError "Extra \\or"

newtype ConditionalMarkerCommand = ConditionalMarkerCommand ConditionalMarker
  deriving (Eq)

instance Show ConditionalMarkerCommand where
  show (ConditionalMarkerCommand c) = case c of
    Eelse -> "\\else"
    Efi -> "\\fi"
    Eor -> "\\or"

instance IsExpandable ConditionalMarkerCommand where
  isConditional _     = False
  isIfCase _          = False
  isConditionalMarker (ConditionalMarkerCommand x) = Just x

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExpand ConditionalMarkerCommand m where
  doExpand (ConditionalMarkerCommand Eelse) = elseCommand
  doExpand (ConditionalMarkerCommand Efi)   = fiCommand
  doExpand (ConditionalMarkerCommand Eor)   = orCommand
  evalBooleanConditional _ = Nothing

instance Meaning ConditionalMarkerCommand where
  meaningString (ConditionalMarkerCommand Eelse) = controlSequence "else"
  meaningString (ConditionalMarkerCommand Efi) = controlSequence "fi"
  meaningString (ConditionalMarkerCommand Eor) = controlSequence "or"

doIfCase :: (MonadTeXState s m, MonadError String m) => Integer -> m ()
doIfCase 0 = do
  modifying conditionals (CondCase:)
doIfCase n = do
  k <- skipUntilOr 0
  case k of
    FoundFi -> return ()
    FoundElse -> modifying conditionals (CondFalsy:)
    FoundOr -> doIfCase (n - 1)

ifcaseCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
ifcaseCommand = do
  x <- readNumber
  doIfCase x
  return []

iftrueCommand :: (MonadTeXState s m, MonadError String m) => m Bool
iftrueCommand = return True

iffalseCommand :: (MonadTeXState s m, MonadError String m) => m Bool
iffalseCommand = return False

-- \if: test character codes
ifCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifCommand = do
  t1 <- fst <$> evalToken
  t2 <- fst <$> evalToken
  let toChar (ETCharacter { etChar = c }) = Just c
      toChar (ETCommandName { etName = NActiveChar c }) = Just c
      toChar (ETCommandName { etName = NControlSeq _ }) = Nothing
  return $ toChar t1 == toChar t2

-- \ifcat: test category codes
ifcatCommand :: (MonadTeXState a m, MonadError String m) => m Bool
ifcatCommand = do
  t1 <- fst <$> evalToken
  t2 <- fst <$> evalToken
  let toCC (ETCharacter { etCatCode = cc }) = Just cc
      toCC (ETCommandName { etName = NActiveChar _ }) = Just CCActive
      toCC (ETCommandName { etName = NControlSeq _ }) = Nothing
  return $ toCC t1 == toCC t2

ifxCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifxCommand = do
  t1 <- required nextEToken >>= meaningWithoutExpansion
  t2 <- required nextEToken >>= meaningWithoutExpansion
  return $ t1 == t2

ifnumCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifnumCommand = do
  x <- readNumber
  readOptionalSpaces
  (rel,_) <- evalToken
  y <- readNumber
  case rel of
    ETCharacter { etChar = '<', etCatCode = CCOther } -> return $ x < y
    ETCharacter { etChar = '=', etCatCode = CCOther } -> return $ x == y
    ETCharacter { etChar = '>', etCatCode = CCOther } -> return $ x > y
    _ -> throwError "unrecognized relation for \\ifnum"

ifdimCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifdimCommand = do
  x <- readDimension
  readOptionalSpaces
  (rel,_) <- evalToken
  y <- readDimension
  case rel of
    ETCharacter { etChar = '<', etCatCode = CCOther } -> return $ x < y
    ETCharacter { etChar = '=', etCatCode = CCOther } -> return $ x == y
    ETCharacter { etChar = '>', etCatCode = CCOther } -> return $ x > y
    _ -> throwError "unrecognized relation for \\ifdim"

ifoddCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifoddCommand = odd <$> readNumber

ifhmodeCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifhmodeCommand = uses mode isHMode

ifvmodeCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifvmodeCommand = uses mode isVMode

ifmmodeCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifmmodeCommand = uses mode isMMode

ifinnerCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifinnerCommand = uses mode isInnerMode

-- e-TeX extension: \ifdefined
ifdefinedCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifdefinedCommand = do
  t <- required nextEToken >>= meaningWithoutExpansion
  case toCommonValue <$> t of
    Right (Just (Undefined _)) -> return False
    _ -> return True

-- e-TeX extension: \ifcsname
ifcsnameCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifcsnameCommand = do
  name <- readUntilEndcsname []
  let tname = T.pack name
  d <- use (localState . controlSeqDef)
  return (Map.member tname d)

-- e-TeX extension: \unless
unlessCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
unlessCommand = do
  test <- required nextEToken >>= meaningWithoutExpansion
  case test of
    Left c | Just c <- evalBooleanConditional c -> expandBooleanConditional (not <$> c)
    _ -> throwError "\\unless must be followed by a boolean conditional command"
    -- You can't use `\\unless' before `XXX'.

-- e-TeX extension: \unexpanded<general text>
unexpandedCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
unexpandedCommand = do
  readFillerAndLBrace
  readUntilEndGroupE LongParam

-- LuaTeX extension: \Uchar
ucharCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
ucharCommand = do
  x <- readUnicodeScalarValue
  if x == ' '
    then return [ETCharacter { etDepth = 0, etChar = ' ', etCatCode = CCSpace }]
    else return [ETCharacter { etDepth = 0, etChar = x, etCatCode = CCOther }]

data CommonExpandable = Eexpandafter
                      | Enoexpand
                      | Ecsname
                      | Estring
                      | Enumber
                      | Eromannumeral
                      | Ethe
                      | Emeaning

                      -- e-TeX extension:
                      | Eunless
                      | Eunexpanded

                      -- LuaTeX extension:
                      | Ebegincsname
                      | Ecsstring
                      | EUchar

                      | Eifcase
                      deriving (Eq,Show)

instance IsExpandable CommonExpandable where
  isConditional e = e == Eifcase
  isIfCase e = e == Eifcase
  isConditionalMarker _ = Nothing

instance (Monad m, MonadTeXState s m, MonadError String m, Meaning (Expandable s), Meaning (Value s)) => DoExpand CommonExpandable m where
  doExpand Eexpandafter = expandafterCommand
  doExpand Enoexpand = noexpandCommand
  doExpand Ecsname = csnameCommand
  doExpand Estring = stringCommand
  doExpand Enumber = numberCommand
  doExpand Eromannumeral = romannumeralCommand
  doExpand Ethe = theCommand
  doExpand Emeaning = meaningCommand
  doExpand Eunless = unlessCommand
  doExpand Eunexpanded = unexpandedCommand
  doExpand Ebegincsname = begincsnameCommand
  doExpand Ecsstring = csstringCommand
  doExpand EUchar = ucharCommand
  doExpand Eifcase = ifcaseCommand
  doTotallyExpand _ Eunexpanded = Just unexpandedCommand -- \unexpanded in \edef
  doTotallyExpand _ _ = Nothing
  evalBooleanConditional _ = Nothing

instance Meaning CommonExpandable where
  meaningString Eexpandafter = controlSequence "expandafter"
  meaningString Enoexpand = controlSequence "noexpand"
  meaningString Ecsname = controlSequence "csname"
  meaningString Estring = controlSequence "string"
  meaningString Enumber = controlSequence "number"
  meaningString Eromannumeral = controlSequence "romannumeral"
  meaningString Ethe = controlSequence "the"
  meaningString Emeaning = controlSequence "meaning"
  meaningString Eunless = controlSequence "unless"
  meaningString Eunexpanded = controlSequence "unexpanded"
  meaningString Ebegincsname = controlSequence "begincsname"
  meaningString Ecsstring = controlSequence "csstring"
  meaningString EUchar = controlSequence "Uchar"
  meaningString Eifcase = controlSequence "ifcase"

data CommonBoolean = Eiftrue
                   | Eiffalse
                   | Eif
                   | Eifcat
                   | Eifx
                   | Eifnum
                   | Eifdim
                   | Eifodd
                   | Eifhmode
                   | Eifvmode
                   | Eifmmode
                   | Eifinner

                   -- e-TeX extension:
                   | Eifdefined
                   | Eifcsname
                   deriving (Eq,Show)

instance IsExpandable CommonBoolean where
  isConditional _ = True
  isIfCase _ = False
  isConditionalMarker _ = Nothing

instance Meaning CommonBoolean where
  meaningString Eiftrue = controlSequence "iftrue"
  meaningString Eiffalse = controlSequence "iffalse"
  meaningString Eif = controlSequence "if"
  meaningString Eifcat = controlSequence "ifcat"
  meaningString Eifx = controlSequence "ifx"
  meaningString Eifnum = controlSequence "ifnum"
  meaningString Eifdim = controlSequence "ifdim"
  meaningString Eifodd = controlSequence "ifodd"
  meaningString Eifhmode = controlSequence "ifhmode"
  meaningString Eifvmode = controlSequence "ifvmode"
  meaningString Eifmmode = controlSequence "ifmmode"
  meaningString Eifinner = controlSequence "ifinner"
  meaningString Eifdefined = controlSequence "ifdefined"
  meaningString Eifcsname = controlSequence "ifcsname"

evalCommonBoolean :: (MonadTeXState s m, MonadError String m) => CommonBoolean -> m Bool
evalCommonBoolean Eiftrue = iftrueCommand
evalCommonBoolean Eiffalse = iffalseCommand
evalCommonBoolean Eif = ifCommand
evalCommonBoolean Eifcat = ifcatCommand
evalCommonBoolean Eifx = ifxCommand
evalCommonBoolean Eifnum = ifnumCommand
evalCommonBoolean Eifdim = ifdimCommand
evalCommonBoolean Eifodd = ifoddCommand
evalCommonBoolean Eifhmode = ifhmodeCommand
evalCommonBoolean Eifvmode = ifvmodeCommand
evalCommonBoolean Eifmmode = ifmmodeCommand
evalCommonBoolean Eifinner = ifinnerCommand
evalCommonBoolean Eifdefined = ifdefinedCommand
evalCommonBoolean Eifcsname = ifcsnameCommand

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExpand CommonBoolean m where
  doExpand e = expandBooleanConditional (evalCommonBoolean e)
  evalBooleanConditional e = Just (evalCommonBoolean e)

expandableDefinitions :: SubList '[ConditionalMarkerCommand, CommonExpandable, CommonBoolean] set => Map.Map Text (Union set)
expandableDefinitions = Map.fromList
  [("expandafter", liftUnion Eexpandafter)
  ,("noexpand",    liftUnion Enoexpand)
  ,("csname",      liftUnion Ecsname)
  ,("string",      liftUnion Estring)
  ,("number",      liftUnion Enumber)
  ,("romannumeral",liftUnion Eromannumeral)
  ,("the",         liftUnion Ethe)
  ,("meaning",     liftUnion Emeaning)
  ,("ifcase",      liftUnion Eifcase)

  -- conditional markers
  ,("else",        liftUnion (ConditionalMarkerCommand Eelse))
  ,("fi",          liftUnion (ConditionalMarkerCommand Efi))
  ,("or",          liftUnion (ConditionalMarkerCommand Eor))

  -- boolean conditional commands
  ,("iftrue",      liftUnion Eiftrue)
  ,("iffalse",     liftUnion Eiffalse)
  ,("if",          liftUnion Eif)
  ,("ifcat",       liftUnion Eifcat)
  ,("ifx",         liftUnion Eifx)
  ,("ifnum",       liftUnion Eifnum)
  ,("ifdim",       liftUnion Eifdim)
  ,("ifodd",       liftUnion Eifodd)
  ,("ifhmode",     liftUnion Eifhmode)
  ,("ifvmode",     liftUnion Eifvmode)
  ,("ifmmode",     liftUnion Eifmmode)
  ,("ifinner",     liftUnion Eifinner)

  -- e-TeX extension:
  ,("ifdefined",   liftUnion Eifdefined)
  ,("ifcsname",    liftUnion Eifcsname)

  ,("unless",      liftUnion Eunless)
  ,("unexpanded",  liftUnion Eunexpanded)

  -- LuaTeX extension:
  ,("begincsname", liftUnion Ebegincsname)
  ,("csstring",    liftUnion Ecsstring)
  ,("Uchar",       liftUnion EUchar)
  ]
-- \endcsname is not included here

-- other expandable primitives:
--   \ifdim
--   \ifeof
--   \ifhbox
--   \ifvbox
--   \ifvoid
--   \input
--   \jobname
-- e-TeX:
--   \detokenize
--   \scantokens
-- pdfTeX:
--   \ifincsname
--   \expanded
-- LuaTeX:
--   \lastnamedcs
-- LaTeX
--   \arabic
--   \@arabic
--   \Roman
--   \roman
--   \Alph
--   \alph
