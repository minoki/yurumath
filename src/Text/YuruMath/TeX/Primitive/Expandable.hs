{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.YuruMath.TeX.Primitive.Expandable
  (expandableDefinitions
  ,ConditionalMarkerCommand
  ,CommonExpandable
  ,CommonBoolean
  ) where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Meaning
import Text.YuruMath.TeX.Expansion
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.Error.Class
import Control.Lens.Getter (use,view,views)
import Control.Lens.Setter (assign,modifying)
import Data.OpenUnion
import TypeFun.Data.List (SubList)

expandafterCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
expandafterCommand = do
  t1 <- required nextUnexpandedToken
  t2 <- required nextUnexpandedToken
  (t1:) <$> expandOnce t2

noexpandCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
noexpandCommand = do
  t <- required nextUnexpandedToken
  case t of
    ETCommandName { etFlavor = ECNFPlain, etName = name } -> do
      m <- use (localState . definitionAt name)
      return $ case m of
        Nothing        -> [t { etFlavor = ECNFNoexpand }] -- undefined
        Just (Left _)  -> [t { etFlavor = ECNFNoexpand }] -- expandable
        Just (Right _) -> [t] -- not expandable
    _ -> return [t]

csnameCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
csnameCommand = do
  name <- readUntilEndcsname
  let tname = T.pack name

  -- THE DREADED SIDE EFFECT OF \csname
  d <- use (localState . controlSeqDef)
  when (Map.notMember tname d)
    $ modifying (localState . controlSeqDef) (Map.insert tname (Right (injectCommonValue Relax)))

  return [ETCommandName { etDepth = 0, etFlavor = ECNFPlain, etName = NControlSeq tname }]

-- LuaTeX extension: \begincsname
begincsnameCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
begincsnameCommand = do
  name <- readUntilEndcsname
  let tname = T.pack name
  return [ETCommandName { etDepth = 0, etFlavor = ECNFPlain, etName = NControlSeq tname }]

stringCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
stringCommand = do
  t <- required nextUnexpandedToken
  stringToEToken <$> case t of
    ETCommandName { etName = name } ->
      showMessageStringM $ showCommandName name -- without a space appended
    ETCharacter { etChar = c } ->
      return [c]

-- LuaTeX extension: \csstring
csstringCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
csstringCommand = do
  t <- required nextUnexpandedToken
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
theCommand = map toEToken <$> theString "\\the"

meaningCommand :: (MonadTeXState s m, MonadError String m, Meaning (Expandable s), Meaning (NValue s)) => m [ExpansionToken]
meaningCommand = do
  value <- required nextUnexpandedToken >>= meaningWithoutExpansion
  stringToEToken <$> showMessageStringM (meaningString value)

romannumeralCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
romannumeralCommand = do
  -- Should guard against big input?
  stringToEToken . showRomannumeral <$> readGeneralInt

insertedRelax :: ExpansionToken
insertedRelax = ETCommandName { etDepth = 0, etFlavor = ECNFIsRelax, etName = NControlSeq "relax" }

elseCommand :: (MonadTeXState s m, MonadError String m) => ExpansionToken -> m [ExpansionToken]
elseCommand self = do
  cs <- use conditionalStack
  case cs of
    CondTruthy:css -> do
      -- \iftrue ... >>>\else<<< ... \fi
      skipUntilFi 0
      assign conditionalStack css
      return []
    CondCase:css -> do
      -- \ifcase ... \or ... >>>\else<<< ... \fi
      skipUntilFi 0
      assign conditionalStack css
      return []
    CondTest:_ -> do
      -- \else in a conditional test: insert a \relax
      -- For example, single-step expansion of '\ifodd1\else\fi' yields '\relax \else \fi '
      return [insertedRelax,self]
    _ -> throwError "Extra \\else"

fiCommand :: (MonadTeXState s m, MonadError String m) => ExpansionToken -> m [ExpansionToken]
fiCommand self = do
  cs <- use conditionalStack
  case cs of
    [] -> throwError "Extra \\fi"
    CondTest:_ -> do
      -- \fi in a conditional test: insert a \relax
      -- For example, single-step expansion of '\ifodd1\fi' yields '\relax \fi '
      return [insertedRelax,self]
    _:css -> do
      -- \iftrue ... >>>\fi<<<
      -- OR
      -- \iffalse ... \else ... >>>\fi<<<
      assign conditionalStack css
      return []

orCommand :: (MonadTeXState s m, MonadError String m) => ExpansionToken -> m [ExpansionToken]
orCommand self = do
  cs <- use conditionalStack
  case cs of
    CondCase:css -> do
      -- \ifcase N ... >>>\or<<< ... \fi
      skipUntilFi 0
      assign conditionalStack css
      return []
    CondTest:_ -> do
      -- \or in a conditional test: insert a \relax
      -- For example, single-step expansion of '\ifcase0\or\fi' yields '\relax \or \fi '
      return [insertedRelax,self]
    _ -> throwError "Extra \\or"

newtype ConditionalMarkerCommand = ConditionalMarkerCommand ConditionalMarker
  deriving (Eq,Enum,Bounded)

instance Show ConditionalMarkerCommand where
  show (ConditionalMarkerCommand c) = case c of
    Eelse -> "\\else"
    Efi -> "\\fi"
    Eor -> "\\or"

instance IsPrimitive ConditionalMarkerCommand where
  primitiveName (ConditionalMarkerCommand c) = case c of
    Eelse -> "else"
    Eor -> "or"
    Efi -> "fi"

instance Meaning ConditionalMarkerCommand

instance IsExpandable ConditionalMarkerCommand where
  isConditional _     = False
  isConditionalMarker (ConditionalMarkerCommand x) = Just x

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExpand ConditionalMarkerCommand m where
  doExpand (ConditionalMarkerCommand m) = case m of
    Eelse -> elseCommand
    Efi   -> fiCommand
    Eor   -> orCommand
  evalBooleanConditional _ = Nothing

doIfCase :: (MonadTeXState s m, MonadError String m) => Integer -> m ()
doIfCase 0 = do
  modifying conditionalStack (CondCase:)
doIfCase n = do
  k <- skipUntilOr 0
  case k of
    FoundFi -> return ()
    FoundElse -> modifying conditionalStack (CondFalsy:)
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

getCharCodeAndCatCode :: (IsNValue v) => (ExpansionToken,v) -> Maybe (Char,CatCode)
getCharCodeAndCatCode p = case p of
  (_,v) | Just (Character c cc) <- toCommonValue v -> Just (c,cc) -- explicit or implicit character token
  (ETCommandName { etName = NActiveChar c, etFlavor = ECNFNoexpand },_) -> Just (c,CCActive) -- \noexpand-ed active character
  _ -> Nothing

-- \if: test character codes
ifCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifCommand = do
  t1 <- required nextExpandedToken
  t2 <- required nextExpandedToken
  return $ fmap fst (getCharCodeAndCatCode t1) == fmap fst (getCharCodeAndCatCode t2)

-- \ifcat: test category codes
ifcatCommand :: (MonadTeXState a m, MonadError String m) => m Bool
ifcatCommand = do
  t1 <- required nextExpandedToken
  t2 <- required nextExpandedToken
  return $ fmap snd (getCharCodeAndCatCode t1) == fmap snd (getCharCodeAndCatCode t2)

ifxCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifxCommand = do
  t1 <- required nextUnexpandedToken >>= meaningWithoutExpansion
  t2 <- required nextUnexpandedToken >>= meaningWithoutExpansion
  return $ t1 == t2

ifnumCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifnumCommand = do
  x <- readNumber
  readOptionalSpaces
  (rel,_) <- required nextExpandedToken
  y <- readNumber
  case rel of
    ETCharacter { etChar = '<', etCatCode = CCOther } -> return $ x < y
    ETCharacter { etChar = '=', etCatCode = CCOther } -> return $ x == y
    ETCharacter { etChar = '>', etCatCode = CCOther } -> return $ x > y
    _ -> throwError "unrecognized relation for \\ifnum"

ifabsnumCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifabsnumCommand = do
  x <- abs <$> readNumber
  readOptionalSpaces
  (rel,_) <- required nextExpandedToken
  y <- abs <$> readNumber
  case rel of
    ETCharacter { etChar = '<', etCatCode = CCOther } -> return $ x < y
    ETCharacter { etChar = '=', etCatCode = CCOther } -> return $ x == y
    ETCharacter { etChar = '>', etCatCode = CCOther } -> return $ x > y
    _ -> throwError "unrecognized relation for \\ifabsnum"

ifdimCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifdimCommand = do
  x <- readDimension
  readOptionalSpaces
  (rel,_) <- required nextExpandedToken
  y <- readDimension
  case rel of
    ETCharacter { etChar = '<', etCatCode = CCOther } -> return $ x < y
    ETCharacter { etChar = '=', etCatCode = CCOther } -> return $ x == y
    ETCharacter { etChar = '>', etCatCode = CCOther } -> return $ x > y
    _ -> throwError "unrecognized relation for \\ifdim"

ifoddCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifoddCommand = odd <$> readNumber

ifhmodeCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifhmodeCommand = views mode isHMode

ifvmodeCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifvmodeCommand = views mode isVMode

ifmmodeCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifmmodeCommand = views mode isMMode

ifinnerCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifinnerCommand = views mode isInnerMode

-- e-TeX extension: \ifdefined
ifdefinedCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifdefinedCommand = do
  t <- required nextUnexpandedToken >>= meaningWithoutExpansion
  return $ t /= Nothing

-- e-TeX extension: \ifcsname
ifcsnameCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifcsnameCommand = do
  name <- readUntilEndcsname
  let tname = T.pack name
  d <- use (localState . controlSeqDef)
  return (Map.member tname d)

-- e-TeX extension: \unless
unlessCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
unlessCommand = do
  test <- required nextUnexpandedToken >>= meaningWithoutExpansion
  case test of
    Just (Left c) | Just c <- evalBooleanConditional c -> expandBooleanConditional (not <$> c)
    _ -> throwError "\\unless must be followed by a boolean conditional command"
    -- You can't use `\\unless' before `XXX'.

-- e-TeX extension: \unexpanded<general text>
unexpandedCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
unexpandedCommand = readUnexpandedGeneralTextE

-- LuaTeX extension: \expanded<general text>
expandedCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
expandedCommand = readExpandedGeneralTextE

-- e-TeX extension: \detokenize<general text>
detokenizeCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
detokenizeCommand = do
  content <- readUnexpandedGeneralText
  stringToEToken <$> showMessageStringM (mconcat $ map showToken content)

-- e-TeX extension: \scantokens<general text>
scantokensCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
scantokensCommand = do
  content <- readUnexpandedGeneralText
  inputText <- showMessageStringM (mconcat $ map showToken content)
  let ts = TokenizerState { tsInput = inputText
                          , tsSpacingState = SSNewLine
                          }
      is = InputState { _inputTokenizerState = ts
                      , _inputPendingTokenList = []
                      }
  modifying inputStateStack (is :)
  return []

-- LuaTeX extension: \Uchar
ucharCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
ucharCommand = do
  x <- readUnicodeScalarValue
  if x == ' '
    then return [ETCharacter { etDepth = 0, etChar = ' ', etCatCode = CCSpace }]
    else return [ETCharacter { etDepth = 0, etChar = x, etCatCode = CCOther }]

-- pdfTeX extension: \pdfstrcmp (\strcmp in XeTeX)
-- \pdfstrcmp<general text><general text>
strcmpCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
strcmpCommand = do
  lhs <- readExpandedGeneralText
  rhs <- readExpandedGeneralText
  lhsS <- showMessageStringM (mconcat $ map showToken lhs)
  rhsS <- showMessageStringM (mconcat $ map showToken rhs)
  return $ case compare lhsS rhsS of
             LT -> stringToEToken "-1"
             EQ -> stringToEToken "0"
             GT -> stringToEToken "1"

ifincsnameCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifincsnameCommand = view isInCsname

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
                      | Edetokenize
                      | Escantokens

                      -- pdfTeX extension
                      | Estrcmp
                      | Eexpanded

                      -- LuaTeX extension:
                      | Ebegincsname
                      | Ecsstring
                      | EUchar

                      | Eifcase
                      deriving (Eq,Show,Enum,Bounded)

instance IsExpandable CommonExpandable where
  isConditional e = e == Eifcase
  isConditionalMarker _ = Nothing

instance (Monad m, MonadTeXState s m, MonadError String m, Meaning (Expandable s), Meaning (NValue s)) => DoExpand CommonExpandable m where
  doExpand cmd _ = case cmd of
    Eexpandafter -> expandafterCommand
    Enoexpand -> noexpandCommand
    Ecsname -> csnameCommand
    Estring -> stringCommand
    Enumber -> numberCommand
    Eromannumeral -> romannumeralCommand
    Ethe -> theCommand
    Emeaning -> meaningCommand
    Eunless -> unlessCommand
    Eunexpanded -> unexpandedCommand
    Edetokenize -> detokenizeCommand
    Escantokens -> scantokensCommand
    Estrcmp -> strcmpCommand
    Eexpanded -> expandedCommand
    Ebegincsname -> begincsnameCommand
    Ecsstring -> csstringCommand
    EUchar -> ucharCommand
    Eifcase -> ifcaseCommand
  doExpandInEdef Eunexpanded = Just (\_ -> unexpandedCommand) -- \unexpanded in \edef
  doExpandInEdef Ethe = Just (\_ -> theCommand)
  doExpandInEdef _ = Nothing
  evalBooleanConditional _ = Nothing

instance IsPrimitive CommonExpandable where
  primitiveName Eexpandafter = "expandafter"
  primitiveName Enoexpand = "noexpand"
  primitiveName Ecsname = "csname"
  primitiveName Estring = "string"
  primitiveName Enumber = "number"
  primitiveName Eromannumeral = "romannumeral"
  primitiveName Ethe = "the"
  primitiveName Emeaning = "meaning"
  primitiveName Eunless = "unless"
  primitiveName Eunexpanded = "unexpanded"
  primitiveName Edetokenize = "detokenize"
  primitiveName Escantokens = "scantokens"
  primitiveName Estrcmp = "strcmp" -- \strcmp rather than \pdfstrcmp
  primitiveName Eexpanded = "expanded"
  primitiveName Ebegincsname = "begincsname"
  primitiveName Ecsstring = "csstring"
  primitiveName EUchar = "Uchar"
  primitiveName Eifcase = "ifcase"

instance Meaning CommonExpandable

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

                   -- pdfTeX extension:
                   | Eifabsnum
                   | Eifincsname
                   deriving (Eq,Show,Enum,Bounded)

instance IsExpandable CommonBoolean where
  isConditional _ = True
  isConditionalMarker _ = Nothing

instance IsPrimitive CommonBoolean where
  primitiveName Eiftrue = "iftrue"
  primitiveName Eiffalse = "iffalse"
  primitiveName Eif = "if"
  primitiveName Eifcat = "ifcat"
  primitiveName Eifx = "ifx"
  primitiveName Eifnum = "ifnum"
  primitiveName Eifdim = "ifdim"
  primitiveName Eifodd = "ifodd"
  primitiveName Eifhmode = "ifhmode"
  primitiveName Eifvmode = "ifvmode"
  primitiveName Eifmmode = "ifmmode"
  primitiveName Eifinner = "ifinner"
  primitiveName Eifdefined = "ifdefined"
  primitiveName Eifcsname = "ifcsname"
  primitiveName Eifabsnum = "ifabsnum"
  primitiveName Eifincsname = "ifincsname"

instance Meaning CommonBoolean

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
evalCommonBoolean Eifabsnum = ifabsnumCommand
evalCommonBoolean Eifincsname = ifincsnameCommand

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExpand CommonBoolean m where
  doExpand e _ = expandBooleanConditional (evalCommonBoolean e)
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
  ,("detokenize",  liftUnion Edetokenize)
  ,("scantokens",  liftUnion Escantokens)

  -- pdfTeX extension:
  ,("pdfstrcmp",   liftUnion Estrcmp) -- pdfTeX name
  ,("strcmp",      liftUnion Estrcmp) -- XeTeX name
  ,("expanded",    liftUnion Eexpanded)
  ,("ifabsnum",    liftUnion Eifabsnum) -- LuaTeX name (\ifpdfabsnum in pdfTeX)
  ,("ifincsname",  liftUnion Eifincsname)

  -- LuaTeX extension:
  ,("begincsname", liftUnion Ebegincsname)
  ,("csstring",    liftUnion Ecsstring)
  ,("Uchar",       liftUnion EUchar)
  ]
-- \endcsname is not included here

-- other expandable primitives:
--   \ifeof
--   \ifhbox
--   \ifvbox
--   \ifvoid
--   \input
--   \jobname
-- pdfTeX:
--   \ifpdfabsdim (\ifabsdim in LuaTeX)
--   \pdfuniformdeviate (without pdf prefix in LuaTeX)
--   \pdfnormaldeviate (without pdf prefix in LuaTeX)
--   (\pdfrandomseed / \pdfsetrandomseed: non-expandable) (without pdf prefix in LuaTeX)
--   \ifpdfprimitive (\ifprimitive in XeTeX, LuaTeX)
--   \pdfprimitive (\primitive in XeTeX, LuaTeX)
-- LuaTeX:
--   \lastnamedcs
-- LaTeX
--   \arabic
--   \@arabic
--   \Roman
--   \roman
--   \Alph
--   \alph
