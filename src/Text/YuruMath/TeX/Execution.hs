{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.YuruMath.TeX.Execution
  (CountReg
  ,DimenReg
  ,SkipReg
  ,MuskipReg
  ,ToksReg
  ,readRegIndex
  ,Assignment
  ,runLocal
  ,runGlobal
  ,texAssign
  ,arithmeticInteger
  ,arithmeticQuantity
  ,CommonExecutable(..)
  ,executableDefinitions
  ) where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Meaning
import Text.YuruMath.TeX.Quantity
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Expansion
import Data.String
import Data.Text (Text)
import Data.Monoid
import qualified Data.Map.Strict as Map
import Control.Monad.Error.Class
import Control.Lens.Lens (Lens')
import Control.Lens.At (at)
import Control.Lens.Iso (non)
import Control.Lens.Getter (use,uses)
import Control.Lens.Setter (assign,mapped,ASetter)
import Data.OpenUnion
import TypeFun.Data.List (SubList,Elem)

-- \countdef-ed value
newtype CountReg = CountReg Int
  deriving (Eq,Show)

countRegAt :: (IsLocalState localstate) => Int -> Lens' localstate Integer
countRegAt !index = countReg . at index . non 0

-- Used by \count, \countdef, etc
-- 8-bit (0-255) on the original TeX, 15-bit (0-32767) on e-TeX, and 16-bit (0-65535) on LuaTeX
readRegIndex :: (MonadTeXState s m, MonadError String m) => m Int
readRegIndex = do
  x <- readIntBetween minBound maxBound
  if x < 0 || 65536 <= x
    then throwError $ "Bad register code (" ++ show x ++ ")"
    else return x

-- \dimendef-ed value
newtype DimenReg = DimenReg Int
  deriving (Eq,Show)

dimenRegAt :: (IsLocalState localstate) => Int -> Lens' localstate Dimen
dimenRegAt !index = dimenReg . at index . non zeroQ

-- \skipdef-ed value
newtype SkipReg = SkipReg Int
  deriving (Eq,Show)

skipRegAt :: (IsLocalState localstate) => Int -> Lens' localstate (Glue Dimen)
skipRegAt !index = skipReg . at index . non zeroQ

-- \muskipdef-ed value
newtype MuskipReg = MuskipReg Int
  deriving (Eq,Show)

muskipRegAt :: (IsLocalState localstate) => Int -> Lens' localstate (Glue MuDimen)
muskipRegAt !index = muskipReg . at index . non zeroQ

-- \toksdef-ed value
newtype ToksReg = ToksReg Int
  deriving (Eq,Show)

toksRegAt :: (IsLocalState localstate) => Int -> Lens' localstate [TeXToken]
toksRegAt !index = toksReg . at index . non []

data Assignment s where
  WillAssign :: ASetter (LocalState s) (LocalState s) b b -> !b -> Assignment s

runLocal, runGlobal :: (MonadTeXState s m) => m (Assignment s) -> m ()
runLocal m = do
  WillAssign setter value <- m
  assign (localState . setter) value
runGlobal m = do
  WillAssign setter value <- m
  assign (localStates . mapped . setter) value

texAssign :: (MonadTeXState s m) => ASetter (LocalState s) (LocalState s) b b -> b -> m (Assignment s)
texAssign setter !value = return (WillAssign setter value)

globalCommand :: (MonadTeXState s m, MonadError String m, Meaning (NValue s)) => m ()
globalCommand = do
  (_,v) <- required nextExpandedToken
  case toCommonValue v of
    Just Relax -> globalCommand -- ignore \relax
    Just (Character _ CCSpace) -> globalCommand -- ignore spaces
    _ -> case doGlobal v of
           Just m -> m
           Nothing -> invalidPrefix "global" v

letCommand :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
letCommand = do
  name <- readCommandName
  readUnexpandedEquals
  readUnexpandedOneOptionalSpace
  v <- required nextUnexpandedToken >>= meaningWithoutExpansion
  texAssign (definitionAt name) v

futureletCommand :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
futureletCommand = do
  name <- readCommandName
  t1 <- required nextUnexpandedToken
  t2 <- required nextUnexpandedToken
  unreadTokens' [t1,t2]
  v <- meaningWithoutExpansion t2
  texAssign (definitionAt name) v

mapChar :: (Char -> Char) -> ExpansionToken -> ExpansionToken
mapChar f t@(ETCharacter { etChar = c })
  | let d = f c, d /= '\0' = t { etChar = d }
mapChar f t@(ETCommandName { etName = NActiveChar c })
  | let d = f c, d /= '\0' = t { etFlavor = ECNFPlain, etName = NActiveChar d } -- strip 'isrelax' flag or 'noexpand' flag
mapChar _ t@(ETCommandName { etFlavor = ECNFIsRelax })
  = t { etFlavor = ECNFPlain } -- strip 'isrelax' flag
mapChar _ t@(ETCommandName { etFlavor = ECNFNoexpand })
  = t { etFlavor = ECNFPlain } -- strip 'noexpand' flag
mapChar _ t = t

uppercaseCommand :: (MonadTeXState s m, MonadError String m) => m ()
uppercaseCommand = do
  text <- readUnexpandedGeneralTextE
  toUpper <- ucCodeFn
  unreadTokens' (map (mapChar toUpper) text)

lowercaseCommand :: (MonadTeXState s m, MonadError String m) => m ()
lowercaseCommand = do
  text <- readUnexpandedGeneralTextE
  toLower <- lcCodeFn
  unreadTokens' (map (mapChar toLower) text)

ignorespacesCommand :: (MonadTeXState s m, MonadError String m) => m ()
ignorespacesCommand = do
  readOptionalSpaces

-- \chardef<control sequence><equals><number>
chardefCommand :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
chardefCommand = do
  name <- readCommandName
  readEquals
  c <- readUnicodeScalarValue
  let w = DefinedCharacter c
  texAssign (definitionAt name) (nonexpandableToValue w)

-- \mathchardef<control sequence><equals><15-bit number>
mathchardefCommand :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
mathchardefCommand = do
  name <- readCommandName
  readEquals
  v <- readIntBetween 0 0x8000 -- "Bad math code (" ++ show v ++ ")"
  let w = DefinedMathCharacter (MathCode (fromIntegral v))
  texAssign (definitionAt name) (nonexpandableToValue w)

-- \Umathchardef<control sequence><equals><3-bit number><8-bit number><21-bit number>
umathchardefCommand :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
umathchardefCommand = do
  name <- readCommandName
  readEquals
  w <- readUMathCodeTriplet
  texAssign (definitionAt name) (nonexpandableToValue $ DefinedMathCharacter w)

-- \Umathcharnumdef<control sequence><equals><32-bit number>
umathcharnumdefCommand :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
umathcharnumdefCommand = do
  name <- readCommandName
  readEquals
  w <- readUMathCode32
  texAssign (definitionAt name) (nonexpandableToValue $ DefinedMathCharacter w)

catcodeGet :: (MonadTeXState s m, MonadError String m) => m Integer
catcodeGet = do
  slot <- readUnicodeScalarValue
  (fromIntegral . fromEnum) <$> categoryCodeOf slot

-- \catcode<21-bit number><equals><4-bit number>
catcodeSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
catcodeSet = do
  slot <- readUnicodeScalarValue
  readEquals
  v <- readIntBetween 0 15 -- "Invalid code (" ++ show v ++ "), should be in the range 0..15."
  let w = toEnum v
  texAssign (catcodeMap . at slot) (Just w)

lccodeGet :: (MonadTeXState s m, MonadError String m) => m Integer
lccodeGet = do
  slot <- readUnicodeScalarValue
  (fromIntegral . fromEnum) <$> lcCodeOf slot

-- \lccode<21-bit number><equals><21-bit number>
lccodeSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
lccodeSet = do
  slot <- readUnicodeScalarValue
  readEquals
  v <- readUnicodeScalarValue
  texAssign (lccodeMap . at slot) (Just v)

uccodeGet :: (MonadTeXState s m, MonadError String m) => m Integer
uccodeGet = do
  slot <- readUnicodeScalarValue
  (fromIntegral . fromEnum) <$> ucCodeOf slot

-- \uccode<21-bit number><equals><21-bit number>
uccodeSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
uccodeSet = do
  slot <- readUnicodeScalarValue
  readEquals
  v <- readUnicodeScalarValue
  texAssign (uccodeMap . at slot) (Just v)

mathcodeGet :: (MonadTeXState s m, MonadError String m) => m Integer
mathcodeGet = do
  slot <- readUnicodeScalarValue
  mathcodeToInt <$> mathCodeOf slot
  where
    mathcodeToInt (MathCode x) = fromIntegral x
    mathcodeToInt (UMathCode x) = fromIntegral x

-- \mathcode<21-bit number><equals><15-bit number>
mathcodeSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
mathcodeSet = do
  slot <- readUnicodeScalarValue
  readEquals
  v <- readIntBetween 0 0x8000 -- "Bad math code (" ++ show v ++ ")"
  let w = MathCode (fromIntegral v)
  texAssign (mathcodeMap . at slot) (Just w)

-- \Umathcode<21-bit number><equals><3-bit number><8-bit number><21-bit number>
umathcodeSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
umathcodeSet = do
  slot <- readUnicodeScalarValue
  readEquals
  w <- readUMathCodeTriplet
  texAssign (mathcodeMap . at slot) (Just w)

-- \Umathcodenum<21-bit number><equals><32-bit number>
umathcodenumSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
umathcodenumSet = do
  slot <- readUnicodeScalarValue
  readEquals
  w <- readUMathCode32
  texAssign (mathcodeMap . at slot) (Just w)

delcodeGet :: (MonadTeXState s m, MonadError String m) => m Integer
delcodeGet = do
  slot <- readUnicodeScalarValue
  delcodeToInt <$> delimiterCodeOf slot
  where
    delcodeToInt (DelimiterCode x) = fromIntegral x
    delcodeToInt (UDelimiterCode x) = fromIntegral x

-- \delcode<21-bit number><equals><24-bit number>
delcodeSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
delcodeSet = do
  slot <- readUnicodeScalarValue
  readEquals
  v <- readIntBetween (-1) 0xFFFFFF -- "Invalid delimiter code."
  let w = DelimiterCode (fromIntegral v)
  texAssign (delcodeMap . at slot) (Just w)

-- \Udelcode<21-bit number><equals><8-bit number><21-bit number>
udelcodeSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
udelcodeSet = do
  slot <- readUnicodeScalarValue
  readEquals
  w <- readUDelimiterCodePair
  texAssign (delcodeMap . at slot) (Just w)

-- \Udelcodenum<21-bit number><equals><32-bit number>
udelcodenumSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
udelcodenumSet = do
  slot <- readUnicodeScalarValue
  readEquals
  w <- readUDelimiterCode32
  texAssign (delcodeMap . at slot) (Just w)

endlinecharGet :: (MonadTeXState s m, MonadError String m) => m Integer
endlinecharGet = do
  uses (localState . endlinechar) fromIntegral

endlinecharSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
endlinecharSet = do
  readEquals
  value <- readIntBetween minBound maxBound
  texAssign endlinechar value

escapecharGet :: (MonadTeXState s m, MonadError String m) => m Integer
escapecharGet = do
  uses (localState . escapechar) fromIntegral

escapecharSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
escapecharSet = do
  readEquals
  value <- readIntBetween minBound maxBound
  texAssign escapechar value

begingroupCommand :: (MonadTeXState s m, MonadError String m) => m ()
begingroupCommand = do
  enterGroup ScopeByBeginGroup

endgroupCommand :: (MonadTeXState s m, MonadError String m) => m ()
endgroupCommand = do
  leaveGroup ScopeByBeginGroup

-- \countdef-ed token
setCountReg :: (MonadTeXState s m, MonadError String m) => Int -> m (Assignment s)
setCountReg !index = do
  readEquals
  value <- readNumber
  texAssign (countRegAt index) value

-- \count command
countSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
countSet = readRegIndex >>= setCountReg

countGet :: (MonadTeXState s m, MonadError String m) => m Integer
countGet = do
  index <- readRegIndex
  use (localState . countRegAt index)

countdefCommand :: (MonadTeXState s m, MonadError String m, Elem CountReg (NValueSet s)) => m (Assignment s)
countdefCommand = do
  name <- readCommandName
  readEquals
  index <- readRegIndex
  texAssign (definitionAt name) (nonexpandableToValue $ CountReg index)

-- \dimendef-ed token
setDimenReg :: (MonadTeXState s m, MonadError String m) => Int -> m (Assignment s)
setDimenReg !index = do
  readEquals
  value <- readDimension
  texAssign (dimenRegAt index) value

-- \dimen command
dimenSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
dimenSet = readRegIndex >>= setDimenReg

dimenGet :: (MonadTeXState s m, MonadError String m) => m Dimen
dimenGet = do
  index <- readRegIndex
  use (localState . dimenRegAt index)

dimendefCommand :: (MonadTeXState s m, MonadError String m, Elem DimenReg (NValueSet s)) => m (Assignment s)
dimendefCommand = do
  name <- readCommandName
  readEquals
  index <- readRegIndex
  texAssign (definitionAt name) (nonexpandableToValue $ DimenReg index)

-- \skipdef-ed token
setSkipReg :: (MonadTeXState s m, MonadError String m) => Int -> m (Assignment s)
setSkipReg !index = do
  readEquals
  value <- readGlue
  texAssign (skipRegAt index) value

-- \skip command
skipSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
skipSet = readRegIndex >>= setSkipReg

skipGet :: (MonadTeXState s m, MonadError String m) => m (Glue Dimen)
skipGet = do
  index <- readRegIndex
  use (localState . skipRegAt index)

skipdefCommand :: (MonadTeXState s m, MonadError String m, Elem SkipReg (NValueSet s)) => m (Assignment s)
skipdefCommand = do
  name <- readCommandName
  readEquals
  index <- readRegIndex
  texAssign (definitionAt name) (nonexpandableToValue $ SkipReg index)

-- \muskipdef-ed token
setMuskipReg :: (MonadTeXState s m, MonadError String m) => Int -> m (Assignment s)
setMuskipReg !index = do
  readEquals
  value <- readMuGlue
  texAssign (muskipRegAt index) value

-- \muskip command
muskipSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
muskipSet = readRegIndex >>= setMuskipReg

muskipGet :: (MonadTeXState s m, MonadError String m) => m (Glue MuDimen)
muskipGet = do
  index <- readRegIndex
  use (localState . muskipRegAt index)

muskipdefCommand :: (MonadTeXState s m, MonadError String m, Elem MuskipReg (NValueSet s)) => m (Assignment s)
muskipdefCommand = do
  name <- readCommandName
  readEquals
  index <- readRegIndex
  texAssign (definitionAt name) (nonexpandableToValue $ MuskipReg index)

-- \toksdef-ed token
-- <token variable><equals><general text>
-- or <token variable><equals><filler><token variable>
setToksReg :: forall s m. (MonadTeXState s m, MonadError String m) => Int -> m (Assignment s)
setToksReg !index = do
  readEquals
  value <- doReadTokenList
  texAssign (toksRegAt index) value
  where
    doReadTokenList :: m [TeXToken]
    doReadTokenList = do
      (_,v) <- required nextExpandedToken
      case toCommonValue v of
        Just (Character _ CCBeginGroup)
          -> map fromEToken <$> readUntilEndGroupE LongParam -- <general text>
        Just (Character _ CCSpace) -> doReadTokenList -- optional spaces: ignoed
        Just Relax -> doReadTokenList -- \relax: ignored
        _ | QToks getTokenList <- getQuantity v -> do
              getTokenList
          | otherwise -> throwError "Unexpected token while reading token list" -- Missing { inserted.

-- \toks command
toksSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
toksSet = readRegIndex >>= setToksReg

toksGet :: (MonadTeXState s m, MonadError String m) => m [TeXToken]
toksGet = do
  index <- readRegIndex
  use (localState . toksRegAt index)

toksdefCommand :: (MonadTeXState s m, MonadError String m, Elem ToksReg (NValueSet s)) => m (Assignment s)
toksdefCommand = do
  name <- readCommandName
  readEquals
  index <- readRegIndex
  texAssign (definitionAt name) (nonexpandableToValue $ ToksReg index)

advanceCommand :: (MonadTeXState s m, MonadError String m) => Bool -> m ()
advanceCommand !global = do
  (et,v) <- required nextExpandedToken
  case doArithmetic v of
    Just m -> do n <- if global
                      then arithmeticGlobalAdvance <$> m
                      else arithmeticLocalAdvance <$> m
                 readOptionalKeyword "by"
                 n
    Nothing -> throwError $ "You can't use " ++ show et ++ " after \\advance"

multiplyCommand :: (MonadTeXState s m, MonadError String m) => Bool -> m ()
multiplyCommand !global = do
  (et,v) <- required nextExpandedToken
  case doArithmetic v of
    Just m -> do n <- if global
                      then arithmeticGlobalMultiply <$> m
                      else arithmeticLocalMultiply <$> m
                 readOptionalKeyword "by"
                 n
    Nothing -> throwError $ "You can't use " ++ show et ++ " after \\multiply"

divideCommand :: (MonadTeXState s m, MonadError String m) => Bool -> m ()
divideCommand !global = do
  (et,v) <- required nextExpandedToken
  case doArithmetic v of
    Just m -> do n <- if global
                      then arithmeticGlobalDivide <$> m
                      else arithmeticLocalDivide <$> m
                 readOptionalKeyword "by"
                 n
    Nothing -> throwError $ "You can't use " ++ show et ++ " after \\divide"

advanceInteger :: (MonadTeXState s m, MonadError String m, IntegralB i) => Lens' (LocalState s) i -> m (Assignment s)
advanceInteger l = do
  value <- use (localState . l)
  arg <- readNumber
  case maybeFromInteger (fromIntegral value + arg) of
    Just value' -> texAssign l (fromInteger value')
    Nothing -> throwError "Arithmetic overflow"

advanceQuantity :: (MonadTeXState s m, MonadError String m, QuantityRead q) => Lens' (LocalState s) q -> m (Assignment s)
advanceQuantity l = do
  value <- use (localState . l)
  arg <- readQuantity
  texAssign l (value <+> arg)

multiplyInteger :: (MonadTeXState s m, MonadError String m, IntegralB i) => Lens' (LocalState s) i -> m (Assignment s)
multiplyInteger l = do
  value <- use (localState . l)
  arg <- readNumber
  case maybeFromInteger (fromIntegral value * arg) of
    Just value' -> texAssign l (fromInteger value')
    Nothing -> throwError "Arithmetic overflow"

multiplyQuantity :: (MonadTeXState s m, MonadError String m, Quantity q) => Lens' (LocalState s) q -> m (Assignment s)
multiplyQuantity l = do
  value <- use (localState . l)
  arg <- readNumber
  texAssign l (scaleAsInteger (* arg) value)

divideInteger :: (MonadTeXState s m, MonadError String m, IntegralB i) => Lens' (LocalState s) i -> m (Assignment s)
divideInteger l = do
  value <- use (localState . l)
  arg <- readNumber
  if arg == 0
    then throwError "Divide by zero" -- TeX says "Arithmetic overflow."
    else case maybeFromInteger (fromIntegral value `quot` arg) of
           Just value' -> texAssign l (fromInteger value')
           Nothing -> throwError "Arithmetic overflow"

divideQuantity :: (MonadTeXState s m, MonadError String m, Quantity q) => Lens' (LocalState s) q -> m (Assignment s)
divideQuantity l = do
  value <- use (localState . l)
  arg <- readNumber
  if arg == 0
    then throwError "Divide by zero"
    else texAssign l (scaleAsInteger (`quot` arg) value)

arithmeticInteger :: (MonadTeXState s m, MonadError String m, IntegralB i) => Lens' (LocalState s) i -> m (Arithmetic m)
arithmeticInteger l = pure $ Arithmetic
  { arithmeticLocalAdvance   = runLocal  $ advanceInteger l
  , arithmeticGlobalAdvance  = runGlobal $ advanceInteger l
  , arithmeticLocalMultiply  = runLocal  $ multiplyInteger l
  , arithmeticGlobalMultiply = runGlobal $ multiplyInteger l
  , arithmeticLocalDivide    = runLocal  $ divideInteger l
  , arithmeticGlobalDivide   = runGlobal $ divideInteger l
  }

arithmeticQuantity :: (MonadTeXState s m, MonadError String m, QuantityRead q) => Lens' (LocalState s) q -> m (Arithmetic m)
arithmeticQuantity l = pure $ Arithmetic
  { arithmeticLocalAdvance   = runLocal  $ advanceQuantity l
  , arithmeticGlobalAdvance  = runGlobal $ advanceQuantity l
  , arithmeticLocalMultiply  = runLocal  $ multiplyQuantity l
  , arithmeticGlobalMultiply = runGlobal $ multiplyQuantity l
  , arithmeticLocalDivide    = runLocal  $ divideQuantity l
  , arithmeticGlobalDivide   = runGlobal $ divideQuantity l
  }

data CommonExecutable = Eglobal
                      | Elet
                      | Efuturelet
                      | Euppercase
                      | Elowercase
                      | Eignorespaces
                      | Echardef
                      | Emathchardef
                      | EUmathchardef
                      | EUmathcharnumdef
                      | Ecatcode
                      | Elccode
                      | Euccode
                      | Emathcode
                      | Edelcode
                      | EUmathcode
                      | EUmathcodenum
                      | EUdelcode
                      | EUdelcodenum
                      | Ebegingroup
                      | Eendgroup
                      | Eendlinechar
                      | Eescapechar
                      | Ecount
                      | Ecountdef
                      | Edimen
                      | Edimendef
                      | Eskip
                      | Eskipdef
                      | Emuskip
                      | Emuskipdef
                      | Etoks
                      | Etoksdef
                      | Eadvance
                      | Emultiply
                      | Edivide
                      deriving (Eq,Show,Enum,Bounded)

instance Meaning CountReg where
  meaningString (CountReg i) = controlSequence "count" <> fromString (show i)

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute CountReg m where
  doExecute (CountReg i)   = runLocal $ setCountReg i
  doGlobal (CountReg i)    = Just $ runGlobal $ setCountReg i
  doArithmetic (CountReg i) = Just $ arithmeticInteger (countRegAt i)
  getQuantity (CountReg i) = QInteger $ use (localState . countRegAt i)

instance Meaning DimenReg where
  meaningString (DimenReg i) = controlSequence "dimen" <> fromString (show i)

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute DimenReg m where
  doExecute (DimenReg i)   = runLocal $ setDimenReg i
  doGlobal (DimenReg i)    = Just $ runGlobal $ setDimenReg i
  doArithmetic (DimenReg i) = Just $ arithmeticQuantity (dimenRegAt i)
  getQuantity (DimenReg i) = QDimension $ use (localState . dimenRegAt i)

instance Meaning SkipReg where
  meaningString (SkipReg i) = controlSequence "skip" <> fromString (show i)

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute SkipReg m where
  doExecute (SkipReg i)   = runLocal $ setSkipReg i
  doGlobal (SkipReg i)    = Just $ runGlobal $ setSkipReg i
  doArithmetic (SkipReg i) = Just $ arithmeticQuantity (skipRegAt i)
  getQuantity (SkipReg i) = QGlue $ use (localState . skipRegAt i)

instance Meaning MuskipReg where
  meaningString (MuskipReg i) = controlSequence "muskip" <> fromString (show i)

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute MuskipReg m where
  doExecute (MuskipReg i)   = runLocal $ setMuskipReg i
  doGlobal (MuskipReg i)    = Just $ runGlobal $ setMuskipReg i
  doArithmetic (MuskipReg i) = Just $ arithmeticQuantity (muskipRegAt i)
  getQuantity (MuskipReg i) = QMuGlue $ use (localState . muskipRegAt i)

instance Meaning ToksReg where
  meaningString (ToksReg i) = controlSequence "toks" <> fromString (show i)

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute ToksReg m where
  doExecute (ToksReg i) = runLocal $ setToksReg i
  doGlobal (ToksReg i) = Just $ runGlobal $ setToksReg i
  getQuantity (ToksReg i) = QToks $ use (localState . toksRegAt i)

instance IsPrimitive CommonExecutable where
  primitiveName Eglobal = "global"
  primitiveName Elet = "let"
  primitiveName Efuturelet = "futurelet"
  primitiveName Euppercase = "uppercase"
  primitiveName Elowercase = "lowercase"
  primitiveName Eignorespaces = "ignorespaces"
  primitiveName Echardef = "chardef"
  primitiveName Emathchardef = "mathchardef"
  primitiveName EUmathchardef = "Umathchardef"
  primitiveName EUmathcharnumdef = "Umathcharnumdef"
  primitiveName Ecatcode = "catcode"
  primitiveName Elccode = "lccode"
  primitiveName Euccode = "uccode"
  primitiveName Emathcode = "mathcode"
  primitiveName Edelcode = "delcode"
  primitiveName EUmathcode = "Umathcode"
  primitiveName EUmathcodenum = "Umathcodenum"
  primitiveName EUdelcode = "Udelcode"
  primitiveName EUdelcodenum = "Udelcodenum"
  primitiveName Ebegingroup = "begingroup"
  primitiveName Eendgroup = "endgroup"
  primitiveName Eendlinechar = "endlinechar"
  primitiveName Eescapechar = "escapechar"
  primitiveName Ecount = "count"
  primitiveName Ecountdef = "countdef"
  primitiveName Edimen = "dimen"
  primitiveName Edimendef = "dimendef"
  primitiveName Eskip = "skip"
  primitiveName Eskipdef = "skipdef"
  primitiveName Emuskip = "muskip"
  primitiveName Emuskipdef = "muskipdef"
  primitiveName Etoks = "toks"
  primitiveName Etoksdef = "toksdef"
  primitiveName Eadvance = "advance"
  primitiveName Emultiply = "multiply"
  primitiveName Edivide = "divide"

instance Meaning CommonExecutable

instance (Monad m, MonadTeXState s m, MonadError String m, Elem CountReg (NValueSet s), Elem DimenReg (NValueSet s), Elem SkipReg (NValueSet s), Elem MuskipReg (NValueSet s), Elem ToksReg (NValueSet s), Meaning (NValue s)) => DoExecute CommonExecutable m where
  doExecute Eglobal          = globalCommand
  doExecute Elet             = runLocal letCommand
  doExecute Efuturelet       = runLocal futureletCommand
  doExecute Echardef         = runLocal chardefCommand
  doExecute Emathchardef     = runLocal mathchardefCommand
  doExecute EUmathchardef    = runLocal umathchardefCommand
  doExecute EUmathcharnumdef = runLocal umathcharnumdefCommand
  doExecute Ecatcode         = runLocal catcodeSet
  doExecute Elccode          = runLocal lccodeSet
  doExecute Euccode          = runLocal uccodeSet
  doExecute Emathcode        = runLocal mathcodeSet
  doExecute Edelcode         = runLocal delcodeSet
  doExecute EUmathcode       = runLocal umathcodeSet
  doExecute EUmathcodenum    = runLocal umathcodenumSet
  doExecute EUdelcode        = runLocal udelcodeSet
  doExecute EUdelcodenum     = runLocal udelcodenumSet
  doExecute Eendlinechar     = runLocal endlinecharSet
  doExecute Eescapechar      = runLocal escapecharSet
  doExecute Euppercase       = uppercaseCommand
  doExecute Elowercase       = lowercaseCommand
  doExecute Ebegingroup      = begingroupCommand
  doExecute Eendgroup        = endgroupCommand
  doExecute Eignorespaces    = ignorespacesCommand
  doExecute Ecount           = runLocal countSet
  doExecute Ecountdef        = runLocal countdefCommand
  doExecute Edimen           = runLocal dimenSet
  doExecute Edimendef        = runLocal dimendefCommand
  doExecute Eskip            = runLocal skipSet
  doExecute Eskipdef         = runLocal skipdefCommand
  doExecute Emuskip          = runLocal muskipSet
  doExecute Emuskipdef       = runLocal muskipdefCommand
  doExecute Etoks            = runLocal toksSet
  doExecute Etoksdef         = runLocal toksdefCommand
  doExecute Eadvance         = advanceCommand False
  doExecute Emultiply        = multiplyCommand False
  doExecute Edivide          = divideCommand False
  doGlobal Eglobal          = Just globalCommand
  doGlobal Elet             = Just $ runGlobal letCommand
  doGlobal Efuturelet       = Just $ runGlobal futureletCommand
  doGlobal Echardef         = Just $ runGlobal chardefCommand
  doGlobal Emathchardef     = Just $ runGlobal mathchardefCommand
  doGlobal EUmathchardef    = Just $ runGlobal umathchardefCommand
  doGlobal EUmathcharnumdef = Just $ runGlobal umathcharnumdefCommand
  doGlobal Ecatcode         = Just $ runGlobal catcodeSet
  doGlobal Elccode          = Just $ runGlobal lccodeSet
  doGlobal Euccode          = Just $ runGlobal uccodeSet
  doGlobal Emathcode        = Just $ runGlobal mathcodeSet
  doGlobal Edelcode         = Just $ runGlobal delcodeSet
  doGlobal EUmathcode       = Just $ runGlobal umathcodeSet
  doGlobal EUmathcodenum    = Just $ runGlobal umathcodenumSet
  doGlobal EUdelcode        = Just $ runGlobal udelcodeSet
  doGlobal EUdelcodenum     = Just $ runGlobal udelcodenumSet
  doGlobal Eendlinechar     = Just $ runGlobal endlinecharSet
  doGlobal Eescapechar      = Just $ runGlobal escapecharSet
  doGlobal Ecount           = Just $ runGlobal countSet
  doGlobal Ecountdef        = Just $ runGlobal countdefCommand
  doGlobal Edimen           = Just $ runGlobal dimenSet
  doGlobal Edimendef        = Just $ runGlobal dimendefCommand
  doGlobal Eskip            = Just $ runGlobal skipSet
  doGlobal Eskipdef         = Just $ runGlobal skipdefCommand
  doGlobal Emuskip          = Just $ runGlobal muskipSet
  doGlobal Emuskipdef       = Just $ runGlobal muskipdefCommand
  doGlobal Etoks            = Just $ runGlobal toksSet
  doGlobal Etoksdef         = Just $ runGlobal toksdefCommand
  doGlobal Eadvance         = Just $ advanceCommand True
  doGlobal Emultiply        = Just $ multiplyCommand True
  doGlobal Edivide          = Just $ divideCommand True
  doGlobal _                = Nothing
  doArithmetic Eendlinechar = Just $ arithmeticInteger endlinechar
  doArithmetic Eescapechar  = Just $ arithmeticInteger escapechar
  doArithmetic Ecount       = Just $ do index <- readRegIndex
                                        arithmeticInteger (countRegAt index)
  doArithmetic Edimen       = Just $ do index <- readRegIndex
                                        arithmeticQuantity (dimenRegAt index)
  doArithmetic Eskip        = Just $ do index <- readRegIndex
                                        arithmeticQuantity (skipRegAt index)
  doArithmetic Emuskip      = Just $ do index <- readRegIndex
                                        arithmeticQuantity (muskipRegAt index)
  doArithmetic _            = Nothing
  getQuantity Ecatcode     = QInteger catcodeGet
  getQuantity Elccode      = QInteger lccodeGet
  getQuantity Euccode      = QInteger uccodeGet
  getQuantity Emathcode    = QInteger mathcodeGet
  getQuantity Edelcode     = QInteger delcodeGet
  getQuantity EUmathcode   = QInteger mathcodeGet -- Same as \mathcode
  getQuantity EUmathcodenum= QInteger mathcodeGet -- Same as \mathcode
  getQuantity EUdelcode    = QInteger delcodeGet  -- Same as \delcode
  getQuantity EUdelcodenum = QInteger delcodeGet  -- Same as \delcode
  getQuantity Eendlinechar = QInteger endlinecharGet
  getQuantity Eescapechar  = QInteger escapecharGet
  getQuantity Ecount       = QInteger countGet
  getQuantity Edimen       = QDimension dimenGet
  getQuantity Eskip        = QGlue skipGet
  getQuantity Emuskip      = QMuGlue muskipGet
  getQuantity Etoks        = QToks toksGet
  getQuantity _            = NotQuantity

executableDefinitions :: (SubList '[CommonValue,CommonExecutable,CountReg,DimenReg,SkipReg,MuskipReg,ToksReg] set) => Map.Map Text (Union set)
executableDefinitions = Map.fromList
  [("relax",          liftUnion Relax)
  ,("endcsname",      liftUnion Endcsname)
  ,("global",         liftUnion Eglobal)
  ,("let",            liftUnion Elet)
  ,("futurelet",      liftUnion Efuturelet)
  ,("uppercase",      liftUnion Euppercase)
  ,("lowercase",      liftUnion Elowercase)
  ,("ignorespaces",   liftUnion Eignorespaces)
  ,("chardef",        liftUnion Echardef)
  ,("mathchardef",    liftUnion Emathchardef)
  ,("Umathchardef",   liftUnion EUmathchardef)
  ,("Umathcharnumdef",liftUnion EUmathcharnumdef)
  ,("catcode",        liftUnion Ecatcode)
  ,("lccode",         liftUnion Elccode)
  ,("uccode",         liftUnion Euccode)
  ,("mathcode",       liftUnion Emathcode)
  ,("delcode",        liftUnion Edelcode)
  ,("Umathcode",      liftUnion EUmathcode)
  ,("Umathcodenum",   liftUnion EUmathcodenum)
  ,("Udelcode",       liftUnion EUdelcode)
  ,("Udelcodenum",    liftUnion EUdelcodenum)
  ,("begingroup",     liftUnion Ebegingroup)
  ,("endgroup",       liftUnion Eendgroup)
  ,("endlinechar",    liftUnion Eendlinechar)
  ,("escapechar",     liftUnion Eescapechar)
  ,("count",          liftUnion Ecount)
  ,("countdef",       liftUnion Ecountdef)
  ,("dimen",          liftUnion Edimen)
  ,("dimendef",       liftUnion Edimendef)
  ,("skip",           liftUnion Eskip)
  ,("skipdef",        liftUnion Eskipdef)
  ,("muskip",         liftUnion Emuskip)
  ,("muskipdef",      liftUnion Emuskipdef)
  ,("toks",           liftUnion Etoks)
  ,("toksdef",        liftUnion Etoksdef)
  ,("advance",        liftUnion Eadvance)
  ,("multiply",       liftUnion Emultiply)
  ,("divide",         liftUnion Edivide)
  ]
