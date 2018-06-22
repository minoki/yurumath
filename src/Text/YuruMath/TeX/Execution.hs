{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Text.YuruMath.TeX.Execution
  (CountReg
  ,DimenReg
  ,SkipReg
  ,MuskipReg
  ,readRegIndex
  ,Assignment
  ,runLocal
  ,runGlobal
  ,runArithmetic
  ,texAssign
  ,advanceInteger
  ,advanceQuantity
  ,multiplyInteger
  ,multiplyQuantity
  ,divideInteger
  ,divideQuantity
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

data Assignment s where
  WillAssign :: ASetter (LocalState s) (LocalState s) b b -> !b -> Assignment s

runLocal, runGlobal :: (MonadTeXState s m) => m (Assignment s) -> m ()
runLocal m = do
  WillAssign setter value <- m
  assign (localState . setter) value
runGlobal m = do
  WillAssign setter value <- m
  assign (localStates . mapped . setter) value

runArithmetic :: (MonadTeXState s m) => m (Assignment s) -> m (Bool -> m ())
runArithmetic m = return $ \g -> if g then runGlobal m else runLocal m

texAssign :: (MonadTeXState s m) => ASetter (LocalState s) (LocalState s) b b -> b -> m (Assignment s)
texAssign setter !value = return (WillAssign setter value)

globalCommand :: (MonadTeXState s m, MonadError String m, Meaning (Value s)) => m ()
globalCommand = do
  (et,v) <- evalToken
  case toCommonValue v of
    Just Relax -> globalCommand -- ignore \relax
    Just (Unexpanded {}) -> globalCommand -- ignore \relax
    Just (Character _ CCSpace) -> globalCommand -- ignore spaces
    _ -> case doGlobal v of
           Just m -> m
           Nothing -> invalidPrefix "global" v

letCommand :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
letCommand = do
  name <- readCommandName
  readEquals
  readOneOptionalSpace
  v <- required nextEToken >>= meaningWithoutExpansion
  texAssign (definitionAt name) v

futureletCommand :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
futureletCommand = do
  name <- readCommandName
  t1 <- required nextEToken
  t2 <- required nextEToken
  unreadETokens' [t1,t2]
  v <- meaningWithoutExpansion t2
  texAssign (definitionAt name) v

uppercaseCommand :: (MonadTeXState s m, MonadError String m) => m ()
uppercaseCommand = do
  text <- readGeneralTextE
  toUpper <- ucCodeFn
  let makeUpper et@(ETCharacter { etChar = c }) | d <- toUpper c, d /= '\0' = et { etChar = d }
      makeUpper et@(ETCommandName { etName = NActiveChar c }) | d <- toUpper c, d /= '\0' = et { etFlavor = ECNFPlain, etName = NActiveChar d }
      makeUpper et@(ETCommandName { etFlavor = ECNFNoexpanded }) = et { etFlavor = ECNFPlain } -- strip 'noexpaded' flag
      makeUpper et = et
  unreadETokens' (map makeUpper text)

lowercaseCommand :: (MonadTeXState s m, MonadError String m) => m ()
lowercaseCommand = do
  text <- readGeneralTextE
  toLower <- lcCodeFn
  let makeLower et@(ETCharacter { etChar = c }) | d <- toLower c, d /= '\0' = et { etChar = d }
      makeLower et@(ETCommandName { etName = NActiveChar c }) | d <- toLower c, d /= '\0' = et { etFlavor = ECNFPlain, etName = NActiveChar d }
      makeLower et@(ETCommandName { etFlavor = ECNFNoexpanded }) = et { etFlavor = ECNFPlain } -- strip 'noexpaded' flag
      makeLower et = et
  unreadETokens' (map makeLower text)

-- \chardef<control sequence><equals><number>
chardefCommand :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
chardefCommand = do
  name <- readCommandName
  readEquals
  c <- readUnicodeScalarValue
  let w = injectCommonValue $ DefinedCharacter c
  texAssign (definitionAt name) (Right w)

-- \mathchardef<control sequence><equals><15-bit number>
mathchardefCommand :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
mathchardefCommand = do
  name <- readCommandName
  readEquals
  v <- readIntBetween 0 0x8000 -- "Bad math code (" ++ show v ++ ")"
  let w = injectCommonValue $ DefinedMathCharacter (MathCode (fromIntegral v))
  texAssign (definitionAt name) (Right w)

-- \Umathchardef<control sequence><equals><3-bit number><8-bit number><21-bit number>
umathchardefCommand :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
umathchardefCommand = do
  name <- readCommandName
  readEquals
  w <- readUMathCodeTriplet
  texAssign (definitionAt name) (Right $ injectCommonValue $ DefinedMathCharacter w)

-- \Umathcharnumdef<control sequence><equals><32-bit number>
umathcharnumdefCommand :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
umathcharnumdefCommand = do
  name <- readCommandName
  readEquals
  w <- readUMathCode32
  texAssign (definitionAt name) (Right $ injectCommonValue $ DefinedMathCharacter w)

-- \countdef, \dimendef, \muskipdef, \skipdef, \toksdef

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

countdefCommand :: (MonadTeXState s m, MonadError String m, Value s ~ Union set, Elem CountReg set) => m (Assignment s)
countdefCommand = do
  name <- readCommandName
  readEquals
  index <- readRegIndex
  texAssign (definitionAt name) (Right $ liftUnion $ CountReg index)

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

dimendefCommand :: (MonadTeXState s m, MonadError String m, Value s ~ Union set, Elem DimenReg set) => m (Assignment s)
dimendefCommand = do
  name <- readCommandName
  readEquals
  index <- readRegIndex
  texAssign (definitionAt name) (Right $ liftUnion $ DimenReg index)

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

skipdefCommand :: (MonadTeXState s m, MonadError String m, Value s ~ Union set, Elem SkipReg set) => m (Assignment s)
skipdefCommand = do
  name <- readCommandName
  readEquals
  index <- readRegIndex
  texAssign (definitionAt name) (Right $ liftUnion $ SkipReg index)

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

muskipdefCommand :: (MonadTeXState s m, MonadError String m, Value s ~ Union set, Elem MuskipReg set) => m (Assignment s)
muskipdefCommand = do
  name <- readCommandName
  readEquals
  index <- readRegIndex
  texAssign (definitionAt name) (Right $ liftUnion $ MuskipReg index)

advanceCommand :: (MonadTeXState s m, MonadError String m) => Bool -> m ()
advanceCommand !global = do
  (et,v) <- evalToken
  case doAdvance v of
    Just m -> do n <- m
                 readOptionalKeyword "by"
                 n global
    Nothing -> throwError $ "You can't use " ++ show et ++ " after \\advance"

multiplyCommand :: (MonadTeXState s m, MonadError String m) => Bool -> m ()
multiplyCommand !global = do
  (et,v) <- evalToken
  case doMultiply v of
    Just m -> do n <- m
                 readOptionalKeyword "by"
                 n global
    Nothing -> throwError $ "You can't use " ++ show et ++ " after \\multiply"

divideCommand :: (MonadTeXState s m, MonadError String m) => Bool -> m ()
divideCommand !global = do
  (et,v) <- evalToken
  case doDivide v of
    Just m -> do n <- m
                 readOptionalKeyword "by"
                 n global
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

data CommonExecutable = Eglobal
                      | Elet
                      | Efuturelet
                      | Euppercase
                      | Elowercase
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
                      | Eadvance
                      | Emultiply
                      | Edivide
                      deriving (Eq,Show)

instance Meaning CountReg where
  meaningString (CountReg i) = controlSequence "count" <> fromString (show i)

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute CountReg m where
  doExecute (CountReg i)   = runLocal $ setCountReg i
  doGlobal (CountReg i)    = Just $ runGlobal $ setCountReg i
  doAdvance (CountReg i)   = Just $ runArithmetic $ advanceInteger (countRegAt i)
  doMultiply (CountReg i)  = Just $ runArithmetic $ multiplyInteger (countRegAt i)
  doDivide (CountReg i)    = Just $ runArithmetic $ divideInteger (countRegAt i)
  getQuantity (CountReg i) = QInteger $ use (localState . countRegAt i)

instance Meaning DimenReg where
  meaningString (DimenReg i) = controlSequence "dimen" <> fromString (show i)

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute DimenReg m where
  doExecute (DimenReg i)   = runLocal $ setDimenReg i
  doGlobal (DimenReg i)    = Just $ runGlobal $ setDimenReg i
  doAdvance (DimenReg i)   = Just $ runArithmetic $ advanceQuantity (dimenRegAt i)
  doMultiply (DimenReg i)  = Just $ runArithmetic $ multiplyQuantity (dimenRegAt i)
  doDivide (DimenReg i)    = Just $ runArithmetic $ divideQuantity (dimenRegAt i)
  getQuantity (DimenReg i) = QDimension $ use (localState . dimenRegAt i)

instance Meaning SkipReg where
  meaningString (SkipReg i) = controlSequence "skip" <> fromString (show i)

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute SkipReg m where
  doExecute (SkipReg i)   = runLocal $ setSkipReg i
  doGlobal (SkipReg i)    = Just $ runGlobal $ setSkipReg i
  doAdvance (SkipReg i)   = Just $ runArithmetic $ advanceQuantity (skipRegAt i)
  doMultiply (SkipReg i)  = Just $ runArithmetic $ multiplyQuantity (skipRegAt i)
  doDivide (SkipReg i)    = Just $ runArithmetic $ divideQuantity (skipRegAt i)
  getQuantity (SkipReg i) = QGlue $ use (localState . skipRegAt i)

instance Meaning MuskipReg where
  meaningString (MuskipReg i) = controlSequence "muskip" <> fromString (show i)

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute MuskipReg m where
  doExecute (MuskipReg i)   = runLocal $ setMuskipReg i
  doGlobal (MuskipReg i)    = Just $ runGlobal $ setMuskipReg i
  doAdvance (MuskipReg i)   = Just $ runArithmetic $ advanceQuantity (muskipRegAt i)
  doMultiply (MuskipReg i)  = Just $ runArithmetic $ multiplyQuantity (muskipRegAt i)
  doDivide (MuskipReg i)    = Just $ runArithmetic $ divideQuantity (muskipRegAt i)
  getQuantity (MuskipReg i) = QMuGlue $ use (localState . muskipRegAt i)

instance Meaning CommonExecutable where
  meaningString Eglobal = controlSequence "global"
  meaningString Elet = controlSequence "let"
  meaningString Efuturelet = controlSequence "futurelet"
  meaningString Euppercase = controlSequence "uppercase"
  meaningString Elowercase = controlSequence "lowercase"
  meaningString Echardef = controlSequence "chardef"
  meaningString Emathchardef = controlSequence "mathchardef"
  meaningString EUmathchardef = controlSequence "Umathchardef"
  meaningString EUmathcharnumdef = controlSequence "Umathcharnumdef"
  meaningString Ecatcode = controlSequence "catcode"
  meaningString Elccode = controlSequence "lccode"
  meaningString Euccode = controlSequence "uccode"
  meaningString Emathcode = controlSequence "mathcode"
  meaningString Edelcode = controlSequence "delcode"
  meaningString EUmathcode = controlSequence "Umathcode"
  meaningString EUmathcodenum = controlSequence "Umathcodenum"
  meaningString EUdelcode = controlSequence "Udelcode"
  meaningString EUdelcodenum = controlSequence "Udelcodenum"
  meaningString Ebegingroup = controlSequence "begingroup"
  meaningString Eendgroup = controlSequence "endgroup"
  meaningString Eendlinechar = controlSequence "endlinechar"
  meaningString Eescapechar = controlSequence "escapechar"
  meaningString Ecount = controlSequence "count"
  meaningString Ecountdef = controlSequence "countdef"
  meaningString Edimen = controlSequence "dimen"
  meaningString Edimendef = controlSequence "dimendef"
  meaningString Eskip = controlSequence "skip"
  meaningString Eskipdef = controlSequence "skipdef"
  meaningString Emuskip = controlSequence "muskip"
  meaningString Emuskipdef = controlSequence "muskipdef"
  meaningString Eadvance = controlSequence "advance"
  meaningString Emultiply = controlSequence "multiply"
  meaningString Edivide = controlSequence "divide"

instance (Monad m, MonadTeXState s m, MonadError String m, Value s ~ Union set, Elem CountReg set, Elem DimenReg set, Elem SkipReg set, Elem MuskipReg set, Meaning (Value s)) => DoExecute CommonExecutable m where
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
  doExecute Ecount           = runLocal countSet
  doExecute Ecountdef        = runLocal countdefCommand
  doExecute Edimen           = runLocal dimenSet
  doExecute Edimendef        = runLocal dimendefCommand
  doExecute Eskip            = runLocal skipSet
  doExecute Eskipdef         = runLocal skipdefCommand
  doExecute Emuskip          = runLocal muskipSet
  doExecute Emuskipdef       = runLocal muskipdefCommand
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
  doGlobal Eadvance         = Just $ advanceCommand True
  doGlobal Emultiply        = Just $ multiplyCommand True
  doGlobal Edivide          = Just $ divideCommand True
  doGlobal _                = Nothing
  doAdvance Eendlinechar    = Just $ runArithmetic $ advanceInteger endlinechar
  doAdvance Eescapechar     = Just $ runArithmetic $ advanceInteger escapechar
  doAdvance Ecount          = Just $ readRegIndex >>= (\index -> runArithmetic $ advanceInteger $ countRegAt index)
  doAdvance Edimen          = Just $ readRegIndex >>= (\index -> runArithmetic $ advanceQuantity $ dimenRegAt index)
  doAdvance Eskip           = Just $ readRegIndex >>= (\index -> runArithmetic $ advanceQuantity $ skipRegAt index)
  doAdvance Emuskip         = Just $ readRegIndex >>= (\index -> runArithmetic $ advanceQuantity $ muskipRegAt index)
  doAdvance _               = Nothing
  doMultiply Eendlinechar   = Just $ runArithmetic $ multiplyInteger endlinechar
  doMultiply Eescapechar    = Just $ runArithmetic $ multiplyInteger escapechar
  doMultiply Ecount         = Just $ readRegIndex >>= (\index -> runArithmetic $ multiplyInteger $ countRegAt index)
  doMultiply Edimen         = Just $ readRegIndex >>= (\index -> runArithmetic $ multiplyQuantity $ dimenRegAt index)
  doMultiply Eskip          = Just $ readRegIndex >>= (\index -> runArithmetic $ multiplyQuantity $ skipRegAt index)
  doMultiply Emuskip        = Just $ readRegIndex >>= (\index -> runArithmetic $ multiplyQuantity $ muskipRegAt index)
  doMultiply _              = Nothing
  doDivide Eendlinechar     = Just $ runArithmetic $ divideInteger endlinechar
  doDivide Eescapechar      = Just $ runArithmetic $ divideInteger escapechar
  doDivide Ecount           = Just $ readRegIndex >>= (\index -> runArithmetic $ divideInteger $ countRegAt index)
  doDivide Edimen           = Just $ readRegIndex >>= (\index -> runArithmetic $ divideQuantity $ dimenRegAt index)
  doDivide Eskip            = Just $ readRegIndex >>= (\index -> runArithmetic $ divideQuantity $ skipRegAt index)
  doDivide Emuskip          = Just $ readRegIndex >>= (\index -> runArithmetic $ divideQuantity $ muskipRegAt index)
  doDivide _                = Nothing
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
  getQuantity _            = NotQuantity

executableDefinitions :: (SubList '[CommonValue,CommonExecutable,CountReg,DimenReg,SkipReg,MuskipReg] set) => Map.Map Text (Union set)
executableDefinitions = Map.fromList
  [("relax",          liftUnion Relax)
  ,("endcsname",      liftUnion Endcsname)
  ,("global",         liftUnion Eglobal)
  ,("let",            liftUnion Elet)
  ,("futurelet",      liftUnion Efuturelet)
  ,("uppercase",      liftUnion Euppercase)
  ,("lowercase",      liftUnion Elowercase)
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
  ,("advance",        liftUnion Eadvance)
  ,("multiply",       liftUnion Emultiply)
  ,("divide",         liftUnion Edivide)
  ]
