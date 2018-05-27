{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Text.YuruMath.TeX.Execution where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Tokenizer
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Expansion
import Data.Char
import Data.Word
import Data.Bits
import Data.Text (Text)
import qualified Data.Text as T
import Data.Semigroup
import Control.Monad
import Control.Monad.Error.Class
import qualified Data.Map.Strict as Map
import Control.Lens.Lens (Lens')
import Control.Lens.At (at)
import Control.Lens.Iso (non)
import Control.Lens.Getter (view,use,uses)
import Control.Lens.Setter (assign,modifying,mapped,ASetter)
import Data.OpenUnion
import TypeFun.Data.List (SubList,Elem)

-- \countdef-ed value
newtype CountReg = CountReg Int
  deriving (Eq,Show)

countRegAt :: (IsLocalState localstate) => Int -> Lens' localstate Integer
countRegAt !index = countReg . at index . non 0

-- Used by \count, \countdef
-- 8-bit (0-255) on the original TeX, 15-bit (0-32767) on e-TeX, and 16-bit (0-65535) on LuaTeX
readCountRegIndex :: (MonadTeXState s m, MonadError String m) => m Int
readCountRegIndex = do
  x <- readIntBetween minBound maxBound
  if x < 0 || 65536 <= x
    then throwError $ "Bad register code (" ++ show x ++ ")"
    else return x

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

globalCommand :: (MonadTeXState s m, MonadError String m) => m ()
globalCommand = do
  (et,v) <- evalToken
  case toCommonValue v of
    Just Relax -> globalCommand -- ignore \relax
    Just (Character _ CCSpace) -> globalCommand -- ignore spaces
    _ -> case doGlobal v of
           Just m -> m
           Nothing -> throwError $ "You can't use a prefix with " ++ show et

letCommand :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
letCommand = do
  name <- readCommandName
  readEquals
  t <- required nextEToken
  t <- case t of
    ETCharacter _ CCSpace -> required nextEToken -- one optional space
    _ -> return t
  v <- meaning t
  texAssign (definitionAt name) v

futureletCommand :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
futureletCommand = do
  name <- readCommandName
  t1 <- required nextEToken
  t2 <- required nextEToken
  unreadETokens 0 [t1,t2]
  v <- meaning t2
  texAssign (definitionAt name) v

uppercaseCommand :: (MonadTeXState s m, MonadError String m) => m ()
uppercaseCommand = do
  text <- readGeneralText
  toUpper <- ucCodeFn
  let makeUpper (TTCharacter c cc) | d <- toUpper c, d /= '\0' = TTCharacter d cc
      makeUpper t = t
  let text' = map makeUpper text
  unreadETokens 0 (map toEToken text')

lowercaseCommand :: (MonadTeXState s m, MonadError String m) => m ()
lowercaseCommand = do
  text <- readGeneralText
  toLower <- lcCodeFn
  let makeLower (TTCharacter c cc) | d <- toLower c, d /= '\0' = TTCharacter d cc
      makeLower t = t
  let text' = map makeLower text
  unreadETokens 0 (map toEToken text')

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
  mathclass <- readIntBetween 0 7 -- "Invalid math code"
  fam <- readIntBetween 0 0xFF -- "Invalid math code"
  c <- readUnicodeScalarValue
  let w = injectCommonValue $ DefinedMathCharacter $ mkUMathCode (toEnum mathclass) (fromIntegral fam) c
  texAssign (definitionAt name) (Right w)

-- \Umathcharnumdef<control sequence><equals><32-bit number>
umathcharnumdefCommand :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
umathcharnumdefCommand = do
  name <- readCommandName
  readEquals
  value <- readInt32
  let valueu = fromIntegral value :: Word32
      -- mathclass = 0x7 .&. (valueu `shiftR` 21)
      -- fam = 0xFF .&. (valueu `shiftR` 24)
      code = 0x1FFFFF .&. valueu
  unless (isUnicodeScalarValue code)
    $ throwError "\\Umathcharnumdef: Invalid math code"
  let w = injectCommonValue $ DefinedMathCharacter $ UMathCode value
  texAssign (definitionAt name) (Right w)

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

-- \UmathcodenumSet

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
  fam <- readIntBetween 0 0xFF -- "Invalid delimiter code."
  c <- readUnicodeScalarValue
  let w = mkUDelCode (fromIntegral fam) c
  texAssign (delcodeMap . at slot) (Just w)

-- \Udelcodenum<21-bit number><equals><32-bit number>
udelcodenumSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
udelcodenumSet = do
  slot <- readUnicodeScalarValue
  readEquals
  value <- readInt32
  unless (0 <= value && value < 2^(29::Int))
    $ throwError "\\Udelcodenum: Invalid delimiter code"
  let valueu = fromIntegral value :: Word32
      -- fam = 0xFF .&. (valueu `shiftR` 21)
      code = 0x1FFFFF .&. valueu
  unless (isUnicodeScalarValue code)
    $ throwError "\\Udelcodenum: Invalid delimiter code"
  let w = UDelimiterCode value
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
countSet = do
  index <- readCountRegIndex
  setCountReg index

countGet :: (MonadTeXState s m, MonadError String m) => m Integer
countGet = do
  index <- readCountRegIndex
  use (localState . countRegAt index)

countdefCommand :: (MonadTeXState s m, MonadError String m, Value s ~ Union set, Elem CountReg set) => m (Assignment s)
countdefCommand = do
  name <- readCommandName
  readEquals
  index <- readCountRegIndex
  texAssign (definitionAt name) (Right $ liftUnion $ CountReg index)

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

advanceInteger :: (MonadTeXState s m, MonadError String m) => Lens' (LocalState s) Integer -> m (Assignment s)
advanceInteger l = do
  value <- use (localState . l)
  arg <- readNumber
  let !value' = value + arg
  texAssign l value'

advanceInt :: (MonadTeXState s m, MonadError String m, Integral i, Bounded i) => Lens' (LocalState s) i -> m (Assignment s)
advanceInt l = do
  value <- use (localState . l)
  arg <- readNumber
  let !value' = fromIntegral value + arg
  if value' < fromIntegral (minBound `asTypeOf` value) || fromIntegral (maxBound `asTypeOf` value) < value'
    then throwError "Arithmetic overflow"
    else texAssign l (fromInteger value')

multiplyInteger :: (MonadTeXState s m, MonadError String m) => Lens' (LocalState s) Integer -> m (Assignment s)
multiplyInteger l = do
  value <- use (localState . l)
  arg <- readNumber
  let !value' = value * arg
  texAssign l value'

multiplyInt :: (MonadTeXState s m, MonadError String m, Integral i, Bounded i) => Lens' (LocalState s) i -> m (Assignment s)
multiplyInt l = do
  value <- use (localState . l)
  arg <- readNumber
  let !value' = fromIntegral value * arg
  if value' < fromIntegral (minBound `asTypeOf` value) || fromIntegral (maxBound `asTypeOf` value) < value'
    then throwError "Arithmetic overflow"
    else texAssign l (fromInteger value')

divideInteger :: (MonadTeXState s m, MonadError String m) => Lens' (LocalState s) Integer -> m (Assignment s)
divideInteger l = do
  value <- use (localState . l)
  arg <- readNumber
  if arg == 0
    then throwError "Divide by zero" -- TeX says "Arithmetic overflow."
    else do
    let !value' = value `quot` arg
    texAssign l value'

divideInt :: (MonadTeXState s m, MonadError String m, Integral i, Bounded i) => Lens' (LocalState s) i -> m (Assignment s)
divideInt l = do
  value <- use (localState . l)
  arg <- readNumber
  if arg == 0
    then throwError "Divide by zero" -- TeX says "Arithmetic overflow."
    else do
    let !value' = fromIntegral value `quot` arg
    if value' < fromIntegral (minBound `asTypeOf` value) || fromIntegral (maxBound `asTypeOf` value) < value'
      then throwError "Arithmetic overflow"
      else texAssign l (fromInteger value')

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
                      | Ebegingroup
                      | Eendgroup
                      | Eendlinechar
                      | Eescapechar
                      | Ecount
                      | Ecountdef
                      | Eadvance
                      | Emultiply
                      | Edivide
                      deriving (Eq,Show)

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute CountReg m where
  doExecute (CountReg i)       = runLocal $ setCountReg i
  doGlobal (CountReg i)        = Just $ runGlobal $ setCountReg i
  doAdvance (CountReg i)       = Just $ runArithmetic $ advanceInteger (countRegAt i)
  doMultiply (CountReg i)      = Just $ runArithmetic $ multiplyInteger (countRegAt i)
  doDivide (CountReg i)        = Just $ runArithmetic $ divideInteger (countRegAt i)
  getIntegerValue (CountReg i) = Just $ use (localState . countRegAt i)

instance (Monad m, MonadTeXState s m, MonadError String m, Value s ~ Union set, Elem CountReg set) => DoExecute CommonExecutable m where
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
  doExecute Eendlinechar     = runLocal endlinecharSet
  doExecute Eescapechar      = runLocal escapecharSet
  doExecute Euppercase       = uppercaseCommand
  doExecute Elowercase       = lowercaseCommand
  doExecute Ebegingroup      = begingroupCommand
  doExecute Eendgroup        = endgroupCommand
  doExecute Ecount           = runLocal countSet
  doExecute Ecountdef        = runLocal countdefCommand
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
  doGlobal Eendlinechar     = Just $ runGlobal endlinecharSet
  doGlobal Eescapechar      = Just $ runGlobal escapecharSet
  doGlobal Ecount           = Just $ runGlobal countSet
  doGlobal Ecountdef        = Just $ runGlobal countdefCommand
  doGlobal Eadvance         = Just $ advanceCommand True
  doGlobal Emultiply        = Just $ multiplyCommand True
  doGlobal Edivide          = Just $ divideCommand True
  doGlobal _                = Nothing
  doAdvance Eendlinechar    = Just $ runArithmetic $ advanceInt endlinechar
  doAdvance Eescapechar     = Just $ runArithmetic $ advanceInt escapechar
  doAdvance Ecount          = Just $ do
    index <- readCountRegIndex
    runArithmetic $ advanceInteger (countRegAt index)
  doAdvance _               = Nothing
  doMultiply Eendlinechar   = Just $ runArithmetic $ multiplyInt endlinechar
  doMultiply Eescapechar    = Just $ runArithmetic $ multiplyInt escapechar
  doMultiply Ecount         = Just $ do
    index <- readCountRegIndex
    runArithmetic $ multiplyInteger (countRegAt index)
  doMultiply _              = Nothing
  doDivide Eendlinechar     = Just $ runArithmetic $ divideInt endlinechar
  doDivide Eescapechar      = Just $ runArithmetic $ divideInt escapechar
  doDivide Ecount           = Just $ do
    index <- readCountRegIndex
    runArithmetic $ divideInteger (countRegAt index)
  doDivide _                = Nothing
  getIntegerValue Ecatcode  = Just catcodeGet
  getIntegerValue Elccode   = Just lccodeGet
  getIntegerValue Euccode   = Just uccodeGet
  getIntegerValue Emathcode = Just mathcodeGet
  getIntegerValue Edelcode  = Just delcodeGet
  getIntegerValue Eendlinechar = Just endlinecharGet
  getIntegerValue Eescapechar  = Just escapecharGet
  getIntegerValue Ecount    = Just countGet
  getIntegerValue _         = Nothing

executableDefinitions :: (SubList '[CommonValue,CommonExecutable,CountReg] set) => Map.Map Text (Union set)
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
  ,("begingroup",     liftUnion Ebegingroup)
  ,("endgroup",       liftUnion Eendgroup)
  ,("endlinechar",    liftUnion Eendlinechar)
  ,("escapechar",     liftUnion Eescapechar)
  ,("count",          liftUnion Ecount)
  ,("countdef",       liftUnion Ecountdef)
  ,("advance",        liftUnion Eadvance)
  ,("multiply",       liftUnion Emultiply)
  ,("divide",         liftUnion Edivide)
  ]
