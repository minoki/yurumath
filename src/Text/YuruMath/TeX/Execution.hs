{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.YuruMath.TeX.Execution
  (readRegIndex
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
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Control.Monad.Error.Class
import Control.Lens.Lens (Lens')
import Control.Lens.At (at)
import Control.Lens.Getter (use,uses)
import Control.Lens.Setter (assign,mapped,ASetter)
import Data.OpenUnion
import TypeFun.Data.List (SubList)

-- Used by \count, \countdef, etc
-- 8-bit (0-255) on the original TeX, 15-bit (0-32767) on e-TeX, and 16-bit (0-65535) on LuaTeX
readRegIndex :: (MonadTeXState s m, MonadError String m) => m Int
readRegIndex = do
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

intdefCommand :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
intdefCommand = do
  name <- readCommandName
  readEquals
  value <- readInt32
  texAssign (definitionAt name) (nonexpandableToValue $ IntegerConstant value)

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
                      | EYuruMathIntDef
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
                      | Eadvance
                      | Emultiply
                      | Edivide
                      deriving (Eq,Show,Enum,Bounded)

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
  primitiveName EYuruMathIntDef = "YuruMathIntDef"
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
  primitiveName Eadvance = "advance"
  primitiveName Emultiply = "multiply"
  primitiveName Edivide = "divide"

instance Meaning CommonExecutable

instance (Monad m, MonadTeXState s m, MonadError String m, Meaning (NValue s)) => DoExecute CommonExecutable m where
  doExecute Eglobal          = globalCommand
  doExecute Elet             = runLocal letCommand
  doExecute Efuturelet       = runLocal futureletCommand
  doExecute Echardef         = runLocal chardefCommand
  doExecute Emathchardef     = runLocal mathchardefCommand
  doExecute EUmathchardef    = runLocal umathchardefCommand
  doExecute EUmathcharnumdef = runLocal umathcharnumdefCommand
  doExecute EYuruMathIntDef  = runLocal intdefCommand
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
  doExecute Eadvance         = advanceCommand False
  doExecute Emultiply        = multiplyCommand False
  doExecute Edivide          = divideCommand False
  doGlobal Eglobal           = Just globalCommand
  doGlobal Elet              = Just $ runGlobal letCommand
  doGlobal Efuturelet        = Just $ runGlobal futureletCommand
  doGlobal Echardef          = Just $ runGlobal chardefCommand
  doGlobal Emathchardef      = Just $ runGlobal mathchardefCommand
  doGlobal EUmathchardef     = Just $ runGlobal umathchardefCommand
  doGlobal EUmathcharnumdef  = Just $ runGlobal umathcharnumdefCommand
  doGlobal EYuruMathIntDef   = Just $ runGlobal intdefCommand
  doGlobal Ecatcode          = Just $ runGlobal catcodeSet
  doGlobal Elccode           = Just $ runGlobal lccodeSet
  doGlobal Euccode           = Just $ runGlobal uccodeSet
  doGlobal Emathcode         = Just $ runGlobal mathcodeSet
  doGlobal Edelcode          = Just $ runGlobal delcodeSet
  doGlobal EUmathcode        = Just $ runGlobal umathcodeSet
  doGlobal EUmathcodenum     = Just $ runGlobal umathcodenumSet
  doGlobal EUdelcode         = Just $ runGlobal udelcodeSet
  doGlobal EUdelcodenum      = Just $ runGlobal udelcodenumSet
  doGlobal Eendlinechar      = Just $ runGlobal endlinecharSet
  doGlobal Eescapechar       = Just $ runGlobal escapecharSet
  doGlobal Eadvance          = Just $ advanceCommand True
  doGlobal Emultiply         = Just $ multiplyCommand True
  doGlobal Edivide           = Just $ divideCommand True
  doGlobal _                 = Nothing
  doArithmetic Eendlinechar  = Just $ arithmeticInteger endlinechar
  doArithmetic Eescapechar   = Just $ arithmeticInteger escapechar
  doArithmetic _             = Nothing
  getQuantity Ecatcode       = QInteger catcodeGet
  getQuantity Elccode        = QInteger lccodeGet
  getQuantity Euccode        = QInteger uccodeGet
  getQuantity Emathcode      = QInteger mathcodeGet
  getQuantity Edelcode       = QInteger delcodeGet
  getQuantity EUmathcode     = QInteger mathcodeGet -- Same as \mathcode
  getQuantity EUmathcodenum  = QInteger mathcodeGet -- Same as \mathcode
  getQuantity EUdelcode      = QInteger delcodeGet  -- Same as \delcode
  getQuantity EUdelcodenum   = QInteger delcodeGet  -- Same as \delcode
  getQuantity Eendlinechar   = QInteger endlinecharGet
  getQuantity Eescapechar    = QInteger escapecharGet
  getQuantity _              = NotQuantity

executableDefinitions :: (SubList '[CommonValue,CommonExecutable] set) => Map.Map Text (Union set)
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
  ,("YuruMathIntDef", liftUnion EYuruMathIntDef)
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
  ,("advance",        liftUnion Eadvance)
  ,("multiply",       liftUnion Emultiply)
  ,("divide",         liftUnion Edivide)
  ]
