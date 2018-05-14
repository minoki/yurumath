{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
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
import Control.Lens.At (at)
import Control.Lens.Getter (view,use,uses)
import Control.Lens.Setter (assign,modifying)
import Data.OpenUnion
import TypeFun.Data.List (SubList)

letCommand :: (MonadTeXState s m, MonadError String m) => m ()
letCommand = do
  name <- readCommandName
  readEquals
  t <- required nextEToken
  t <- case t of
    ETCharacter _ CCSpace -> required nextEToken -- one optional space
    _ -> return t
  v <- meaning t
  assign (localState . definitionAt name) v

futureletCommand :: (MonadTeXState s m, MonadError String m) => m ()
futureletCommand = do
  name <- readCommandName
  t1 <- required nextEToken
  t2 <- required nextEToken
  unreadETokens 0 [t1,t2]
  v <- meaning t2
  assign (localState . definitionAt name) v

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
chardefCommand :: (MonadTeXState s m, MonadError String m) => m ()
chardefCommand = do
  name <- readCommandName
  readEquals
  c <- readUnicodeScalarValue
  let w = injectCommonValue $ DefinedCharacter c
  assign (localState . definitionAt name) (Right w)

-- \mathchardef<control sequence><equals><15-bit number>
mathchardefCommand :: (MonadTeXState s m, MonadError String m) => m ()
mathchardefCommand = do
  name <- readCommandName
  readEquals
  v <- readIntBetween 0 0x8000 -- "Bad math code (" ++ show v ++ ")"
  let w = injectCommonValue $ DefinedMathCharacter (MathCode (fromIntegral v))
  assign (localState . definitionAt name) (Right w)

-- \Umathchardef<control sequence><equals><3-bit number><8-bit number><21-bit number>
umathchardefCommand :: (MonadTeXState s m, MonadError String m) => m ()
umathchardefCommand = do
  name <- readCommandName
  readEquals
  mathclass <- readIntBetween 0 7 -- "Invalid math code"
  fam <- readIntBetween 0 0xFF -- "Invalid math code"
  c <- readUnicodeScalarValue
  let w = injectCommonValue $ DefinedMathCharacter $ mkUMathCode (toEnum mathclass) (fromIntegral fam) c
  assign (localState . definitionAt name) (Right w)

-- \Umathcharnumdef<control sequence><equals><32-bit number>
umathcharnumdefCommand :: (MonadTeXState s m, MonadError String m) => m ()
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
  assign (localState . definitionAt name) (Right w)

-- \countdef, \dimendef, \muskipdef, \skipdef, \toksdef

catcodeGet :: (MonadTeXState s m, MonadError String m) => m Integer
catcodeGet = do
  slot <- readUnicodeScalarValue
  (fromIntegral . fromEnum) <$> categoryCodeOf slot

-- \catcode<21-bit number><equals><4-bit number>
catcodeSet :: (MonadTeXState s m, MonadError String m) => m ()
catcodeSet = do
  slot <- readUnicodeScalarValue
  readEquals
  v <- readIntBetween 0 15 -- "Invalid code (" ++ show v ++ "), should be in the range 0..15."
  let w = toEnum v
  assign (localState . catcodeMap . at slot) (Just w)

lccodeGet :: (MonadTeXState s m, MonadError String m) => m Integer
lccodeGet = do
  slot <- readUnicodeScalarValue
  (fromIntegral . fromEnum) <$> lcCodeOf slot

-- \lccode<21-bit number><equals><21-bit number>
lccodeSet :: (MonadTeXState s m, MonadError String m) => m ()
lccodeSet = do
  slot <- readUnicodeScalarValue
  readEquals
  v <- readUnicodeScalarValue
  assign (localState . lccodeMap . at slot) (Just v)

uccodeGet :: (MonadTeXState s m, MonadError String m) => m Integer
uccodeGet = do
  slot <- readUnicodeScalarValue
  (fromIntegral . fromEnum) <$> ucCodeOf slot

-- \uccode<21-bit number><equals><21-bit number>
uccodeSet :: (MonadTeXState a m, MonadError String m) => m ()
uccodeSet = do
  slot <- readUnicodeScalarValue
  readEquals
  v <- readUnicodeScalarValue
  assign (localState . uccodeMap . at slot) (Just v)

mathcodeGet :: (MonadTeXState s m, MonadError String m) => m Integer
mathcodeGet = do
  slot <- readUnicodeScalarValue
  mathcodeToInt <$> mathCodeOf slot
  where
    mathcodeToInt (MathCode x) = fromIntegral x
    mathcodeToInt (UMathCode x) = fromIntegral x

-- \mathcode<21-bit number><equals><15-bit number>
mathcodeSet :: (MonadTeXState s m, MonadError String m) => m ()
mathcodeSet = do
  slot <- readUnicodeScalarValue
  readEquals
  v <- readIntBetween 0 0x8000 -- "Bad math code (" ++ show v ++ ")"
  let w = MathCode (fromIntegral v)
  assign (localState . mathcodeMap . at slot) (Just w)

-- \UmathcodenumSet

delcodeGet :: (MonadTeXState s m, MonadError String m) => m Integer
delcodeGet = do
  slot <- readUnicodeScalarValue
  delcodeToInt <$> delimiterCodeOf slot
  where
    delcodeToInt (DelimiterCode x) = fromIntegral x
    delcodeToInt (UDelimiterCode x) = fromIntegral x

-- \delcode<21-bit number><equals><24-bit number>
delcodeSet :: (MonadTeXState s m, MonadError String m) => m ()
delcodeSet = do
  slot <- readUnicodeScalarValue
  readEquals
  v <- readIntBetween (-1) 0xFFFFFF -- "Invalid delimiter code."
  let w = DelimiterCode (fromIntegral v)
  assign (localState . delcodeMap . at slot) (Just w)

-- \Udelcode<21-bit number><equals><8-bit number><21-bit number>
udelcodeSet :: (MonadTeXState s m, MonadError String m) => m ()
udelcodeSet = do
  slot <- readUnicodeScalarValue
  readEquals
  fam <- readIntBetween 0 0xFF -- "Invalid delimiter code."
  c <- readUnicodeScalarValue
  let w = mkUDelCode (fromIntegral fam) c
  assign (localState . delcodeMap . at slot) (Just w)

-- \Udelcodenum<21-bit number><equals><32-bit number>
udelcodenumSet :: (MonadTeXState s m, MonadError String m) => m ()
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
  assign (localState . delcodeMap . at slot) (Just w)

defCommand :: (MonadTeXState s m, MonadError String m) => m ()
defCommand = do
  cs <- required nextEToken
  throwError "\\def: not implemented yet"

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute CommonValue m where
  doExecute (Character _ _)          = return () -- not implemented yet
  doExecute (DefinedCharacter _)     = return () -- not implemented yet
  doExecute (DefinedMathCharacter _) = return () -- not implemented yet
  doExecute (IntegerConstant _)      = throwError $ "Unexpected integer constant."
  doExecute Relax                    = return () -- do nothing
  doExecute (Unexpanded _)           = return () -- do nothing
  doExecute (Undefined _)            = throwError $ "Undefined control sequence."
  doExecute Endcsname                = throwError "Extra \\endcsname"
  getIntegerValue _ = Nothing

data CommonExecutable = Elet
                      | Efuturelet
                      | Euppercase
                      | Elowercase
                      | Echardef
                      | Emathchardef
                      | EUmathchardef
                      | EUmathcharnumdef
                      | Edef
                      | Ecatcode
                      | Elccode
                      | Euccode
                      | Emathcode
                      | Edelcode
                      deriving (Eq,Show)

-- orphaned instance...
instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute CommonExecutable m where
  doExecute Elet             = letCommand
  doExecute Efuturelet       = futureletCommand
  doExecute Euppercase       = uppercaseCommand
  doExecute Elowercase       = lowercaseCommand
  doExecute Echardef         = chardefCommand
  doExecute Emathchardef     = mathchardefCommand
  doExecute EUmathchardef    = umathchardefCommand
  doExecute EUmathcharnumdef = umathcharnumdefCommand
  doExecute Edef             = defCommand
  doExecute Ecatcode         = catcodeSet
  doExecute Elccode          = lccodeSet
  doExecute Euccode          = uccodeSet
  doExecute Emathcode        = mathcodeSet
  doExecute Edelcode         = delcodeSet
  getIntegerValue Ecatcode  = Just catcodeGet
  getIntegerValue Elccode   = Just lccodeGet
  getIntegerValue Euccode   = Just uccodeGet
  getIntegerValue Emathcode = Just mathcodeGet
  getIntegerValue Edelcode  = Just delcodeGet
  getIntegerValue _         = Nothing

executableDefinitions :: (SubList '[CommonValue,CommonExecutable] set) => Map.Map Text (Union set)
executableDefinitions = Map.fromList
  [("relax",          liftUnion Relax)
  ,("endcsname",      liftUnion Endcsname)
  ,("let",            liftUnion Elet)
  ,("futurelet",      liftUnion Efuturelet)
  ,("uppercase",      liftUnion Euppercase)
  ,("lowercase",      liftUnion Elowercase)
  ,("chardef",        liftUnion Echardef)
  ,("mathchardef",    liftUnion Emathchardef)
  ,("Umathchardef",   liftUnion EUmathchardef)
  ,("Umathcharnumdef",liftUnion EUmathcharnumdef)
  ,("def",            liftUnion Edef)
  ,("catcode",        liftUnion Ecatcode)
  ,("lccode",         liftUnion Elccode)
  ,("uccode",         liftUnion Euccode)
  ,("mathcode",       liftUnion Emathcode)
  ,("delcode",        liftUnion Edelcode)

   -- plain TeX / LaTeX
  ,("z@",             liftUnion (IntegerConstant 0))
  ,("@ne",            liftUnion (IntegerConstant 1))
  ,("m@ne",           liftUnion (IntegerConstant (-1)))
  ,("tw@",            liftUnion (IntegerConstant 2))
  ,("sixt@@n",        liftUnion (IntegerConstant 16))
  ,("@m",             liftUnion (IntegerConstant 1000))
  ,("@MM",            liftUnion (IntegerConstant 20000))
  ,("active",         liftUnion (IntegerConstant 13))
  ,("bgroup",         liftUnion (Character '{' CCBeginGroup))
  ,("egroup",         liftUnion (Character '}' CCEndGroup))

   -- LaTeX
  ,("@xxxii",         liftUnion (IntegerConstant 32))
  ,("@Mi",            liftUnion (IntegerConstant 10001))
  ,("@Mii",           liftUnion (IntegerConstant 10002))
  ,("@Miii",          liftUnion (IntegerConstant 10003))
  ,("@Miv",           liftUnion (IntegerConstant 10004))
  ]
