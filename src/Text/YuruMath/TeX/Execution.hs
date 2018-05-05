{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.YuruMath.TeX.Execution where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Tokenizer
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Expansion
import Data.Char
import Data.Word
import Data.Bits
import Data.Text (Text)
import Control.Monad
import Control.Monad.Error.Class
import qualified Data.Map.Strict as Map
import Control.Lens.At (at)
import Control.Lens.Getter (view,use,uses)
import Control.Lens.Setter (assign,modifying)

letCommand :: (MonadTeXState a m, MonadError String m) => m ()
letCommand = do
  name <- readCommandName
  readEquals
  t <- required nextEToken
  t <- case t of
    ETCharacter _ CCSpace -> required nextEToken -- one optional space
    _ -> return t
  v <- meaning t
  assign (localState . definitionAt name) v

futureletCommand :: (MonadTeXState a m, MonadError String m) => m ()
futureletCommand = do
  name <- readCommandName
  t1 <- required nextEToken
  t2 <- required nextEToken
  unreadETokens 0 [t1,t2]
  v <- meaning t2
  assign (localState . definitionAt name) v

uppercaseCommand :: (MonadTeXState a m, MonadError String m) => m ()
uppercaseCommand = do
  text <- readGeneralText
  toUpper <- ucCodeFn
  let makeUpper (TTCharacter c cc) | d <- toUpper c, d /= '\0' = TTCharacter d cc
      makeUpper t = t
  let text' = map makeUpper text
  unreadETokens 0 (map toEToken text')

lowercaseCommand :: (MonadTeXState a m, MonadError String m) => m ()
lowercaseCommand = do
  text <- readGeneralText
  toLower <- lcCodeFn
  let makeLower (TTCharacter c cc) | d <- toLower c, d /= '\0' = TTCharacter d cc
      makeLower t = t
  let text' = map makeLower text
  unreadETokens 0 (map toEToken text')

-- \chardef<control sequence><equals><number>
chardefCommand :: (MonadTeXState a m, MonadError String m) => m ()
chardefCommand = do
  name <- readCommandName
  readEquals
  c <- readUnicodeScalarValue
  let w = DefinedCharacter c
  assign (localState . definitionAt name) (Right w)

-- \mathchardef<control sequence><equals><15-bit number>
mathchardefCommand :: (MonadTeXState a m, MonadError String m) => m ()
mathchardefCommand = do
  name <- readCommandName
  readEquals
  v <- readNumber
  unless (0 <= v && v <= 0x8000)
    $ throwError $ "\\mathchardef: Bad math code (" ++ show v ++ ")"
  let w = DefinedMathCharacter (MathCode (fromInteger v))
  assign (localState . definitionAt name) (Right w)

-- \Umathchardef<control sequence><equals><3-bit number><8-bit number><21-bit number>
umathchardefCommand :: (MonadTeXState a m, MonadError String m) => m ()
umathchardefCommand = do
  name <- readCommandName
  readEquals
  mathclass <- readNumber
  unless (0 <= mathclass && mathclass <= 7)
    $ throwError "\\Umathchardef: Invalid math code"
  fam <- readNumber
  unless (0 <= fam && fam <= 0xFF)
    $ throwError "\\Umathchardef: Invalid math code"
  c <- readUnicodeScalarValue
  let w = DefinedMathCharacter $ mkUMathCode (toEnum $ fromIntegral mathclass) (fromIntegral fam) c
  assign (localState . definitionAt name) (Right w)

-- \Umathcharnumdef<control sequence><equals><32-bit number>
umathcharnumdefCommand :: (MonadTeXState a m, MonadError String m) => m ()
umathcharnumdefCommand = do
  name <- readCommandName
  readEquals
  value <- readNumber
  unless (-2^31 <= value && value < 2^31)
    $ throwError "\\Umathcharnumdef: Number too big"
  let valueu = fromIntegral value :: Word32
      -- mathclass = 0x7 .&. (valueu `shiftR` 21)
      -- fam = 0xFF .&. (valueu `shiftR` 24)
      code = 0x1FFFFF .&. valueu
  unless (isUnicodeScalarValue code)
    $ throwError "\\Umathcharnumdef: Invalid math code"
  let w = DefinedMathCharacter $ UMathCode (fromIntegral valueu)
  assign (localState . definitionAt name) (Right w)

-- \countdef, \dimendef, \muskipdef, \skipdef, \toksdef

-- \mathcode<21-bit number><equals><15-bit number>
mathcodeSet :: (MonadTeXState a m, MonadError String m) => m ()
mathcodeSet = do
  slot <- readUnicodeScalarValue
  readEquals
  v <- readNumber
  unless (0 <= v && v <= 0x8000)
    $ throwError $ "\\mathcode: Bad math code (" ++ show v ++ ")"
  let w = MathCode (fromInteger v)
  assign (localState . mathcodeMap . at slot) (Just w)

-- \UmathcodenumSet

-- \delcode<21-bit number><equals><24-bit number>
delcodeSet :: (MonadTeXState a m, MonadError String m) => m ()
delcodeSet = do
  slot <- readUnicodeScalarValue
  readEquals
  v <- readNumber
  unless (-1 <= v && v <= 0xFFFFFF)
    $ throwError $ "\\delcode: Invalid delimiter code."
  let w = DelimiterCode (fromIntegral v)
  assign (localState . delcodeMap . at slot) (Just w)

-- \Udelcode<21-bit number><equals><8-bit number><21-bit number>
udelcodeSet :: (MonadTeXState a m, MonadError String m) => m ()
udelcodeSet = do
  slot <- readUnicodeScalarValue
  readEquals
  fam <- readNumber
  unless (0 <= fam && fam <= 0xFF)
    $ throwError "\\Udelcode: Invalid delimiter code."
  c <- readUnicodeScalarValue
  let w = mkUDelCode (fromInteger fam) c
  assign (localState . delcodeMap . at slot) (Just w)

-- \Udelcodenum<21-bit number><equals><32-bit number>
udelcodenumSet :: (MonadTeXState a m, MonadError String m) => m ()
udelcodenumSet = do
  slot <- readUnicodeScalarValue
  readEquals
  value <- readNumber
  unless (-2^31 <= value && value < 2^31)
    $ throwError "\\Udelcodenum: Number too big"
  unless (0 <= value && value < 2^29)
    $ throwError "\\Udelcodenum: Invalid delimiter code"
  let valueu = fromIntegral value :: Word32
      -- fam = 0xFF .&. (valueu `shiftR` 21)
      code = 0x1FFFFF .&. valueu
  unless (isUnicodeScalarValue code)
    $ throwError "\\Udelcodenum: Invalid delimiter code"
  let w = UDelimiterCode (fromIntegral valueu)
  assign (localState . delcodeMap . at slot) (Just w)

defCommand :: (MonadTeXState a m, MonadError String m) => m ()
defCommand = do
  cs <- required nextEToken
  throwError "\\def: not implemented yet"
