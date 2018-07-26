{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.YuruMath.TeX.Expansion where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Quantity
import Text.YuruMath.TeX.Meaning
import qualified Text.YuruMath.TeX.Tokenizer as Tok (nextToken)
import Text.YuruMath.TeX.State
import Data.Int
import Data.Char
import Data.Ratio
import Data.Bits
import qualified Data.Text as T
import Data.Monoid ((<>))
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Control.Lens.Getter (use,view)
import Control.Lens.Setter (assign,modifying,set)

toEToken :: TeXToken -> ExpansionToken
toEToken (TTCommandName name) = ETCommandName { etDepth = 0, etFlavor = ECNFPlain, etName = name }
toEToken (TTCharacter c cc) = ETCharacter { etDepth = 0, etChar = c, etCatCode = cc }

fromEToken :: ExpansionToken -> TeXToken
fromEToken (ETCommandName { etName = name }) = TTCommandName name -- etFlavor is ignored
fromEToken (ETCharacter { etChar = c, etCatCode = cc }) = TTCharacter c cc

nextUnexpandedToken :: (MonadState s m, IsState s, MonadError String m) => m (Maybe ExpansionToken)
nextUnexpandedToken = do
  pending <- use esPendingTokenList
  case pending of
    [] -> do
      t <- Tok.nextToken
      case t of
        Nothing -> do
          is <- use inputStateStack
          case is of
            _:xs@(_:_) -> do -- more in the input stack
              assign inputStateStack xs
              nextUnexpandedToken
            _ -> return Nothing
        Just t -> return (Just (toEToken t))
    t:ts -> do
      assign esPendingTokenList ts
      return (Just t)

unreadTokens' :: (MonadReader Context m, MonadState s m, IsState s, MonadError String m) => [ExpansionToken] -> m ()
unreadTokens' ts = do
  limit <- view maxPendingTokenListLength
  ts' <- use esPendingTokenList
  when (length ts' + length ts > limit) $ throwError "token list too long"
  assign esPendingTokenList (ts ++ ts')

unreadTokens :: (MonadReader Context m, MonadState s m, IsState s, MonadError String m) => Int -> [ExpansionToken] -> m ()
unreadTokens !depth ts = do
  limit <- view maxPendingTokenListLength
  ts' <- use esPendingTokenList
  maxDepth <- view maxExpansionDepth
  when (depth >= maxDepth) $ throwError "recursion too deep"
  when (length ts' + length ts > limit) $ throwError "token list too long"
  assign esPendingTokenList (map (\t -> t { etDepth = depth }) ts ++ ts')

unreadToken :: (MonadReader Context m, MonadState s m, IsState s, MonadError String m) => ExpansionToken -> m ()
unreadToken t = do
  limit <- view maxPendingTokenListLength
  ts' <- use esPendingTokenList
  maxDepth <- view maxExpansionDepth
  when (etDepth t >= maxDepth) $ throwError "recursion too deep"
  when (length ts' + 1 > limit) $ throwError "token list too long"
  assign esPendingTokenList (t : ts')

readUntilEndGroupE :: (MonadState s m, IsState s, MonadError String m) => ParamLong -> m [ExpansionToken]
readUntilEndGroupE !long = loop (0 :: Int) []
  where
    loop !depth revTokens = do
      t <- nextUnexpandedToken
      case t of
        Nothing -> throwError "unexpected end of input when reading an argument"
        Just t@(ETCharacter { etCatCode = CCEndGroup })
          | depth == 0 -> return (reverse revTokens)
          | otherwise -> loop (depth - 1) (t : revTokens)
        Just t@(ETCharacter { etCatCode = CCBeginGroup })
          -> loop (depth + 1) (t : revTokens)
        Just (ETCommandName { etName = NControlSeq "par" })
          | long == ShortParam -> throwError "Paragraph ended before argument was compelete"
        Just t -> loop depth (t : revTokens)

readUntilEndGroup :: (MonadState s m, IsState s, MonadError String m) => ParamLong -> m [TeXToken]
readUntilEndGroup !long = map fromEToken <$> readUntilEndGroupE long

-- Used by \edef
edefReadUntilEndGroupE :: forall s m. (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
edefReadUntilEndGroupE = loop (0 :: Int) []
  where
    loop :: Int -> [ExpansionToken] -> m [ExpansionToken]
    loop !depth revTokens = do
      t <- required nextUnexpandedToken
      case t of
        ETCharacter { etCatCode = CCEndGroup }
          | depth == 0 -> return (reverse revTokens)
          | otherwise -> loop (depth - 1) (t : revTokens)
        ETCharacter { etCatCode = CCBeginGroup }
          -> loop (depth + 1) (t : revTokens)
        ETCommandName { etFlavor = ECNFPlain, etName = name } -> do
          m <- use (localState . definitionAt name)
          case m of
            -- undefined
            Nothing -> undefinedControlSequence name

            -- expandable
            Just (Left e) ->
              case doExpandInEdef e of
                Nothing -> do
                  r <- doExpand e t
                  unreadTokens (etDepth t + 1) r
                  loop depth revTokens
                Just m -> do r <- m t
                             -- the result should be balanced text
                             loop depth (reverse r ++ revTokens)

            -- non-expandable
            Just (Right _) -> loop depth (t : revTokens) -- unexpandable

        -- character, \noexpand-ed name, inserted \relax
        t -> loop depth (t : revTokens) -- noexpand flag should be stripped later

edefReadUntilEndGroup :: (MonadTeXState s m, MonadError String m) => m [TeXToken]
edefReadUntilEndGroup = map fromEToken <$> edefReadUntilEndGroupE

-- reads undelimited macro argument
readArgument :: (MonadState s m, IsState s, MonadError String m) => ParamLong -> m [TeXToken]
readArgument !long = do
  t <- nextUnexpandedToken
  case t of
    Nothing -> throwError "unexpected end of input when expecting an argument"
    Just (ETCharacter { etCatCode = CCSpace }) -> readArgument long
    Just (ETCharacter { etCatCode = CCEndGroup }) -> throwError "unexpected end of group"
    Just (ETCharacter { etCatCode = CCBeginGroup }) -> readUntilEndGroup long
    Just (ETCommandName { etName = NControlSeq "par" })
      | long == ShortParam -> throwError "Paragraph ended before argument was compelete"
    Just t -> return [fromEToken t]

-- reads a control sequence or an active character
readCommandName :: (MonadState s m, IsState s, MonadError String m) => m CommandName
readCommandName = do
  t <- required nextUnexpandedToken
  case t of
    ETCommandName { etName = name } -> return name
    _ -> throwError $ "unexpected character token: " ++ show t
         -- or, "Missing control sequence inserted"

-- Used by \let
readUnexpandedOneOptionalSpace :: (MonadTeXState s m, MonadError String m) => m ()
readUnexpandedOneOptionalSpace = do
  t <- nextUnexpandedToken
  case t of
    Just (ETCharacter { etCatCode = CCSpace }) ->
      return () -- consumed
    Just t -> do
      v <- meaningWithoutExpansion t
      case v of
        Just (Right v) | isImplicitSpace v -> return () -- consume a space
        _ -> unreadToken t
    Nothing -> return ()

readOneOptionalSpace :: (MonadTeXState s m, MonadError String m) => m ()
readOneOptionalSpace = do
  t <- nextExpandedToken
  case t of
    Just (ETCharacter { etCatCode = CCSpace },_) ->
      return () -- consumed
    Just (t,v) | isImplicitSpace v -> return () -- consume a space
               | otherwise -> unreadToken t
    Nothing -> return ()

readOptionalSpaces :: (MonadTeXState s m, MonadError String m) => m ()
readOptionalSpaces = do
  t <- nextExpandedToken
  case t of
    Just (ETCharacter { etCatCode = CCSpace },_) ->
      readOptionalSpaces -- consumed, read more
    Just (t,v) | isImplicitSpace v -> readOptionalSpaces -- consume a space, and read more
               | otherwise -> unreadToken t
    Nothing -> return ()

-- <equals> ::= <optional spaces> | <optional spaces>'='12
readEquals :: (MonadTeXState s m, MonadError String m) => m ()
readEquals = do
  t <- nextExpandedToken
  case t of
    Just (ETCharacter { etChar = '=', etCatCode = CCOther },_) -> return () -- consume equals
    Just (ETCharacter { etCatCode = CCSpace },_) -> readEquals -- consume a space, and read more
    Just (t,v) | isImplicitSpace v -> readEquals -- consume a space, and read more
               | otherwise -> unreadToken t
    Nothing -> return ()

-- Used by \let
readUnexpandedEquals :: (MonadTeXState s m, MonadError String m) => m ()
readUnexpandedEquals = do
  t <- nextUnexpandedToken
  case t of
    Just (ETCharacter { etChar = '=', etCatCode = CCOther }) -> return () -- consume equals
    Just (ETCharacter { etCatCode = CCSpace }) -> readUnexpandedEquals -- consume a space, and read more
    Just t -> do
      v <- meaningWithoutExpansion t
      case v of
        Just (Right v) | isImplicitSpace v -> readUnexpandedEquals -- consume a space, and read more
        _ -> unreadToken t
    Nothing -> return ()

-- read a number between 0.."10FFFF excluding "D800.."DFFF, and convert it to a Char
-- Note: Although neither LuaTeX nor XeTeX seems to forbid surrogate codes ("D800-"DFFF), we do.
readUnicodeScalarValue :: (MonadTeXState s m, MonadError String m) => m Char
readUnicodeScalarValue = do
  x <- readNumber
  if isUnicodeScalarValue x
    then return $ chr $ fromIntegral x
    else throwError $ "Bad character code (" ++ show x ++ ")"

-- try to read a keyword
readKeyword :: (MonadTeXState s m, MonadError String m) => String -> m Bool
readKeyword xs = do
  readOptionalSpaces
  loop xs
  where
    loop [] = return True
    loop (x:xs) = do
      t <- nextExpandedToken
      case fst <$> t of
        Just t@(ETCharacter { etChar = c })
          | x == c || x == toLower c -> do
              r <- loop xs
              if r
                then return True
                else do unreadToken t
                        return False
        Just t -> do unreadToken t
                     return False
        Nothing -> return False

readOptionalKeyword :: (MonadTeXState s m, MonadError String m) => String -> m ()
readOptionalKeyword name = do _ <- readKeyword name
                              return ()

-- read an optional keyword and return the associated value with it
readOneOfKeywordsV :: (MonadTeXState s m, MonadError String m) => [(String,v)] -> m (Maybe v)
readOneOfKeywordsV keywords = do
  readOptionalSpaces
  loop keywords
  where
    loop [] = return Nothing
    loop keywords | Just v <- lookup "" keywords = return (Just v)
    loop keywords = do
      t <- nextExpandedToken
      case fst <$> t of
        Just t@(ETCharacter { etChar = c }) -> do
          k <- loop [(xs,v) | (x:xs,v) <- keywords, x == c || x == toLower c]
          case k of
            Just _ -> return k
            Nothing -> do unreadToken t
                          return Nothing
        Just t -> do unreadToken t
                     return Nothing
        Nothing -> return Nothing

readKeywordArguments :: (MonadTeXState s m, MonadError String m, Monoid n) => [(String,m n)] -> m n
readKeywordArguments keywords = doRead mempty $ map (\(k,v) -> (k,(k,v))) keywords
  where
    doRead !acc argSpec = do
      k <- readOneOfKeywordsV argSpec
      case k of
        Just (w,action) -> do
          v <- action
          doRead (acc <> v) [a | a <- argSpec, fst a /= w]
        _ -> return acc

undefinedControlSequence :: (MonadTeXState s m, MonadError String m) => CommandName -> m a
undefinedControlSequence name = do
  throwErrorMessage ("Undefined contorl sequence: " <> showCommandName name)

-- used by \expandafter
expandOnce :: (MonadTeXState s m, MonadError String m, DoExpand (Expandable s) m) => ExpansionToken -> m [ExpansionToken]
expandOnce t@(ETCommandName { etFlavor = ECNFPlain, etName = name }) = do
  m <- use (localState . definitionAt name)
  case m of
    Nothing -> undefinedControlSequence name
    Just (Left e) -> doExpand e t
    Just (Right _) -> return [t]
expandOnce t@(ETCommandName { etFlavor = ECNFNoexpand }) = do
  return [t { etFlavor = ECNFPlain }]
expandOnce t = return [t]

-- Do a repeated expansion of the token and return the resulting unexpandable token and its meaning
nextExpandedToken :: forall s m. (MonadTeXState s m, MonadError String m) => m (Maybe (ExpansionToken,NValue s))
nextExpandedToken = loop
  where
    loop :: m (Maybe (ExpansionToken,NValue s))
    loop = do
      t <- nextUnexpandedToken
      case t of
        -- a command name
        Just t@(ETCommandName { etFlavor = ECNFPlain, etName = name }) -> do
          m <- use (localState . definitionAt name)
          case m of
            -- undefined
            Nothing -> undefinedControlSequence name

            -- expandable
            Just (Left e) -> do
              r <- doExpand e t
              unreadTokens (etDepth t + 1) r
              loop

            -- non-expandable
            Just (Right v) -> return $ Just (t,v)

        -- a token whose meaning is \relax (inserted by \else, \fi or \or)
        Just t@(ETCommandName { etFlavor = ECNFIsRelax }) -> do
          return $ Just (t,injectCommonValue Relax)

        -- a token yielded by \noexpand
        Just t@(ETCommandName { etFlavor = ECNFNoexpand }) -> do
          return $ Just (t,injectCommonValue Relax)

        -- non-active character
        Just t@(ETCharacter { etChar = c, etCatCode = cc }) ->
          return $ Just (t,injectCommonValue $ Character c cc)

        Nothing -> return Nothing

required :: (MonadError String m) => m (Maybe a) -> m a
required m = do a <- m
                case a of
                  Nothing -> throwError "unexpected end of input"
                  Just a -> return a

-- used by \csname and \ifcsname
readUntilEndcsname :: (MonadTeXState s m, MonadError String m) => m String
readUntilEndcsname = local (set isInCsname True) $ loop []
  where
    loop revName = do
      (t,v) <- required nextExpandedToken
      case toCommonValue v of
        Just Endcsname -> return (reverse revName)
        _ -> case t of
          ETCommandName { etName = name } -> throwError $ "unexpected " ++ show name ++ " while looking for \\endcsname" -- not expandable, or \noexpand-ed
          ETCharacter { etChar = c } -> loop (c:revName) -- non-active character

stringToEToken :: String -> [ExpansionToken]
stringToEToken = map charToEToken
  where charToEToken ' ' = ETCharacter { etDepth = 0, etChar = ' ', etCatCode = CCSpace }
        charToEToken x   = ETCharacter { etDepth = 0, etChar = x, etCatCode = CCOther }

stringToToken :: String -> [TeXToken]
stringToToken = map charToToken
  where charToToken ' ' = TTCharacter ' ' CCSpace
        charToToken x   = TTCharacter x CCOther

-- <optional signs> ::= <optional spaces> | <optional signs><plus or minus><optional spaces>
readOptionalSigns :: (MonadTeXState s m, MonadError String m) => Int -> m Int
readOptionalSigns !s = do
  (t,v) <- required nextExpandedToken
  case t of
    ETCharacter { etChar = '+', etCatCode = CCOther } -> readOptionalSigns s
    ETCharacter { etChar = '-', etCatCode = CCOther } -> readOptionalSigns (-s)
    _ | isImplicitSpace v -> readOptionalSigns s -- space: ignored
      | otherwise -> unreadToken t >> return s

readUnsignedDecimalInteger :: forall s m. (MonadTeXState s m, MonadError String m) => Char -> m Integer
readUnsignedDecimalInteger !c = readRest (fromIntegral (digitToInt c))
  where
    readRest :: Integer -> m Integer
    readRest !x = do
      m <- nextExpandedToken
      case m of
        Just (ETCharacter { etChar = c, etCatCode = CCOther },_)
          | isDigit c -> readRest (10 * x + fromIntegral (digitToInt c))
        Just (t,v) | isImplicitSpace v -> return x -- space: consumed
                   | otherwise -> unreadToken t >> return x
        Nothing -> return x

readUnsignedOctal :: forall s m. (MonadTeXState s m, MonadError String m) => m Integer
readUnsignedOctal = do
  (t,_) <- required nextExpandedToken
  case t of
    ETCharacter { etChar = c, etCatCode = CCOther }
      | isOctDigit c -> readRest (fromIntegral (digitToInt c))
    _ -> throwError $ "unexpected token while reading octal: " ++ show t
  where
    readRest :: Integer -> m Integer
    readRest !x = do
      m <- nextExpandedToken
      case m of
        Just (ETCharacter { etChar = c, etCatCode = CCOther },_)
          | isOctDigit c -> readRest (8 * x + fromIntegral (digitToInt c))
        Just (t,v) | isImplicitSpace v -> return x -- consumed
                   | otherwise -> unreadToken t >> return x
        Nothing -> return x

readUnsignedHex :: forall s m. (MonadTeXState s m, MonadError String m) => m Integer
readUnsignedHex = do
  (t,_) <- required nextExpandedToken
  case t of
    ETCharacter { etChar = c, etCatCode = CCOther }
      | isUpperHexDigit c -> readRest (fromIntegral (digitToInt c))
    ETCharacter { etChar = c, etCatCode = CCLetter }
      | isHexDigit c && isAsciiUpper c ->
          readRest (fromIntegral (digitToInt c))
    _ -> throwError $ "unexpected token while reading hexadecimal: " ++ show t
  where
    readRest :: Integer -> m Integer
    readRest !x = do
      m <- nextExpandedToken
      case m of
        Just (ETCharacter { etChar = c, etCatCode = CCOther },_)
          | isUpperHexDigit c -> readRest (16 * x + fromIntegral (digitToInt c))
        Just (ETCharacter { etChar = c, etCatCode = CCLetter },_)
          | isHexDigit c && isAsciiUpper c ->
              readRest (16 * x + fromIntegral (digitToInt c))
        Just (t,v) | isImplicitSpace v -> return x -- consumed
                   | otherwise -> unreadToken t >> return x
        Nothing -> return x
    isUpperHexDigit c = isHexDigit c && (isDigit c || isAsciiUpper c)

readCharacterCode :: (MonadTeXState s m, MonadError String m) => m Integer
readCharacterCode = do
  t <- required nextUnexpandedToken -- without expansion
  readOneOptionalSpace
  case t of
    ETCommandName { etName = NControlSeq name } -> case T.unpack name of
      [c] -> return (fromIntegral $ ord c)
      _ -> throwError "Improper alphabetic constant."
    ETCommandName { etName = NActiveChar c } -> return (fromIntegral $ ord c)
    ETCharacter { etChar = c } -> return (fromIntegral $ ord c)

-- <number> ::= <optional signs><unsigned number>
-- <unsigned number> ::= <normal integer> | <coerced integer>
-- <normal integer> ::= <internal integer>
--                    | <integer constant><one optional space>
--                    | '\''12<octal constant><one optional space>
--                    | '"'12<hexadecimal constant><one optional space>
--                    | '`'12<character token><one optional space>
-- <integer constant> ::= <digit> | <digit><integer constant>
readNumber :: (MonadTeXState s m, MonadError String m) => m Integer
readNumber = do
  sign <- readOptionalSigns 1
  (t,v) <- required nextExpandedToken
  (fromIntegral sign *) <$> case t of
    ETCharacter { etChar = '\'', etCatCode = CCOther } -> readUnsignedOctal
    ETCharacter { etChar = '"', etCatCode = CCOther } -> readUnsignedHex
    ETCharacter { etChar = '`', etCatCode = CCOther } -> readCharacterCode
    ETCharacter { etChar = c, etCatCode = CCOther } | isDigit c -> readUnsignedDecimalInteger c
    _ -> case getQuantity v of
           QInteger getInteger -> getInteger
           QDimension getDimension -> asScaledPoints <$> getDimension
           QGlue getGlue -> (asScaledPoints . glueSpace) <$> getGlue
           _ -> throwError $ "Unexpected token while reading number: " ++ show t -- Missing number, treated as zero.

readGeneralInt :: (MonadTeXState s m, MonadError String m, IntegralB i) => m i
readGeneralInt = do
  x <- readNumber
  case maybeFromInteger x of
    Just x -> return x
    Nothing
      | x < -2^(31::Int) || 2^(31::Int) <= x ->
        throwError "Number too big"
      | otherwise ->
        -- maybe another message? (like "Out of range")
        throwError "Number too big"

readInt32 :: (MonadTeXState s m, MonadError String m) => m Int32
readInt32 = do
  x <- readNumber
  case maybeFromInteger x of
    Just x -> return x
    Nothing -> throwError "Number too big"

readIntBetween :: (MonadTeXState s m, MonadError String m) => Int -> Int -> m Int
readIntBetween lo hi = do
  x <- readNumber
  if fromIntegral lo <= x && x <= fromIntegral hi
    then return (fromInteger x)
    else if x < -2^(31::Int) || 2^(31::Int) <= x
         then throwError "Number too big"
         else throwError "Out of range"

readUnsignedDecimalFraction :: forall s m. (MonadTeXState s m, MonadError String m) => Char -> m Rational
readUnsignedDecimalFraction c
  | c == '.' || c == ',' = readFractionPart 0 0
  | otherwise = readIntegerPart (fromIntegral (digitToInt c)) -- c should be a digit
  where
    readIntegerPart :: Integer -> m Rational
    readIntegerPart !x = do
      m <- nextExpandedToken
      case m of
        Just (ETCharacter { etChar = '.', etCatCode = CCOther },_) -> readFractionPart x 0
        Just (ETCharacter { etChar = ',', etCatCode = CCOther },_) -> readFractionPart x 0
        Just (ETCharacter { etChar = c, etCatCode = CCOther },_)
          | isDigit c -> readIntegerPart (10 * x + fromIntegral (digitToInt c))
        Just (t,_) -> unreadToken t >> return (fromInteger x)
        Nothing -> return (fromInteger x)
    readFractionPart :: Integer -> Int -> m Rational
    readFractionPart !intPart !expPart = do
      m <- nextExpandedToken
      case m of
        Just (ETCharacter { etChar = c, etCatCode = CCOther },_)
          | isDigit c -> readFractionPart (10 * intPart + fromIntegral (digitToInt c)) (expPart + 1)
        Just (t,_) -> unreadToken t >> return (intPart % 10^expPart)
        Nothing -> return (intPart % 10^expPart)

class DimenRead f where
  doUnit :: (MonadTeXState s m, MonadError String m) => (Rational -> m a) -> Rational -> m (f a)
  fixedDimen :: a -> f a
  negateDim :: (Quantity a) => f a -> f a

newtype SimpleDimen a = SimpleDimen { runSimpleDimen :: a }
instance DimenRead SimpleDimen where
  doUnit m !factor = SimpleDimen <$> m factor
  fixedDimen = SimpleDimen
  negateDim (SimpleDimen x) = SimpleDimen (negateQ x)

instance DimenRead StretchShrink where
  doUnit m !factor = do
    fil <- readKeyword "fil"
    if fil
      then readFil 0
      else FixedSS <$> m factor
      where readFil !i = do l <- readKeyword "l"
                            if l
                              then readFil (i + 1)
                              else return (InfiniteSS (truncate (65536 * factor)) i)
  fixedDimen = FixedSS
  negateDim = negateQ

-- <dimen> ::= <optional signs><unsigned dimen>
-- <unsigned dimen> ::= <normal dimen> | <coerced dimen>
-- <normal dimen> ::= <internal dimen> | <factor><unit of measure>
-- <coerced dimen> ::= <internal glue>
-- <unit of measure> ::= <optional spaces><internal unit>
--                     | <optional "true"><physical unit><one optional space>
-- <internal unit> ::= "em"<one optional space> | "ex"<one optional space>
--                   | <internal integer> | <internal dimen> | <internal glue>
-- <optional "true"> ::= "true" | <empty>
-- <physical unit> ::= "pt" | "pc" | "in" | "bp" | "cm" | "mm" | "dd" | "cc" | "sp"
readDimensionF :: (MonadTeXState s m, MonadError String m, DimenRead f) => m (f Dimen)
readDimensionF = do
  -- read <optional signs>
  sign <- readOptionalSigns 1
  let applySign | sign > 0 = id
                | otherwise = negateDim
  -- read <internal dimen> or <factor> or <internal glue>
  (t,v) <- required nextExpandedToken
  applySign <$> case t of
    ETCharacter { etChar = '\'', etCatCode = CCOther } ->
      (fromInteger <$> readUnsignedOctal) >>= doUnit readDimenUnit
    ETCharacter { etChar = '"', etCatCode = CCOther } ->
      (fromInteger <$> readUnsignedHex) >>= doUnit readDimenUnit
    ETCharacter { etChar = '`', etCatCode = CCOther } ->
      (fromInteger <$> readCharacterCode) >>= doUnit readDimenUnit
    ETCharacter { etChar = c, etCatCode = CCOther }
      | isDigit c || c == '.' || c == ',' ->
          readUnsignedDecimalFraction c >>= doUnit readDimenUnit
    _ -> case getQuantity v of
           QInteger getInteger -> (fromInteger <$> getInteger) >>= doUnit readDimenUnit
           QDimension getDimen -> fixedDimen <$> getDimen
           QGlue getGlue -> (fixedDimen . glueSpace) <$> getGlue
           _ -> throwError $ "Unexpected " ++ show t ++ " while reading dimension"
  where
    readDimenUnit !factor = do
      -- read <unit of measure>
      readOptionalSpaces
      -- "em" | "ex" | "true"<physical unit> | <physical unit> | <internal integer> | <internal dimen> | <internal glue>
      (t,v) <- required nextExpandedToken
      case getQuantity v of
        QInteger getInteger -> do v <- getInteger
                                  return (sp (factor * fromInteger v))
        QDimension getDimen -> scaleByRational factor <$> getDimen
        QGlue getGlue -> (scaleByRational factor . glueSpace) <$> getGlue
        _ -> do unreadToken t
                true <- readKeyword "true"
                let physicalUnits = [("pt",pt)
                                    ,("pc",pc)
                                    ,("in",inch)
                                    ,("bp",bp)
                                    ,("cm",cm)
                                    ,("mm",mm)
                                    ,("dd",dd)
                                    ,("cc",cc)
                                    ,("sp",sp)
                                    ]
                    relativeUnits = [("em",\x -> pt (10 * x))  -- Assume 1em = 10pt
                                    ,("ex",\x -> pt (4.3 * x)) -- Assume 1ex = 4.3pt
                                    ]
                -- if true, read <physical unit>
                -- otherwise, read "em" | "ex" | <physical unit>
                kw <- readOneOfKeywordsV $ if true
                                           then physicalUnits
                                           else physicalUnits ++ relativeUnits
                readOneOptionalSpace
                case kw of
                  Just unit -> return $ unit factor
                  Nothing -> throwError "Illegal unit of measure"

readDimension :: (MonadTeXState s m, MonadError String m) => m Dimen
readDimension = runSimpleDimen <$> readDimensionF

-- <mudimen> ::= <optional signs><unsigned mudimen>
-- <unsigned mudimen> ::= <normal mudimen> | <coerced mudimen>
-- <coerced mudimen> ::= <internal muglue>
-- <normal mudimen> ::= <factor><mu unit>
-- <mu unit> ::= <optional spaces><internal muglue> | "mu"<one optional space>
readMuDimensionF :: (MonadTeXState s m, MonadError String m, DimenRead f) => m (f MuDimen)
readMuDimensionF = do
  -- read <optional signs>
  sign <- readOptionalSigns 1
  let applySign | sign > 0 = id
                | otherwise = negateDim
  -- read <factor> or <internal muglue>
  (t,v) <- required nextExpandedToken
  applySign <$> case t of
    ETCharacter { etChar = '\'', etCatCode = CCOther } ->
      (fromInteger <$> readUnsignedOctal) >>= doUnit readMuUnit
    ETCharacter { etChar = '"', etCatCode = CCOther } ->
      (fromInteger <$> readUnsignedHex) >>= doUnit readMuUnit
    ETCharacter { etChar = '`', etCatCode = CCOther } ->
      (fromInteger <$> readCharacterCode) >>= doUnit readMuUnit
    ETCharacter { etChar = c, etCatCode = CCOther }
      | isDigit c || c == '.' || c == '.' ->
        (readUnsignedDecimalFraction c) >>= doUnit readMuUnit
    _ -> case getQuantity v of
           QInteger getInteger -> (fromInteger <$> getInteger) >>= doUnit readMuUnit
           QMuGlue getMuGlue -> (fixedDimen . glueSpace) <$> getMuGlue
           _ -> throwError $ "Unexpected " ++ show t ++ " while reading mu-dimension"
  where
    readMuUnit !factor = do
      muKeyword <- readKeyword "mu"
      if muKeyword
        then readOneOptionalSpace >> return (mu factor)
        else do (_,v) <- required nextExpandedToken
                case getQuantity v of
                  QMuGlue getMuGlue -> (scaleByRational factor . glueSpace) <$> getMuGlue
                  _ -> throwError "Illegal unit of measure"

readMuDimension :: (MonadTeXState s m, MonadError String m) => m MuDimen
readMuDimension = runSimpleDimen <$> readMuDimensionF

readGlue :: (MonadTeXState s m, MonadError String m) => m (Glue Dimen)
readGlue = do
  sign <- readOptionalSigns 1
  let applySign | sign > 0 = id
                | otherwise = negateQ
  (t,v) <- required nextExpandedToken
  applySign <$> case getQuantity v of
    QGlue getGlue -> getGlue
    _ -> do unreadToken t
            dimen <- readDimension
            plus <- readKeyword "plus"
            stretch <- if plus
                       then readDimensionF
                       else pure zeroQ
            minus <- readKeyword "minus"
            shrink <- if minus
                      then readDimensionF
                      else pure zeroQ
            return $ Glue { glueSpace = dimen
                          , glueStretch = stretch
                          , glueShrink = shrink
                          }

readMuGlue :: (MonadTeXState s m, MonadError String m) => m (Glue MuDimen)
readMuGlue = do
  sign <- readOptionalSigns 1
  let applySign | sign > 0 = id
                | otherwise = negateQ
  (t,v) <- required nextExpandedToken
  applySign <$> case getQuantity v of
    QMuGlue getMuGlue -> getMuGlue
    _ -> do unreadToken t
            dimen <- readMuDimension
            plus <- readKeyword "plus"
            stretch <- if plus
                       then readMuDimensionF
                       else pure zeroQ
            minus <- readKeyword "minus"
            shrink <- if minus
                      then readMuDimensionF
                      else pure zeroQ
            return $ Glue { glueSpace = dimen
                          , glueStretch = stretch
                          , glueShrink = shrink
                          }

class (Quantity q) => QuantityRead q where
  readQuantity :: (MonadTeXState s m, MonadError String m) => m q

instance QuantityRead Integer where
  readQuantity = readNumber

instance QuantityRead Dimen where
  readQuantity = readDimension

instance QuantityRead MuDimen where
  readQuantity = readMuDimension

instance QuantityRead (Glue Dimen) where
  readQuantity = readGlue

instance QuantityRead (Glue MuDimen) where
  readQuantity = readMuGlue

-- \the, \showthe
theString :: (MonadTeXState s m, MonadError String m) => String -> m [TeXToken]
theString name = do
  (t,v) <- required nextExpandedToken
  case getQuantity v of
    QInteger getInteger ->
      (stringToToken . show) <$> getInteger
    QDimension getDimension ->
      (stringToToken . showDimension) <$> getDimension
    QGlue getGlue -> do
      Glue { glueSpace = x, glueStretch = stretch, glueShrink = shrink } <- getGlue
      return $ stringToToken $ showDimension x ++ showSS " plus " stretch ++ showSS " minus " shrink
    QMuGlue getMuGlue -> do
      Glue { glueSpace = x, glueStretch = stretch, glueShrink = shrink } <- getMuGlue
      return $ stringToToken $ showMuDimension x ++ showMuSS " plus " stretch ++ showMuSS " minus " shrink
    QToks getTokenList -> getTokenList
    _ -> throwError $ "You can't use `" ++ show t ++ "' after " ++ name

showDimension :: Dimen -> String
showDimension x = showScaledAsDecimal (asScaledPoints x) ++ "pt"

showMuDimension :: MuDimen -> String
showMuDimension x = showScaledAsDecimal (asScaledMu x) ++ "mu"

showSS :: String -> StretchShrink Dimen -> String
showSS prefix (FixedSS s) | s == zeroQ = ""
                          | otherwise = prefix ++ showDimension s
showSS prefix (InfiniteSS i l) = prefix ++ showScaledAsDecimal i ++ "fil" ++ replicate l 'l'

showMuSS :: String -> StretchShrink MuDimen -> String
showMuSS prefix (FixedSS s) | s == zeroQ = ""
                            | otherwise = prefix ++ showMuDimension s
showMuSS prefix (InfiniteSS i l) = prefix ++ showScaledAsDecimal i ++ "fil" ++ replicate l 'l'

showScaledAsDecimal :: Integer -> String
showScaledAsDecimal x
  | x < 0 = '-' : showScaledAsDecimal (- x)
  | otherwise = case quotRem (round (x * 100000 % 65536) :: Integer) 100000 of
                  (intPart,0) -> show intPart ++ ".0"
                  (intPart,fracPart) -> show intPart ++ "." ++ adjustFracPart (show fracPart)
  where
    adjustFracPart s | length s < 5 = stripTrailingZero (replicate (5 - length s) '0' ++ s)
                     | otherwise = stripTrailingZero s
    stripTrailingZero "" = ""
    stripTrailingZero "0" = ""
    stripTrailingZero (x:xs) = case x:stripTrailingZero xs of
                                 "0" -> ""
                                 y -> y

showRomannumeral :: Int -> String
showRomannumeral !x
  | x <= 0 = "" -- cannot be expressed
  | otherwise = let (e1,d1) = x `quotRem` 10
                    (e2,d2) = e1 `quotRem` 10
                    (d4,d3) = e2 `quotRem` 10
                in replicate d4 'm' ++ (a3 !! d3) ++ (a2 !! d2) ++ (a1 !! d1)
  where a3 = ["", "c", "cc", "ccc", "cd", "d", "dc", "dcc", "dccc", "cm"]
        a2 = ["", "x", "xx", "xxx", "xl", "l", "lx", "lxx", "lxxx", "xc"]
        a1 = ["", "i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix"]

-- True if encountered \else, False if encountered \fi
skipUntilElse :: (MonadTeXState s m, MonadError String m) => Int -> m Bool
skipUntilElse !level = do
  x <- required nextUnexpandedToken >>= meaningWithoutExpansion
  case x of
    Just (Left c)
      | Just m <- isConditionalMarker c -> case m of
          Eor | level == 0 -> throwError "Extra \\or"
          Eelse | level == 0 -> return True
          Efi | level == 0 -> return False
              | otherwise -> skipUntilElse (level - 1)
          _ -> skipUntilElse level -- Inner \else, \or
      | isConditional c -> skipUntilElse (level + 1)
    _ -> skipUntilElse level

skipUntilFi :: (MonadTeXState s m, MonadError String m) => Int -> m ()
skipUntilFi !level = do
  x <- required nextUnexpandedToken >>= meaningWithoutExpansion
  case x of
    Just (Left c)
      | isConditionalMarker c == Just Efi
        -> if level == 0
           then return ()
           else skipUntilFi (level - 1)
      | isConditional c -> skipUntilFi (level + 1)
    _ -> skipUntilFi level

data SkipUntilOr = FoundOr
                 | FoundElse
                 | FoundFi

skipUntilOr :: (MonadTeXState s m, MonadError String m) => Int -> m SkipUntilOr
skipUntilOr !level = do
  x <- required nextUnexpandedToken >>= meaningWithoutExpansion
  case x of
    Just (Left c)
      | Just m <- isConditionalMarker c -> case m of
          Eor | level == 0 -> return FoundOr
          Eelse | level == 0 -> return FoundElse
          Efi | level == 0 -> return FoundFi
              | otherwise -> skipUntilOr (level - 1)
          _ -> skipUntilOr level -- Inner \else, \or
      | isConditional c -> skipUntilOr (level + 1)
    _ -> skipUntilOr level

-- TODO: Do something for '\if\iftrue abc\else d\fi'
doBooleanConditional :: (MonadTeXState s m, MonadError String m) => Bool -> m ()
doBooleanConditional True = do
  modifying conditionalStack (\(CondTest:xs) -> CondTruthy:xs)
doBooleanConditional False = do
  e <- skipUntilElse 0
  if e
    then modifying conditionalStack (\(CondTest:xs) -> CondFalsy:xs)
    else modifying conditionalStack tail

expandBooleanConditional :: (MonadTeXState s m, MonadError String m) => m Bool -> m [ExpansionToken]
expandBooleanConditional c = do
  modifying conditionalStack (CondTest:)
  b <- c
  doBooleanConditional b
  return []

meaningWithoutExpansion :: (MonadState s m, IsState s, MonadError String m) => ExpansionToken -> m (Value s)
meaningWithoutExpansion t = do
  case t of
    ETCommandName { etFlavor = ECNFPlain, etName = name } -> use (localState . definitionAt name)
    ETCommandName { etFlavor = ECNFIsRelax }   -> return $ nonexpandableToValue Relax
    ETCommandName { etFlavor = ECNFNoexpand }  -> return $ nonexpandableToValue Relax
    ETCharacter { etChar = c, etCatCode = cc } -> return $ nonexpandableToValue $ Character c cc

-- Reads an explicit or implicit `{' (character with category code 2), and enters a new group
readLBrace :: forall s m. (MonadTeXState s m, MonadError String m) => m ()
readLBrace = loop
  where
    loop :: m ()
    loop = do
      (t,v) <- required nextExpandedToken
      case toCommonValue v of
        Just (Character _ CCSpace) -> loop -- ignore spaces
        Just (Character _ CCBeginGroup) -> do
          enterGroup ScopeByBrace
        _ -> throwError ("Expected `{', but got " ++ show t)

readFillerAndLBrace :: forall s m. (MonadTeXState s m, MonadError String m) => Bool -> m ()
readFillerAndLBrace !createScope = loop
  where
    loop :: m ()
    loop = do
      (t,v) <- required nextExpandedToken
      case toCommonValue v of
        Just (Character _ CCBeginGroup)
          | createScope -> enterGroup ScopeByBrace -- explicit or implict left brace
          | otherwise -> return ()
        Just (Character _ CCSpace) -> loop -- optional spaces: ignored
        Just Relax -> loop -- \relax: ignored
        _ -> throwError ("Expected `{', but got " ++ show t) -- Missing { inserted

readUnexpandedGeneralText :: (MonadTeXState s m, MonadError String m) => m [TeXToken]
readUnexpandedGeneralText = map fromEToken <$> readUnexpandedGeneralTextE

readUnexpandedGeneralTextE :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
readUnexpandedGeneralTextE = do
  readFillerAndLBrace False
  readUntilEndGroupE LongParam

readExpandedGeneralText :: (MonadTeXState s m, MonadError String m) => m [TeXToken]
readExpandedGeneralText = map fromEToken <$> readExpandedGeneralTextE

readExpandedGeneralTextE :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
readExpandedGeneralTextE = do
  readFillerAndLBrace True
  content <- edefReadUntilEndGroupE
  leaveGroup ScopeByBrace
  return content

-- \Umathchardef, \Umathcode, \Umathchar's math code: <0-7><0-255><21-bit number>
readUMathCodeTriplet :: (MonadTeXState s m, MonadError String m) => m MathCode
readUMathCodeTriplet = do
  mathclass <- readIntBetween 0 7 -- "Invalid math code"
  fam <- readIntBetween 0 0xFF -- "Invalid math code"
  slot <- readUnicodeScalarValue
  return $ mkUMathCode (toEnum mathclass) (fromIntegral fam) slot

-- \Umathcharnumdef, \Umathcodenum, \Umathcharnum's math code: <32-bit number>
readUMathCode32 :: (MonadTeXState s m, MonadError String m) => m MathCode
readUMathCode32 = do
  value <- int32ToWord32 <$> readInt32
  let slot = 0x1FFFFF .&. value
  if isUnicodeScalarValue slot
    then return $ UMathCode (word32ToInt32 value)
    else throwError "Invalid math code"

-- \Udelcode's delimiter code: <0-255><21-bit number>
readUDelimiterCodePair :: (MonadTeXState s m, MonadError String m) => m DelimiterCode
readUDelimiterCodePair = do
  fam <- readIntBetween 0 0xFF -- "Invalid delimiter code."
  slot <- readUnicodeScalarValue
  return $ mkUDelCode (fromIntegral fam) slot

-- \Udelcodenum's delimiter code (8-bit + 21-bit in 32 bit)
readUDelimiterCode32 :: (MonadTeXState s m, MonadError String m) => m DelimiterCode
readUDelimiterCode32 = do
  value <- readInt32
  let slot = 0x1FFFFF .&. int32ToWord32 value
  if 0 <= value && value < 2^(29::Int) && isUnicodeScalarValue slot
    then return $ UDelimiterCode value
    else throwError "Invalid delimiter code"
