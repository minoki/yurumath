{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.YuruMath.TeX.Expansion where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Quantity
import qualified Text.YuruMath.TeX.Tokenizer as Tok (nextToken)
import Text.YuruMath.TeX.State
import Data.Int
import Data.Char
import Data.Ratio
import qualified Data.Text as T
import Data.Monoid ((<>))
import Control.Monad
import Control.Monad.Error.Class
import Control.Lens.Getter (use)
import Control.Lens.Setter (assign,modifying)

toEToken :: TeXToken -> ExpansionToken
toEToken (TTCommandName name) = ETCommandName { etNoexpand = False, etName = name }
toEToken (TTCharacter c cc) = ETCharacter { etChar = c, etCatCode = cc }

fromEToken :: ExpansionToken -> TeXToken
fromEToken (ETCommandName { etName = name }) = TTCommandName name -- etNoexpand is ignored
fromEToken (ETCharacter { etChar = c, etCatCode = cc }) = TTCharacter c cc

nextEToken :: (MonadTeXState s m, MonadError String m) => m (Maybe ExpansionToken)
nextEToken = do
  pending <- use esPendingTokenList
  case pending of
    [] -> do t <- Tok.nextToken
             return (toEToken <$> t)
    (_,t):ts -> do
      assign esPendingTokenList ts
      return (Just t)

nextETokenWithDepth :: (MonadTeXState s m, MonadError String m) => m (Maybe (Int,ExpansionToken))
nextETokenWithDepth = do
  pending <- use esPendingTokenList
  case pending of
    [] -> do t <- Tok.nextToken
             return (((,) 0 . toEToken) <$> t)
    t:ts -> do
      assign esPendingTokenList ts
      return (Just t)

unreadETokens :: (MonadTeXState s m, MonadError String m) => Int -> [ExpansionToken] -> m ()
unreadETokens !depth ts = do
  limit <- use esMaxPendingToken
  ts' <- use esPendingTokenList
  maxDepth <- use esMaxDepth
  when (depth >= maxDepth) $ throwError "recursion too deep"
  when (length ts' + length ts > limit) $ throwError "token list too long"
  assign esPendingTokenList (map ((,) depth) ts ++ ts')

readUntilEndGroup :: (MonadTeXState s m, MonadError String m) => ParamLong -> m [TeXToken]
readUntilEndGroup !long = loop (0 :: Int) []
  where
    loop !depth revTokens = do
      t <- nextEToken
      case t of
        Nothing -> throwError "unexpected end of input when reading an argument"
        Just t@(ETCharacter { etCatCode = CCEndGroup })
          | depth == 0 -> return (reverse revTokens)
          | otherwise -> loop (depth - 1) (fromEToken t : revTokens)
        Just t@(ETCharacter { etCatCode = CCBeginGroup })
          -> loop (depth + 1) (fromEToken t : revTokens)
        Just (ETCommandName { etName = NControlSeq "par" })
          | long == ShortParam -> throwError "Paragraph ended before argument was compelete"
        Just t -> loop depth (fromEToken t : revTokens)

-- reads undelimited macro argument
readArgument :: (MonadTeXState s m, MonadError String m) => ParamLong -> m [TeXToken]
readArgument !long = do
  t <- nextEToken
  case t of
    Nothing -> throwError "unexpected end of input when expecting an argument"
    Just (ETCharacter { etCatCode = CCSpace }) -> readArgument long
    Just (ETCharacter { etCatCode = CCEndGroup }) -> throwError "unexpected end of group"
    Just (ETCharacter { etCatCode = CCBeginGroup }) -> readUntilEndGroup long
    Just (ETCommandName { etName = NControlSeq "par" })
      | long == ShortParam -> throwError "Paragraph ended before argument was compelete"
    Just t -> return [fromEToken t]

-- reads a control sequence or an active character
readCommandName :: (MonadTeXState s m, MonadError String m) => m CommandName
readCommandName = do
  t <- required nextEToken
  case t of
    ETCommandName { etName = name } -> return name
    _ -> throwError $ "unexpected character token: " ++ show t
         -- or, "Missing control sequence inserted"

-- explicit space or implicit space
isImplicitSpace :: IsValue v => v -> Bool
isImplicitSpace v = case toCommonValue v of
  Just (Character _ CCSpace) -> True
  _ -> False

readOneOptionalSpace :: (MonadTeXState s m, MonadError String m) => m ()
readOneOptionalSpace = do
  et <- maybeEvalToken
  case et of
    Just (t,v) | isImplicitSpace v -> return () -- consumed
               | otherwise -> unreadETokens 0 [t]
    Nothing -> return ()

readOptionalSpaces :: (MonadTeXState s m, MonadError String m) => m ()
readOptionalSpaces = do
  et <- maybeEvalToken
  case et of
    Just (t,v) | isImplicitSpace v -> readOptionalSpaces -- consumed
               | otherwise -> unreadETokens 0 [t]
    Nothing -> return ()

readEquals :: (MonadTeXState s m, MonadError String m) => m ()
readEquals = do
  t <- nextETokenWithDepth
  case t of
    Just (_,ETCharacter { etCatCode = CCSpace }) -> readOptionalSpaces -- consumed
    Just (_,ETCharacter { etChar = '=', etCatCode = CCOther }) -> return () -- consumed
    Just (d,t) -> unreadETokens d [t] -- not consumed
    Nothing -> return ()

-- read a number between 0.."10FFFF excluding "D800.."DFFF, and convert it to a Char
-- Note: Although neither LuaTeX nor XeTeX seems to forbid surrogate codes ("D800-"DFFF), we do.
readUnicodeScalarValue :: (MonadTeXState s m, MonadError String m) => m Char
readUnicodeScalarValue = do
  x <- readNumber
  if isUnicodeScalarValue x
    then return $ chr $ fromIntegral x
    else throwError $ "Bad character code (" ++ show x ++ ")"

readKeyword :: (MonadTeXState s m, MonadError String m) => String -> m Bool
readKeyword xs = do
  readOptionalSpaces
  loop xs
  where
    loop [] = return True
    loop (x:xs) = do
      t <- nextETokenWithDepth
      case t of
        Just (d,t@(ETCharacter { etChar = c }))
          | x == c || x == toLower c -> do
              r <- loop xs
              if r
                then return True
                else do unreadETokens d [t]
                        return False
        Just (d,t) -> do unreadETokens d [t]
                         return False
        Nothing -> return False

readOneOfKeywords :: (MonadTeXState s m, MonadError String m) => [String] -> m (Maybe String)
readOneOfKeywords keywords = readOneOfKeywordsV (map (\k -> (k,k)) keywords)

-- read an optional keyword and return the associated value with it
readOneOfKeywordsV :: (MonadTeXState s m, MonadError String m) => [(String,v)] -> m (Maybe v)
readOneOfKeywordsV keywords = do
  readOptionalSpaces
  loop keywords
  where
    loop [] = return Nothing
    loop keywords | Just v <- lookup "" keywords = return (Just v)
    loop keywords = do
      t <- nextETokenWithDepth
      case t of
        Just (d,t@(ETCharacter { etChar = c })) -> do
          k <- loop [(xs,v) | (x:xs,v) <- keywords, x == c || x == toLower c]
          case k of
            Just _ -> return k
            Nothing -> do unreadETokens d [t]
                          return Nothing
        Just (d,t) -> do unreadETokens d [t]
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

readOptionalKeyword :: (MonadTeXState s m, MonadError String m) => String -> m ()
readOptionalKeyword name = do _ <- readKeyword name
                              return ()

-- used by \expandafter
expandOnce :: (MonadTeXState s m, MonadError String m, DoExpand (Expandable s) m) => ExpansionToken -> m [ExpansionToken]
expandOnce et@(ETCommandName { etNoexpand = False, etName = name }) = do
  m <- use (localState . definitionAt name)
  case m of
    Left e -> doExpand e
    _ -> return [et]
expandOnce et = return [et]

-- used by number reading, \if and \ifcat argument, general text
evalToken :: (MonadTeXState s m, MonadError String m) => m (ExpansionToken,Value s)
evalToken = do
  (d,t) <- required nextETokenWithDepth
  case t of
    ETCommandName { etNoexpand = False, etName = name } -> do
      m <- use (localState . definitionAt name)
      case m of
        Left e | Just v <- isConditionalMarker e -> do
          cs <- use conditionals
          case cs of
            CondTest:_ -> do
              unreadETokens d [t]
              return (ETCommandName { etNoexpand = False, etName = NControlSeq "relax" }, injectCommonValue $ Relax)
            _ -> do
              r <- doExpand e
              unreadETokens (d+1) r
              evalToken
        Left e -> do
          r <- doExpand e
          unreadETokens (d+1) r
          evalToken
        Right v -> return (t,v) -- non-expandable commands are not executed
    ETCommandName { etNoexpand = True, etName = name } -> do
      return (t,injectCommonValue $ Unexpanded name)
    ETCharacter { etChar = c, etCatCode = cc } ->
      return (t,injectCommonValue $ Character c cc)

maybeEvalToken :: (MonadTeXState s m, MonadError String m) => m (Maybe (ExpansionToken,Value s))
maybeEvalToken = do
  et <- nextETokenWithDepth
  case et of
    Just (d,t) ->
      case t of
        ETCommandName { etNoexpand = False, etName = name } -> do
          m <- use (localState . definitionAt name)
          case m of
            Left e | Just v <- isConditionalMarker e -> do
                       cs <- use conditionals
                       case cs of
                         CondTest:_ -> do
                           unreadETokens d [t]
                           return $ Just (ETCommandName { etNoexpand = False, etName = NControlSeq "relax" }, injectCommonValue $ Relax)
                         _ -> do
                           r <- doExpand e
                           unreadETokens (d+1) r
                           maybeEvalToken
            Left e -> do
              r <- doExpand e
              unreadETokens (d+1) r
              maybeEvalToken
            Right v -> return $ Just (t,v) -- non-expandable commands are not executed
        ETCommandName { etNoexpand = True, etName = name } -> do
          return $ Just (t,injectCommonValue $ Unexpanded name)
        ETCharacter { etChar = c, etCatCode = cc } ->
          return $ Just (t,injectCommonValue $ Character c cc)
    Nothing -> return Nothing

evalToValue :: (MonadTeXState s m, MonadError String m) => m (Maybe (Value s))
evalToValue = do
  et <- nextETokenWithDepth
  case et of
    Just (d,t) -> do
      case t of
        ETCommandName { etNoexpand = False, etName = name } -> do
          m <- use (localState . definitionAt name)
          case m of
            Left e -> do
              r <- doExpand e
              unreadETokens (d+1) r
              evalToValue
            Right v -> return (Just v)
        ETCommandName { etNoexpand = True, etName = name } -> do
          return (Just (injectCommonValue $ Unexpanded name))
        ETCharacter { etChar = c, etCatCode = cc } ->
          return (Just (injectCommonValue $ Character c cc))
    Nothing -> return Nothing

required :: (MonadError String m) => m (Maybe a) -> m a
required m = do a <- m
                case a of
                  Nothing -> throwError "unexpected end of input"
                  Just a -> return a

-- used by \csname and \ifcsname
readUntilEndcsname :: (MonadTeXState s m, MonadError String m) => [Char] -> m [Char]
readUntilEndcsname revName = do
  (t,v) <- evalToken
  case toCommonValue v of
    Just Endcsname -> return (reverse revName)
    _ -> case t of
      ETCommandName { etName = name } -> throwError $ "unexpected " ++ show name ++ " while looking for \\endcsname" -- not expandable, or \noexpand-ed
      ETCharacter { etChar = c } -> readUntilEndcsname (c:revName) -- non-active character

stringToEToken :: (MonadTeXState s m, MonadError String m) => String -> m [ExpansionToken]
stringToEToken [] = return []
stringToEToken (' ':xs) = (ETCharacter { etChar = ' ', etCatCode = CCSpace } :) <$> stringToEToken xs
stringToEToken (x:xs) = (ETCharacter { etChar = x, etCatCode = CCOther } :) <$> stringToEToken xs

-- <optional signs> ::= <optional spaces> | <optional signs><plus or minus><optional spaces>
readOptionalSigns :: (MonadTeXState s m, MonadError String m) => Int -> m Int
readOptionalSigns !s = do
  (t,v) <- evalToken
  case t of
    ETCharacter { etChar = '+', etCatCode = CCOther } -> readOptionalSigns s
    ETCharacter { etChar = '-', etCatCode = CCOther } -> readOptionalSigns (-s)
    _ | isImplicitSpace v -> readOptionalSigns s -- space: ignored
      | otherwise -> unreadETokens 0 [t] >> return s

readUnsignedDecimalInteger :: forall s m. (MonadTeXState s m, MonadError String m) => Char -> m Integer
readUnsignedDecimalInteger !c = readRest (fromIntegral (digitToInt c))
  where
    readRest :: Integer -> m Integer
    readRest !x = do
      (t,v) <- evalToken
      case t of
        ETCharacter { etChar = c, etCatCode = CCOther }
          | isDigit c -> readRest (10 * x + fromIntegral (digitToInt c))
        _ | isImplicitSpace v -> return x -- space: consumed
          | otherwise -> unreadETokens 0 [t] >> return x

readUnsignedOctal :: forall s m. (MonadTeXState s m, MonadError String m) => m Integer
readUnsignedOctal = do
  (t,v) <- evalToken
  case t of
    ETCharacter { etChar = c, etCatCode = CCOther }
      | isOctDigit c -> readRest (fromIntegral (digitToInt c))
    _ -> throwError $ "unexpected token while reading octal: " ++ show t
  where
    readRest :: Integer -> m Integer
    readRest !x = do
      (t,v) <- evalToken
      case t of
        ETCharacter { etChar = c, etCatCode = CCOther }
          | isOctDigit c -> readRest (8 * x + fromIntegral (digitToInt c))
        _ | isImplicitSpace v -> return x -- consumed
          | otherwise -> unreadETokens 0 [t] >> return x

readUnsignedHex :: forall s m. (MonadTeXState s m, MonadError String m) => m Integer
readUnsignedHex = do
  (t,v) <- evalToken
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
      (t,v) <- evalToken
      case t of
        ETCharacter { etChar = c, etCatCode = CCOther }
          | isUpperHexDigit c -> readRest (16 * x + fromIntegral (digitToInt c))
        ETCharacter { etChar = c, etCatCode = CCLetter }
          | isHexDigit c && isAsciiUpper c ->
              readRest (16 * x + fromIntegral (digitToInt c))
        _ | isImplicitSpace v -> return x -- consumed
          | otherwise -> unreadETokens 0 [t] >> return x
    isUpperHexDigit c = isHexDigit c && (isDigit c || isAsciiUpper c)

readCharacterCode :: (MonadTeXState s m, MonadError String m) => m Integer
readCharacterCode = do
  t <- required nextEToken
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
  (t,v) <- evalToken
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
      (t,v) <- evalToken
      case t of
        ETCharacter { etChar = '.', etCatCode = CCOther } -> readFractionPart x 0
        ETCharacter { etChar = ',', etCatCode = CCOther } -> readFractionPart x 0
        ETCharacter { etChar = c, etCatCode = CCOther }
          | isDigit c -> readIntegerPart (10 * x + fromIntegral (digitToInt c))
        _ -> unreadETokens 0 [t] >> return (fromInteger x)
    readFractionPart :: Integer -> Int -> m Rational
    readFractionPart !intPart !expPart = do
      (t,v) <- evalToken
      case t of
        ETCharacter { etChar = c, etCatCode = CCOther }
          | isDigit c -> readFractionPart (10 * intPart + fromIntegral (digitToInt c)) (expPart + 1)
        _ -> unreadETokens 0 [t] >> return (intPart % 10^expPart)

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
  (t,v) <- evalToken
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
      (t,v) <- evalToken
      case getQuantity v of
        QInteger getInteger -> do v <- getInteger
                                  return (sp (factor * fromInteger v))
        QDimension getDimen -> scaleByRational factor <$> getDimen
        QGlue getGlue -> (scaleByRational factor . glueSpace) <$> getGlue
        _ -> do unreadETokens 0 [t]
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
  (t,v) <- evalToken
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
        else do (t,v) <- evalToken
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
  (t,v) <- evalToken
  applySign <$> case getQuantity v of
    QGlue getGlue -> getGlue
    _ -> do unreadETokens 0 [t]
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
  (t,v) <- evalToken
  applySign <$> case getQuantity v of
    QMuGlue getMuGlue -> getMuGlue
    _ -> do unreadETokens 0 [t]
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
theString :: (MonadTeXState s m, MonadError String m) => String -> m String
theString name = do
  (t,v) <- evalToken
  case getQuantity v of
    QInteger getInteger ->
      do show <$> getInteger
    QDimension getDimension ->
      do showDimension <$> getDimension
    QGlue getGlue ->
      do Glue { glueSpace = x, glueStretch = stretch, glueShrink = shrink } <- getGlue
         return $ showDimension x ++ showSS " plus " stretch ++ showSS " minus " shrink
    QMuGlue getMuGlue ->
      do Glue { glueSpace = x, glueStretch = stretch, glueShrink = shrink } <- getMuGlue
         return $ showMuDimension x ++ showMuSS " plus " stretch ++ showMuSS " minus " shrink
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

-- to be used by conditionals, \or, \else
-- does not actually expand the token.
shallowEval :: (MonadTeXState s m, MonadError String m) => m (Maybe (Expandable s))
shallowEval = do
  t <- required nextEToken
  case t of
    ETCommandName { etNoexpand = False, etName = name } -> do
      m <- use (localState . definitionAt name)
      case m of
        Left e -> return (Just e) -- expandable
        Right _ -> return Nothing -- non-expandable
    _ -> return Nothing -- non-expandable

-- True if encountered \else, False if encountered \fi
skipUntilElse :: (MonadTeXState s m, MonadError String m) => Int -> m Bool
skipUntilElse !level = do
  x <- shallowEval
  case x of
    Just c | Just m <- isConditionalMarker c -> case m of
               Eor | level == 0 -> throwError "Extra \\or"
               Eelse | level == 0 -> return True
               Efi | level == 0 -> return False
                   | otherwise -> skipUntilElse (level - 1)
               _ -> skipUntilElse level -- Inner \else, \or
           | isConditional c -> skipUntilElse (level + 1)
    _ -> skipUntilElse level

skipUntilFi :: (MonadTeXState s m, MonadError String m) => Int -> m ()
skipUntilFi !level = do
  x <- shallowEval
  case x of
    Just c | isConditionalMarker c == Just Efi
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
  x <- shallowEval
  case x of
    Just c | Just m <- isConditionalMarker c -> case m of
               Eor | level == 0 -> return FoundOr
               Eelse | level == 0 -> return FoundElse
               Efi | level == 0 -> return FoundFi
                   | otherwise -> skipUntilOr (level - 1)
               _ -> skipUntilOr level -- Inner \else, \or
    Just c | isConditional c -> skipUntilOr (level + 1)
    _ -> skipUntilOr level

doBooleanConditional :: (MonadTeXState s m, MonadError String m) => Bool -> m ()
doBooleanConditional True = do
  modifying conditionals (\(CondTest:xs) -> CondTruthy:xs)
doBooleanConditional False = do
  e <- skipUntilElse 0
  if e
    then modifying conditionals (\(CondTest:xs) -> CondFalsy:xs)
    else modifying conditionals tail

expandBooleanConditional :: (MonadTeXState s m, MonadError String m) => m Bool -> m [ExpansionToken]
expandBooleanConditional c = do
    modifying conditionals (CondTest:)
    b <- c
    doBooleanConditional b
    return []

meaningWithoutExpansion :: (MonadTeXState s m, MonadError String m) => ExpansionToken -> m (Either (Expandable s) (Value s))
meaningWithoutExpansion t = do
  case t of
    ETCommandName { etNoexpand = True, etName = name } -> return (Right (injectCommonValue $ Unexpanded name))
    ETCommandName { etNoexpand = False, etName = name } -> use (localState . definitionAt name)
    ETCharacter { etChar = c, etCatCode = cc } -> return (Right (injectCommonValue $ Character c cc))

readGeneralText :: (MonadTeXState s m, MonadError String m) => m [TeXToken]
readGeneralText = do
  (t,v) <- evalToken
  case toCommonValue v of
    Just (Character _ CCSpace) -> readGeneralText -- optional spaces: ignored
    Just (Character _ CCBeginGroup) -> readUntilEndGroup LongParam
    Just Relax -> readGeneralText -- relax: ignored
    _ -> throwError $ "unexpected token " ++ show t -- Missing { inserted

readLBrace :: (MonadTeXState s m, MonadError String m) => m ()
readLBrace = do
  (t,v) <- evalToken
  case toCommonValue v of
    Just (Character _ CCSpace) -> readLBrace
    Just (Character _ CCBeginGroup) -> do
      enterGroup ScopeByBrace
    _ -> throwError ("Expected `{', but got " ++ show t)
