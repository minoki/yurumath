{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module Text.YuruMath.TeX.Expansion where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Tokenizer
import Text.YuruMath.TeX.State
import Data.Int
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Control.Lens.Getter (view,use,uses)
import Control.Lens.Setter (assign,modifying)
import Control.Applicative
import Data.OpenUnion
import TypeFun.Data.List (SubList)

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
    [] -> do t <- nextToken
             return (toEToken <$> t)
    (_,t):ts -> do
      assign esPendingTokenList ts
      return (Just t)

nextETokenWithDepth :: (MonadTeXState s m, MonadError String m) => m (Maybe (Int,ExpansionToken))
nextETokenWithDepth = do
  pending <- use esPendingTokenList
  case pending of
    [] -> do t <- nextToken
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

readOptionalSpaces :: (MonadTeXState s m, MonadError String m) => m ()
readOptionalSpaces = do
  t <- nextETokenWithDepth
  case t of
    Just (_,ETCharacter { etCatCode = CCSpace }) -> readOptionalSpaces -- consumed
    Just (d,t) -> unreadETokens d [t] -- not consumed
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

expandafterCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
expandafterCommand = do
  t1 <- required nextEToken
  t2 <- required nextEToken
  (t1:) <$> expandOnce t2

noexpandCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
noexpandCommand = do
  t <- required nextEToken
  case t of
    ETCommandName { etNoexpand = False, etName = name } -> do
      m <- use (localState . definitionAt name)
      return $ case m of
        Left _ -> [ETCommandName { etNoexpand = True, etName = name }] -- expandable
        Right c | Just (Undefined _) <- toCommonValue c -> [ETCommandName { etNoexpand = True, etName = name }] -- undefined
        Right _ -> [t] -- not expandable
    _ -> return [t]

-- used by \csname and \ifcsname
readUntilEndcsname :: (MonadTeXState s m, MonadError String m) => [Char] -> m [Char]
readUntilEndcsname revName = do
  (t,v) <- evalToken
  case toCommonValue v of
    Just Endcsname -> return (reverse revName)
    _ -> case t of
      ETCommandName { etName = name } -> throwError $ "unexpected " ++ show name ++ " while looking for \\endcsname" -- not expandable, or \noexpand-ed
      ETCharacter { etChar = c } -> readUntilEndcsname (c:revName) -- non-active character

csnameCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
csnameCommand = do
  name <- readUntilEndcsname []
  let tname = T.pack name

  -- THE DREADED SIDE EFFECT OF \csname
  d <- use (localState . tsDefinitions)
  when (Map.notMember tname d)
    $ modifying (localState . tsDefinitions) (Map.insert tname (Right (injectCommonValue Relax)))

  return [ETCommandName { etNoexpand = False, etName = NControlSeq tname }]

-- LuaTeX extension: \begincsname
begincsnameCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
begincsnameCommand = do
  name <- readUntilEndcsname []
  let tname = T.pack name
  return [ETCommandName { etNoexpand = False, etName = NControlSeq tname }]

stringToEToken :: (MonadTeXState s m, MonadError String m) => String -> m [ExpansionToken]
stringToEToken [] = return []
stringToEToken (' ':xs) = (ETCharacter { etChar = ' ', etCatCode = CCSpace } :) <$> stringToEToken xs
stringToEToken (x:xs) = (ETCharacter { etChar = x, etCatCode = CCOther } :) <$> stringToEToken xs

stringCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
stringCommand = do
  t <- required nextEToken
  case t of
    ETCommandName { etName = NControlSeq name } -> do
      ech <- use (localState . escapechar)
      if isUnicodeScalarValue ech
      then stringToEToken (chr ech : T.unpack name)
      else stringToEToken (T.unpack name)
    ETCommandName { etName = NActiveChar c } ->
      stringToEToken [c]
    ETCharacter { etChar = c } ->
      stringToEToken [c]

-- LuaTeX extension: \csstring
csstringCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
csstringCommand = do
  t <- required nextEToken
  case t of
    ETCommandName { etName = NControlSeq name } ->
      stringToEToken (T.unpack name)
    ETCommandName { etName = NActiveChar c } ->
      stringToEToken [c]
    ETCharacter { etChar = c } ->
      stringToEToken [c]

readOptionalSigns :: (MonadTeXState s m, MonadError String m) => Int -> m Int
readOptionalSigns !s = do
  (t,v) <- evalToken
  case toCommonValue v of
    Just (Character _ CCSpace) -> readOptionalSigns s -- space: ignored
    Just (Character '+' CCOther) -> readOptionalSigns s
    Just (Character '-' CCOther) -> readOptionalSigns (-s)
    _ -> unreadETokens 0 [t] >> return s

readUnsignedDecimal :: (MonadTeXState s m, MonadError String m) => Char -> m Integer
readUnsignedDecimal c = readRest (fromIntegral (digitToInt c))
  where readRest !x = do
          (t,v) <- evalToken
          case toCommonValue v of
            Just (Character c CCOther) | isDigit c -> do
                                           readRest (10 * x + fromIntegral (digitToInt c))
            Just (Character _ CCSpace) -> return x -- consumed
            _ -> unreadETokens 0 [t] >> return x

readUnsignedOctal :: (MonadTeXState s m, MonadError String m) => m Integer
readUnsignedOctal = do
  (t,v) <- evalToken
  case toCommonValue v of
    Just (Character c CCOther) | isOctDigit c -> do
                                   readRest (fromIntegral (digitToInt c))
    _ -> throwError $ "unexpected token while reading octal: " ++ show t
  where readRest !x = do
          (t,v) <- evalToken
          case toCommonValue v of
            Just (Character c CCOther) | isOctDigit c -> do
                                           readRest (8 * x + fromIntegral (digitToInt c))
            Just (Character _ CCSpace) -> return x -- consumed
            _ -> unreadETokens 0 [t] >> return x

readUnsignedHex :: (MonadTeXState s m, MonadError String m) => m Integer
readUnsignedHex = do
  (t,v) <- evalToken
  case toCommonValue v of
    Just (Character c CCOther) | isUpperHexDigit c -> do
                                   readRest (fromIntegral (digitToInt c))
    Just (Character c CCLetter) | isHexDigit c && isAsciiUpper c -> do
                             readRest (fromIntegral (digitToInt c))
    _ -> throwError $ "unexpected token while reading hexadecimal: " ++ show t
  where readRest !x = do
          (t,v) <- evalToken
          case toCommonValue v of
            Just (Character c CCOther) | isUpperHexDigit c -> do
                                           readRest (16 * x + fromIntegral (digitToInt c))
            Just (Character c CCLetter) | isHexDigit c && isAsciiUpper c -> do
                                            readRest (16 * x + fromIntegral (digitToInt c))
            Just (Character _ CCSpace) -> return x -- consumed
            _ -> unreadETokens 0 [t] >> return x
        isUpperHexDigit c = isHexDigit c && (isDigit c || isAsciiUpper c)

readCharacterCode :: (MonadTeXState s m, MonadError String m) => m Integer
readCharacterCode = do
  t <- required nextEToken
  (u,v) <- evalToken -- one optional space
  case toCommonValue v of
    Just (Character _ CCSpace) -> return () -- consumed
    _ -> unreadETokens 0 [u]
  case t of
    ETCommandName { etName = NControlSeq name } -> case T.unpack name of
      [c] -> return (fromIntegral $ ord c)
      _ -> throwError "Improper alphabetic constant."
    ETCommandName { etName = NActiveChar c } -> return (fromIntegral $ ord c)
    ETCharacter { etChar = c } -> return (fromIntegral $ ord c)

readNumber :: (MonadTeXState s m, MonadError String m) => m Integer
readNumber = do
  sign <- readOptionalSigns 1
  (t,v) <- evalToken
  (fromIntegral sign *) <$> case toCommonValue v of
    Just (Character '\'' CCOther) -> readUnsignedOctal
    Just (Character '"' CCOther) -> readUnsignedHex
    Just (Character '`' CCOther) -> readCharacterCode
    Just (Character c CCOther) | isDigit c -> readUnsignedDecimal c
    Just (DefinedCharacter c) -> return (fromIntegral $ ord c)
    Just (IntegerConstant x) -> return (fromIntegral x)
    Just (DefinedMathCharacter m) -> return $ case m of
      MathCode x -> fromIntegral x
      UMathCode x -> fromIntegral x
    _ | Just i <- getIntegerValue v -> i
      | otherwise -> throwError $ "unexpected token while reading number: " ++ show t -- Missing number, treated as zero.

readInt32 :: (MonadTeXState s m, MonadError String m) => m Int32
readInt32 = do
  x <- readNumber
  if x < -2^(31::Int) || 2^(31::Int) <= x
    then throwError "Number too big"
    else return (fromInteger x)

readIntBetween :: (MonadTeXState s m, MonadError String m) => Int -> Int -> m Int
readIntBetween lo hi = do
  x <- readNumber
  if fromIntegral lo <= x && x <= fromIntegral hi
    then return (fromIntegral x)
    else throwError "Out of range"

numberCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
numberCommand = do
  x <- readNumber
  stringToEToken (show x)

theCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
theCommand = do
  x <- readNumber
  -- TODO: support other quantities
  stringToEToken (show x)

meaningCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
meaningCommand = do
  throwError "\\meaning: not implemented yet"

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

romannumeralCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
romannumeralCommand = do
  x <- readNumber
  if x < 0
    then return []
    else if x > fromIntegral (maxBound :: Int)
         then throwError "\\romannumeral: too large"
         else stringToEToken (showRomannumeral (fromIntegral x))

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
  deriving (Eq,Show)

instance IsExpandable ConditionalMarkerCommand where
  isConditional _     = False
  isIfCase _          = False
  isConditionalMarker (ConditionalMarkerCommand x) = Just x

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExpand ConditionalMarkerCommand m where
  doExpand (ConditionalMarkerCommand Eelse) = elseCommand
  doExpand (ConditionalMarkerCommand Efi)   = fiCommand
  doExpand (ConditionalMarkerCommand Eor)   = orCommand
  evalBooleanConditional _ = Nothing

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

expandBooleanConditional :: (MonadTeXState s m, MonadError String m) => m Bool -> m [ExpansionToken]
expandBooleanConditional c = do
    modifying conditionals (CondTest:)
    b <- c
    doBooleanConditional b
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

meaning :: (MonadTeXState s m, MonadError String m) => ExpansionToken -> m (Either (Expandable s) (Value s))
meaning t = do
  case t of
    ETCommandName { etNoexpand = True, etName = name } -> return (Right (injectCommonValue $ Unexpanded name))
    ETCommandName { etNoexpand = False, etName = name } -> use (localState . definitionAt name)
    ETCharacter { etChar = c, etCatCode = cc } -> return (Right (injectCommonValue $ Character c cc))

ifxCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifxCommand = do
  t1 <- required nextEToken >>= meaning
  t2 <- required nextEToken >>= meaning
  return $ t1 == t2

ifnumCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifnumCommand = do
  x <- readNumber
  -- TODO: skip spaces
  rel <- required nextEToken >>= meaning
  y <- readNumber
  case toCommonValue <$> rel of
    Right (Just (Character '<' CCOther)) -> return $ x < y
    Right (Just (Character '=' CCOther)) -> return $ x == y
    Right (Just (Character '>' CCOther)) -> return $ x > y
    _ -> throwError "unrecognized relation for \\ifnum"

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
  t <- required nextEToken >>= meaning
  case toCommonValue <$> t of
    Right (Just (Undefined _)) -> return False
    _ -> return True

-- e-TeX extension: \ifcsname
ifcsnameCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifcsnameCommand = do
  name <- readUntilEndcsname []
  let tname = T.pack name
  d <- use (localState . tsDefinitions)
  return (Map.member tname d)

-- e-TeX extension: \unless
unlessCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
unlessCommand = do
  test <- required nextEToken >>= meaning
  case test of
    Left c | Just c <- evalBooleanConditional c -> expandBooleanConditional (not <$> c)
    _ -> throwError "\\unless must be followed by a boolean conditional command"
    -- You can't use `\\unless' before `XXX'.

readGeneralText :: (MonadTeXState s m, MonadError String m) => m [TeXToken]
readGeneralText = do
  (t,v) <- evalToken
  case toCommonValue v of
    Just (Character _ CCSpace) -> readGeneralText -- optional spaces: ignored
    Just (Character _ CCBeginGroup) -> readUntilEndGroup LongParam
    Just Relax -> readGeneralText -- relax: ignored
    _ -> throwError $ "unexpected token " ++ show t -- Missing { inserted

-- e-TeX extension: \unexpanded
unexpandedCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
unexpandedCommand = do
  throwError "\\unexpanded: not implemented yet"

-- LuaTeX extension: \Uchar
ucharCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
ucharCommand = do
  x <- readUnicodeScalarValue
  return [ETCharacter { etChar = x, etCatCode = CCOther }] -- TODO: category code?

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

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExpand CommonExpandable m where
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
  evalBooleanConditional _ = Nothing

data CommonBoolean = Eiftrue
                   | Eiffalse
                   | Eif
                   | Eifcat
                   | Eifx
                   | Eifnum
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

evalCommonBoolean :: (MonadTeXState s m, MonadError String m) => CommonBoolean -> m Bool
evalCommonBoolean Eiftrue = iftrueCommand
evalCommonBoolean Eiffalse = iffalseCommand
evalCommonBoolean Eif = ifCommand
evalCommonBoolean Eifcat = ifcatCommand
evalCommonBoolean Eifx = ifxCommand
evalCommonBoolean Eifnum = ifnumCommand
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
