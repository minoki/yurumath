{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Text.YuruMath.TeX.Expansion where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Tokenizer
import Text.YuruMath.TeX.State
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
toEToken (TTControlSeq name) = ETCommandName False (NControlSeq name)
toEToken (TTCharacter c CCActive) = ETCommandName False (NActiveChar c)
toEToken (TTCharacter c cc) = ETCharacter c cc

fromEToken :: ExpansionToken -> TeXToken
fromEToken (ETCommandName _ (NControlSeq name)) = TTControlSeq name
fromEToken (ETCommandName _ (NActiveChar c)) = TTCharacter c CCActive
fromEToken (ETCharacter c cc) = TTCharacter c cc

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

readUntilEndGroup :: (MonadTeXState s m, MonadError String m) => Bool -> Int -> [TeXToken] -> m [TeXToken]
readUntilEndGroup !isLong !depth revTokens = do
  t <- nextEToken
  case t of
    Nothing -> throwError "unexpected end of input when reading an argument"
    Just t@(ETCharacter _ CCEndGroup)
      | depth == 0 -> return (reverse revTokens)
      | otherwise -> readUntilEndGroup isLong (depth - 1) (fromEToken t : revTokens)
    Just t@(ETCharacter _ CCBeginGroup)
      -> readUntilEndGroup isLong (depth + 1) (fromEToken t : revTokens)
    Just (ETCommandName _ (NControlSeq par))
      | not isLong, par == "par" -> throwError "Paragraph ended before argument was compelete"
    Just t
      -> readUntilEndGroup isLong depth (fromEToken t : revTokens)

-- reads undelimited macro argument
readArgument :: (MonadTeXState s m, MonadError String m) => Bool -> m [TeXToken]
readArgument !isLong = do
  t <- nextEToken
  case t of
    Nothing -> throwError "unexpected end of input when expecting an argument"
    Just (ETCharacter _ CCSpace) -> readArgument isLong
    Just (ETCharacter _ CCEndGroup) -> throwError "unexpected end of group"
    Just (ETCharacter _ CCBeginGroup) -> readUntilEndGroup isLong 0 []
    Just (ETCommandName _ (NControlSeq par)) | not isLong, par == "par" -> throwError "Paragraph ended before argument was compelete"
    Just t -> return [fromEToken t]

-- reads a control sequence or an active character
readCommandName :: (MonadTeXState s m, MonadError String m) => m CommandName
readCommandName = do
  t <- required nextEToken
  case t of
    ETCommandName _ name -> return name
    _ -> throwError $ "unexpected character token: " ++ show t
         -- or, "Missing control sequence inserted"

readOptionalSpaces :: (MonadTeXState s m, MonadError String m) => m ()
readOptionalSpaces = do
  t <- nextETokenWithDepth
  case t of
    Just (_,ETCharacter _ CCSpace) -> readOptionalSpaces -- consumed
    Just (d,t) -> unreadETokens d [t] -- not consumed
    Nothing -> return ()

readEquals :: (MonadTeXState s m, MonadError String m) => m ()
readEquals = do
  t <- nextETokenWithDepth
  case t of
    Just (_,ETCharacter _ CCSpace) -> readOptionalSpaces -- consumed
    Just (_,ETCharacter '=' CCOther) -> return () -- consumed
    Just (d,t) -> unreadETokens d [t] -- not consumed
    Nothing -> return ()

isUnicodeScalarValue :: (Integral a) => a -> Bool
isUnicodeScalarValue x = 0 <= x && x <= 0x10FFFF && not (0xD800 <= x && x <= 0xDFFF)

-- read a number between 0.."10FFFF excluding "D800.."DFFF, and convert it to a Char
-- Note: Although neither LuaTeX nor XeTeX seems to forbid surrogate codes ("D800-"DFFF), we do.
readUnicodeScalarValue :: (MonadTeXState s m, MonadError String m) => m Char
readUnicodeScalarValue = do
  x <- readNumber
  if isUnicodeScalarValue x
    then return $ chr $ fromIntegral x
    else throwError $ "Bad character code (" ++ show x ++ ")"

{-
testDelimiter :: (MonadTeXState s m, MonadError String m) => [TeXToken] -> m Bool
testDelimiter [] = return True
testDelimiter (d:ds) = do
  t <- nextEToken
  if etToken t == d
    then do u <- testDelimiter
            if u
              then return True
              else unreadETokens 0 [t] >> 
    

readDelimitedArgument :: (MonadTeXState s m, MonadError String m) => Bool -> [TeXToken] -> m [TeXToken]
readDelimitedArgument !isLong [] = error "readDelimitedArgument: delimiter must not be empty"
readDelimitedArgument !isLong delimiter@(d0:_) = do
  t <- nextEToken
  case t of
    ExpansionToken _ tk | tk == d0 -> return []
    ExpansionToken _ (TTCharacter _ CCBeginGroup) -> readUntilEndGroup
-}

-- used by \unexpanded
isExpandableNameF :: (MonadTeXState s m) => m (TeXToken -> Bool)
isExpandableNameF = do
  cd <- use (localState . tsDefinitions)
  ad <- use (localState . tsActiveDefinitions)
  return $ \t -> case t of
    TTControlSeq name -> case Map.lookup name cd of
                           Just (Left _) -> True
                           _ -> False
    TTCharacter c CCActive -> case Map.lookup c ad of
                                Just (Left _) -> True
                                _ -> False
    _ -> False

-- used by \expandafter
expandOnce :: (MonadTeXState s m, MonadError String m, DoExpand (Expandable s) m) => ExpansionToken -> m [ExpansionToken]
expandOnce et@(ETCommandName False name) = do
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
    ETCommandName False name -> do
      m <- use (localState . definitionAt name)
      case m of
        Left e | Just v <- isConditionalMarker e -> do
          cs <- use conditionals
          case cs of
            CondTest:_ -> do
              unreadETokens d [t]
              return (ETCommandName False (NControlSeq "relax"), injectCommonValue $ Relax)
            _ -> do
              r <- doExpand e
              unreadETokens (d+1) r
              evalToken
        Left e -> do
          r <- doExpand e
          unreadETokens (d+1) r
          evalToken
        Right v -> return (t,v) -- non-expandable commands are not executed
    ETCommandName True name -> do
      return (t,injectCommonValue $ Unexpanded name)
    ETCharacter c cc ->
      return (t,injectCommonValue $ Character c cc)

evalToValue :: (MonadTeXState s m, MonadError String m) => m (Maybe (Value s))
evalToValue = do
  et <- nextETokenWithDepth
  case et of
    Just (d,t) -> do
      case t of
        ETCommandName False name -> do
          m <- use (localState . definitionAt name)
          case m of
            Left e -> do
              r <- doExpand e
              unreadETokens (d+1) r
              evalToValue
            Right v -> return (Just v)
        ETCommandName True name -> do
          return (Just (injectCommonValue $ Unexpanded name))
        ETCharacter c cc ->
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
    ETCommandName False name -> do
      m <- use (localState . definitionAt name)
      return $ case m of
        Left _ -> [ETCommandName True name] -- expandable
        Right c | Just (Undefined _) <- toCommonValue c -> [ETCommandName True name] -- undefined
        Right _ -> [t] -- not expandable
    _ -> return [t]

-- used by \csname and \ifcsname
readUntilEndcsname :: (MonadTeXState s m, MonadError String m) => [Char] -> m [Char]
readUntilEndcsname revName = do
  (t,v) <- evalToken
  case toCommonValue v of
    Just Endcsname -> return (reverse revName)
    _ -> case t of
      ETCommandName _ name -> throwError $ "unexpected " ++ show name ++ " while looking for \\endcsname" -- not expandable, or \noexpand-ed
      ETCharacter c _ -> readUntilEndcsname (c:revName) -- non-active character

csnameCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
csnameCommand = do
  name <- readUntilEndcsname []
  let tname = T.pack name

  -- THE DREADED SIDE EFFECT OF \csname
  d <- use (localState . tsDefinitions)
  when (Map.notMember tname d)
    $ modifying (localState . tsDefinitions) (Map.insert tname (Right (injectCommonValue Relax)))

  return [ETCommandName False (NControlSeq tname)]

-- LuaTeX extension: \begincsname
begincsnameCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
begincsnameCommand = do
  name <- readUntilEndcsname []
  let tname = T.pack name
  return [ETCommandName False (NControlSeq tname)]

stringToEToken :: (MonadTeXState s m, MonadError String m) => String -> m [ExpansionToken]
stringToEToken [] = return []
stringToEToken (' ':xs) = (ETCharacter ' ' CCSpace :) <$> stringToEToken xs
stringToEToken (x:xs) = (ETCharacter x CCOther :) <$> stringToEToken xs

stringCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
stringCommand = do
  t <- required nextEToken
  case t of
    ETCommandName _ (NControlSeq name) ->
      stringToEToken ('\\' : T.unpack name)
    ETCommandName _ (NActiveChar c) ->
      stringToEToken [c]
    ETCharacter c _ ->
      stringToEToken [c]

-- LuaTeX extension: \csstring
csstringCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
csstringCommand = do
  t <- required nextEToken
  case t of
    ETCommandName _ (NControlSeq name) ->
      stringToEToken (T.unpack name)
    ETCommandName _ (NActiveChar c) ->
      stringToEToken [c]
    ETCharacter c _ ->
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
    ETCommandName _ (NControlSeq name) -> case T.unpack name of
      [c] -> return (fromIntegral $ ord c)
      _ -> throwError "Improper alphabetic constant."
    ETCommandName _ (NActiveChar c) -> return (fromIntegral $ ord c)
    ETCharacter c _ -> return (fromIntegral $ ord c)

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
    -- DefinedMathCharacter
    -- make extensible?
    _ -> throwError $ "unexpected token while reading number: " ++ show t -- Missing number, treated as zero.

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
    ETCommandName False name -> do
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
               Eelse | level == 0 -> return True
               Efi | level == 0 -> return False
                   | otherwise -> skipUntilElse (level - 1)
               Eor | level == 0 -> throwError "Extra \\or"
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

-- orphaned instance...
instance (Monad m, MonadTeXState s m, MonadError String m) => DoExpand ConditionalMarker m where
  doExpand Eelse = elseCommand
  doExpand Efi = fiCommand
  doExpand Eor = orCommand
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
  t1 <- (fromEToken . fst) <$> evalToken
  t2 <- (fromEToken . fst) <$> evalToken
  case (t1, t2) of
    (TTCharacter c1 _, TTCharacter c2 _) -> return $ c1 == c2
    (TTControlSeq _, TTControlSeq _) -> return True
    (_, _) -> return False

-- \ifcat: test category codes
ifcatCommand :: (MonadTeXState a m, MonadError String m) => m Bool
ifcatCommand = do
  t1 <- (fromEToken . fst) <$> evalToken
  t2 <- (fromEToken . fst) <$> evalToken
  case (t1, t2) of
    (TTCharacter _ cc1, TTCharacter _ cc2) -> return $ cc1 == cc2
    (TTControlSeq _, TTControlSeq _) -> return True
    (_, _) -> return False

meaning :: (MonadTeXState s m, MonadError String m) => ExpansionToken -> m (Either (Expandable s) (Value s))
meaning t = do
  case t of
    ETCommandName True name -> return (Right (injectCommonValue $ Unexpanded name))
    ETCommandName False name -> use (localState . definitionAt name)
    ETCharacter c cc -> return (Right (injectCommonValue $ Character c cc))

-- \ifx: test category codes
ifxCommand :: (MonadTeXState s m, MonadError String m) => m Bool
ifxCommand = do
  t1 <- required nextEToken >>= meaning
  t2 <- required nextEToken >>= meaning
  return $ t1 == t2
  {-
  case (t1, t2) of
    (Left (ConditionalMarker v1), Left (ConditionalMarker v2)) -> return $ v1 == v2
    (Left IfCase, Left IfCase) -> return True
    -- TODO: other expandable commands
    (Right (Character c1 cc1), Right (Character c2 cc2)) -> return $ c1 == c2 && cc1 == cc2
    (Right (DefinedCharacter c1), Right (DefinedCharacter c2)) -> return $ c1 == c2
    (Right (DefinedMathCharacter c1), Right (DefinedMathCharacter c2)) -> return $ c1 == c2
    (Right (Unexpanded c1), Right (Unexpanded c2)) -> return True
    (Right (Undefined c1), Right (Undefined c2)) -> return True
    (Right Relax, Right Relax) -> return True
    -- Note: \noexpand-ed token and \relax are not equivalent in the sense of \ifx
    (_, _) -> return False
-}

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
    Just (Character _ CCBeginGroup) -> readUntilEndGroup True 0 []
    Just Relax -> readGeneralText -- relax: ignored
    _ -> throwError $ "unexpected token " ++ show t -- Missing { inserted

-- e-TeX extension: \unexpanded
unexpandedCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
unexpandedCommand = do
  ie <- isExpandableNameF
  -- map (\t -> ExpansionToken (ie t) t) <$> readGeneralText
  throwError "\\unexpanded: not implemented yet"

-- LuaTeX extension: \Uchar
ucharCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
ucharCommand = do
  x <- readUnicodeScalarValue
  return [ETCharacter x CCOther] -- TODO: category code?

-- LuaTeX extension: \mathstyle
mathstyleCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
mathstyleCommand = do
  ismm <- uses mode isMMode
  if ismm
    then do
    style <- use (localState . mathStyle)
    stringToEToken $ show $ fromEnum style
    else stringToEToken "-1"

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

expandableDefinitions :: SubList '[ConditionalMarker, CommonExpandable, CommonBoolean] set => Map.Map Text (Union set)
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
  ,("else",        liftUnion Eelse)
  ,("fi",          liftUnion Efi)
  ,("or",          liftUnion Eor)

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
