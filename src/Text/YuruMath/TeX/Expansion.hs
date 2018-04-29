{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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

toEToken :: TeXToken -> ExpansionToken
toEToken (TTControlSeq name) = ETCommandName False (NControlSeq name)
toEToken (TTCharacter c CCActive) = ETCommandName False (NActiveChar c)
toEToken (TTCharacter c cc) = ETCharacter c cc

fromEToken :: ExpansionToken -> TeXToken
fromEToken (ETCommandName _ (NControlSeq name)) = TTControlSeq name
fromEToken (ETCommandName _ (NActiveChar c)) = TTCharacter c CCActive
fromEToken (ETCharacter c cc) = TTCharacter c cc

nextEToken :: (MonadTeXState a m, MonadError String m) => m (Maybe ExpansionToken)
nextEToken = do
  pending <- use esPendingTokenList
  case pending of
    [] -> do t <- nextToken
             return (toEToken <$> t)
    (_,t):ts -> do
      assign esPendingTokenList ts
      return (Just t)

nextETokenWithDepth :: (MonadTeXState a m, MonadError String m) => m (Maybe (Int,ExpansionToken))
nextETokenWithDepth = do
  pending <- use esPendingTokenList
  case pending of
    [] -> do t <- nextToken
             return (((,) 0 . toEToken) <$> t)
    t:ts -> do
      assign esPendingTokenList ts
      return (Just t)

unreadETokens :: (MonadTeXState a m, MonadError String m) => Int -> [ExpansionToken] -> m ()
unreadETokens !depth ts = do
  limit <- use esMaxPendingToken
  ts' <- use esPendingTokenList
  maxDepth <- use esMaxDepth
  when (depth >= maxDepth) $ throwError "recursion too deep"
  when (length ts' + length ts > limit) $ throwError "token list too long"
  assign esPendingTokenList (map ((,) depth) ts ++ ts')

readUntilEndGroup :: (MonadTeXState a m, MonadError String m) => Bool -> Int -> [TeXToken] -> m [TeXToken]
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
readArgument :: (MonadTeXState a m, MonadError String m) => Bool -> m [TeXToken]
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
readCommandName :: (MonadTeXState a m, MonadError String m) => m CommandName
readCommandName = do
  t <- required nextEToken
  case t of
    ETCommandName _ name -> return name
    _ -> throwError $ "unexpected character token: " ++ show t
         -- or, "Missing control sequence inserted"

-- used by \unexpanded
isExpandableNameF :: (MonadTeXState a m) => m (TeXToken -> Bool)
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
expandOnce :: (MonadTeXState a m, MonadError String m) => ExpansionToken -> m [ExpansionToken]
expandOnce et@(ETCommandName False name) = do
  m <- use (localState . definitionAt name)
  case m of
    Left e -> runExpandable e
    _ -> return [et]
expandOnce et = return [et]

-- used by number reading, \if and \ifcat argument, general text
evalToken :: (MonadTeXState a m, MonadError String m) => m (ExpansionToken,Value a)
evalToken = do
  (d,t) <- required nextETokenWithDepth
  case t of
    ETCommandName False name -> do
      m <- use (localState . definitionAt name)
      case m of
        Left e@(ConditionalMarker v) -> do
          cs <- use conditionals
          case cs of
            CondTest:_ -> do
              unreadETokens d [t]
              return (ETCommandName False (NControlSeq "relax"), Relax)
            _ -> do
              r <- runExpandable e
              unreadETokens (d+1) r
              evalToken
        Left e -> do
          r <- runExpandable e
          unreadETokens (d+1) r
          evalToken
        Right v -> return (t,v) -- non-expandable commands are not executed
    ETCommandName True name -> do
      return (t,Unexpanded name)
    ETCharacter c cc ->
      return (t,Character c cc)

evalToValue :: (MonadTeXState a m, MonadError String m) => m (Maybe (Value a))
evalToValue = do
  et <- nextETokenWithDepth
  case et of
    Just (d,t) -> do
      case t of
        ETCommandName False name -> do
          m <- use (localState . definitionAt name)
          case m of
            Left e -> do
              r <- runExpandable e
              unreadETokens (d+1) r
              evalToValue
            Right v -> return (Just v)
        ETCommandName True name -> do
          return (Just (Unexpanded name))
        ETCharacter c cc ->
          return (Just (Character c cc))
    Nothing -> return Nothing

required :: (MonadError String m) => m (Maybe a) -> m a
required m = do a <- m
                case a of
                  Nothing -> throwError "unexpected end of input"
                  Just a -> return a

expandafterCommand :: (MonadTeXState a m, MonadError String m) => m [ExpansionToken]
expandafterCommand = do
  t1 <- required nextEToken
  t2 <- required nextEToken
  (t1:) <$> expandOnce t2

noexpandCommand :: (MonadTeXState a m, MonadError String m) => m [ExpansionToken]
noexpandCommand = do
  t <- required nextEToken
  case t of
    ETCommandName False name -> do
      m <- use (localState . definitionAt name)
      return $ case m of
        Left _ -> [ETCommandName True name] -- expandable
        Right (Undefined _) -> [ETCommandName True name] -- undefined
        Right _ -> [t] -- not expandable
    _ -> return [t]

-- used by \csname and \ifcsname
readUntilEndcsname :: (MonadTeXState a m, MonadError String m) => [Char] -> m [Char]
readUntilEndcsname revName = do
  (t,v) <- evalToken
  case v of
    Endcsname -> return (reverse revName)
    _ -> case t of
      ETCommandName _ name -> throwError $ "unexpected " ++ show name ++ " while looking for \\endcsname" -- not expandable, or \noexpand-ed
      ETCharacter c _ -> readUntilEndcsname (c:revName) -- non-active character

csnameCommand :: (MonadTeXState a m, MonadError String m) => m [ExpansionToken]
csnameCommand = do
  name <- readUntilEndcsname []
  let tname = T.pack name

  -- THE DREADED SIDE EFFECT OF \csname
  d <- use (localState . tsDefinitions)
  when (Map.notMember tname d)
    $ modifying (localState . tsDefinitions) (Map.insert tname (Right Relax))

  return [ETCommandName False (NControlSeq tname)]

-- LuaTeX extension: \begincsname
begincsnameCommand :: (MonadTeXState a m, MonadError String m) => m [ExpansionToken]
begincsnameCommand = do
  name <- readUntilEndcsname []
  let tname = T.pack name
  return [ETCommandName False (NControlSeq tname)]

stringToEToken :: (MonadTeXState a m, MonadError String m) => String -> m [ExpansionToken]
stringToEToken [] = return []
stringToEToken (' ':xs) = (ETCharacter ' ' CCSpace :) <$> stringToEToken xs
stringToEToken (x:xs) = (ETCharacter x CCOther :) <$> stringToEToken xs

stringCommand :: (MonadTeXState a m, MonadError String m) => m [ExpansionToken]
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
csstringCommand :: (MonadTeXState a m, MonadError String m) => m [ExpansionToken]
csstringCommand = do
  t <- required nextEToken
  case t of
    ETCommandName _ (NControlSeq name) ->
      stringToEToken (T.unpack name)
    ETCommandName _ (NActiveChar c) ->
      stringToEToken [c]
    ETCharacter c _ ->
      stringToEToken [c]

readOptionalSigns :: (MonadTeXState a m, MonadError String m) => Int -> m Int
readOptionalSigns !s = do
  (t,v) <- evalToken
  case v of
    Character ' ' CCSpace -> readOptionalSigns s -- space: ignored
    Character '+' CCOther -> readOptionalSigns s
    Character '-' CCOther -> readOptionalSigns (-s)
    _ -> unreadETokens 0 [t] >> return s

readUnsignedDecimal :: (MonadTeXState a m, MonadError String m) => Char -> m Integer
readUnsignedDecimal c = readRest (fromIntegral (digitToInt c))
  where readRest !x = do
          (t,v) <- evalToken
          case v of
            Character c CCOther | isDigit c -> do
                                    readRest (10 * x + fromIntegral (digitToInt c))
            Character ' ' CCSpace -> return x -- consumed
            _ -> unreadETokens 0 [t] >> return x

readUnsignedOctal :: (MonadTeXState a m, MonadError String m) => m Integer
readUnsignedOctal = do
  (t,v) <- evalToken
  case v of
    Character c CCOther | isOctDigit c -> do
                            readRest (fromIntegral (digitToInt c))
    _ -> throwError $ "unexpected token while reading octal: " ++ show t
  where readRest !x = do
          (t,v) <- evalToken
          case v of
            Character c CCOther | isOctDigit c -> do
                                    readRest (8 * x + fromIntegral (digitToInt c))
            Character ' ' CCSpace -> return x -- consumed
            _ -> unreadETokens 0 [t] >> return x

readUnsignedHex :: (MonadTeXState a m, MonadError String m) => m Integer
readUnsignedHex = do
  (t,v) <- evalToken
  case v of
    Character c CCOther | isUpperHexDigit c -> do
                            let c0 = digitToInt c
                            readRest (fromIntegral c0)
    Character c CCLetter | isHexDigit c && isAsciiUpper c -> do
                             let c0 = digitToInt c
                             readRest (fromIntegral c0)
    _ -> throwError $ "unexpected token while reading hexadecimal: " ++ show t
  where readRest !x = do
          (t,v) <- evalToken
          case v of
            Character c CCOther | isUpperHexDigit c -> do
                                    readRest (16 * x + fromIntegral (digitToInt c))
            Character c CCLetter | isHexDigit c && isAsciiUpper c -> do
                                     readRest (16 * x + fromIntegral (digitToInt c))
            Character ' ' CCSpace -> return x -- consumed
            _ -> unreadETokens 0 [t] >> return x
        isUpperHexDigit c = isHexDigit c && (isDigit c || isAsciiUpper c)

readCharacterCode :: (MonadTeXState a m, MonadError String m) => m Integer
readCharacterCode = do
  t <- required nextEToken
  -- TODO: read an optional space
  case t of
    ETCommandName _ (NControlSeq name) -> case T.unpack name of
      [c] -> return (fromIntegral $ ord c)
      _ -> throwError "Improper alphabetic constant."
    ETCommandName _ (NActiveChar c) -> return (fromIntegral $ ord c)
    ETCharacter c _ -> return (fromIntegral $ ord c)

readNumber :: (MonadTeXState a m, MonadError String m) => m Integer
readNumber = do
  sign <- readOptionalSigns 1
  (t,v) <- evalToken
  (fromIntegral sign *) <$> case v of
    Character '\'' CCOther -> readUnsignedOctal
    Character '"' CCOther -> readUnsignedHex
    Character '`' CCOther -> readCharacterCode
    Character c CCOther | isDigit c -> readUnsignedDecimal c
    DefinedCharacter c -> return (fromIntegral $ ord c)
    IntegerConstant x -> return (fromIntegral x)
    _ -> throwError $ "unexpected token while reading number: " ++ show t -- Missing number, treated as zero.

numberCommand :: (MonadTeXState a m, MonadError String m) => m [ExpansionToken]
numberCommand = do
  x <- readNumber
  stringToEToken (show x)

theCommand :: (MonadTeXState a m, MonadError String m) => m [ExpansionToken]
theCommand = do
  x <- readNumber
  -- TODO: support other quantities
  stringToEToken (show x)

meaningCommand :: (MonadTeXState a m, MonadError String m) => m [ExpansionToken]
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

romannumeralCommand :: (MonadTeXState a m, MonadError String m) => m [ExpansionToken]
romannumeralCommand = do
  x <- readNumber
  if x < 0
    then return []
    else if x > fromIntegral (maxBound :: Int)
         then throwError "\\romannumeral: too large"
         else stringToEToken (showRomannumeral (fromIntegral x))

-- to be used by conditionals, \or, \else
-- does not actually expand the token.
shallowEval :: (MonadTeXState a m, MonadError String m) => m (Maybe (Expandable a))
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
skipUntilElse :: (MonadTeXState a m, MonadError String m) => Int -> m Bool
skipUntilElse !level = do
  x <- shallowEval
  case x of
    Just (ConditionalMarker Eelse) | level == 0 -> return True
    Just (ConditionalMarker Efi) | level == 0 -> return False
                                 | otherwise -> skipUntilElse (level - 1)
    Just (ConditionalMarker Eor) | level == 0 -> throwError "Extra \\or"
    Just c | isConditional c -> skipUntilElse (level + 1)
    _ -> skipUntilElse level

skipUntilFi :: (MonadTeXState a m, MonadError String m) => Int -> m ()
skipUntilFi !level = do
  x <- shallowEval
  case x of
    Just (ConditionalMarker Efi) | level == 0 -> return ()
                                 | otherwise -> skipUntilFi (level - 1)
    Just c | isConditional c -> skipUntilFi (level + 1)
    _ -> skipUntilFi level

data SkipUntilOr = FoundOr
                 | FoundElse
                 | FoundFi

skipUntilOr :: (MonadTeXState a m, MonadError String m) => Int -> m SkipUntilOr
skipUntilOr !level = do
  x <- shallowEval
  case x of
    Just (ConditionalMarker Eor) | level == 0 -> return FoundOr
    Just (ConditionalMarker Eelse) | level == 0 -> return FoundElse
    Just (ConditionalMarker Efi) | level == 0 -> return FoundFi
                                 | otherwise -> skipUntilOr (level - 1)
    Just c | isConditional c -> skipUntilOr (level + 1)
    _ -> skipUntilOr level

doBooleanConditional :: (MonadTeXState a m, MonadError String m) => Bool -> m ()
doBooleanConditional True = do
  modifying conditionals (\(CondTest:xs) -> CondTruthy:xs)
doBooleanConditional False = do
  e <- skipUntilElse 0
  if e
    then modifying conditionals (\(CondTest:xs) -> CondFalsy:xs)
    else modifying conditionals tail

elseCommand :: (MonadTeXState a m, MonadError String m) => m [ExpansionToken]
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

fiCommand :: (MonadTeXState a m, MonadError String m) => m [ExpansionToken]
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

orCommand :: (MonadTeXState a m, MonadError String m) => m [ExpansionToken]
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

doIfCase :: (MonadTeXState a m, MonadError String m) => Integer -> m ()
doIfCase 0 = do
  modifying conditionals (CondCase:)
doIfCase n = do
  k <- skipUntilOr 0
  case k of
    FoundFi -> return ()
    FoundElse -> modifying conditionals (CondFalsy:)
    FoundOr -> doIfCase (n - 1)

ifcaseCommand :: (MonadTeXState a m, MonadError String m) => m [ExpansionToken]
ifcaseCommand = do
  x <- readNumber
  doIfCase x
  return []

runExpandable :: (MonadTeXState a m, MonadError String m) => Expandable a -> m [ExpansionToken]
runExpandable (ConditionalMarker v) = case v of
  Eelse -> elseCommand
  Efi -> fiCommand
  Eor -> orCommand
runExpandable (ExpandableCommand c) = c
runExpandable (BooleanConditionalCommand c) = do
    modifying conditionals (CondTest:)
    b <- c
    doBooleanConditional b
    return []
runExpandable IfCase = ifcaseCommand

iftrueCommand :: (MonadTeXState a m, MonadError String m) => m Bool
iftrueCommand = return True

iffalseCommand :: (MonadTeXState a m, MonadError String m) => m Bool
iffalseCommand = return False

-- \if: test character codes
ifCommand :: (MonadTeXState a m, MonadError String m) => m Bool
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

meaning :: (MonadTeXState a m, MonadError String m) => ExpansionToken -> m (Either (Expandable a) (Value a))
meaning t = do
  case t of
    ETCommandName True name -> return (Right (Unexpanded name))
    ETCommandName False name -> use (localState . definitionAt name)
    ETCharacter c cc -> return (Right (Character c cc))

-- \ifx: test category codes
ifxCommand :: (MonadTeXState a m, MonadError String m) => m Bool
ifxCommand = do
  t1 <- required nextEToken >>= meaning
  t2 <- required nextEToken >>= meaning
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

ifnumCommand :: (MonadTeXState a m, MonadError String m) => m Bool
ifnumCommand = do
  x <- readNumber
  rel <- required nextEToken >>= meaning
  y <- readNumber
  case rel of
    Right (Character '<' CCOther) -> return $ x < y
    Right (Character '=' CCOther) -> return $ x == y
    Right (Character '>' CCOther) -> return $ x > y
    _ -> throwError "unrecognized relation for \\ifnum"

ifoddCommand :: (MonadTeXState a m, MonadError String m) => m Bool
ifoddCommand = odd <$> readNumber

ifhmodeCommand :: (MonadTeXState a m, MonadError String m) => m Bool
ifhmodeCommand = uses mode isHMode

ifvmodeCommand :: (MonadTeXState a m, MonadError String m) => m Bool
ifvmodeCommand = uses mode isVMode

ifmmodeCommand :: (MonadTeXState a m, MonadError String m) => m Bool
ifmmodeCommand = uses mode isMMode

ifinnerCommand :: (MonadTeXState a m, MonadError String m) => m Bool
ifinnerCommand = uses mode isInnerMode

-- e-TeX extension: \ifdefined
ifdefinedCommand :: (MonadTeXState a m, MonadError String m) => m Bool
ifdefinedCommand = do
  t <- required nextEToken >>= meaning
  case t of
    Right (Undefined _) -> return False
    _ -> return True

-- e-TeX extension: \ifcsname
ifcsnameCommand :: (MonadTeXState a m, MonadError String m) => m Bool
ifcsnameCommand = do
  name <- readUntilEndcsname []
  let tname = T.pack name
  d <- use (localState . tsDefinitions)
  return (Map.member tname d)

-- e-TeX extension: \unless
unlessCommand :: (MonadTeXState a m, MonadError String m) => m [ExpansionToken]
unlessCommand = do
  test <- required nextEToken >>= meaning
  case test of
    Left (BooleanConditionalCommand c) -> do
      modifying conditionals (CondTest:)
      b <- c
      doBooleanConditional (not b)
      return []
    _ -> throwError "\\unless must be followed by a boolean conditional command"

readGeneralText :: (MonadTeXState a m, MonadError String m) => m [TeXToken]
readGeneralText = do
  (t,v) <- evalToken
  case v of
    Character ' ' CCSpace -> readGeneralText -- optional spaces: ignored
    Character _ CCBeginGroup -> readUntilEndGroup True 0 []
    Relax -> readGeneralText -- relax: ignored
    _ -> throwError $ "unexpected token " ++ show t -- Missing { inserted

-- e-TeX extension: \unexpanded
unexpandedCommand :: (MonadTeXState a m, MonadError String m) => m [ExpansionToken]
unexpandedCommand = do
  ie <- isExpandableNameF
  -- map (\t -> ExpansionToken (ie t) t) <$> readGeneralText
  throwError "\\unexpanded: not implemented yet"

expandableDefinitions :: Map.Map Text (Expandable a)
expandableDefinitions = Map.fromList
  [("expandafter", ExpandableCommand expandafterCommand)
  ,("noexpand",    ExpandableCommand noexpandCommand)
  ,("csname",      ExpandableCommand csnameCommand)
  ,("string",      ExpandableCommand stringCommand)
  ,("number",      ExpandableCommand numberCommand)
  ,("romannumeral",ExpandableCommand romannumeralCommand)
  ,("the",         ExpandableCommand theCommand)
  ,("meaning",     ExpandableCommand meaningCommand)
  -- ,("endcsname",   Endcsname)
  ,("else",        ConditionalMarker Eelse)
  ,("fi",          ConditionalMarker Efi)
  ,("or",          ConditionalMarker Eor)
  ,("ifcase",      IfCase)
  ,("iftrue",      BooleanConditionalCommand iftrueCommand)
  ,("iffalse",     BooleanConditionalCommand iffalseCommand)
  ,("if",          BooleanConditionalCommand ifCommand)
  ,("ifcat",       BooleanConditionalCommand ifcatCommand)
  ,("ifx",         BooleanConditionalCommand ifxCommand)
  ,("ifnum",       BooleanConditionalCommand ifnumCommand)
  ,("ifodd",       BooleanConditionalCommand ifoddCommand)
  ,("ifhmode",     BooleanConditionalCommand ifhmodeCommand)
  ,("ifvmode",     BooleanConditionalCommand ifvmodeCommand)
  ,("ifmmode",     BooleanConditionalCommand ifmmodeCommand)
  ,("ifinner",     BooleanConditionalCommand ifinnerCommand)

  -- e-TeX extension:
  ,("ifdefined",   BooleanConditionalCommand ifdefinedCommand)
  ,("ifcsname",    BooleanConditionalCommand ifcsnameCommand)
  ,("unless",      ExpandableCommand unlessCommand)
  ,("unexpanded",  ExpandableCommand unexpandedCommand)

  -- LuaTeX extension:
  ,("begincsname", ExpandableCommand begincsnameCommand)
  ,("csstring",    ExpandableCommand csstringCommand)
  ]

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
