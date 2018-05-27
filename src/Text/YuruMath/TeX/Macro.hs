{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Text.YuruMath.TeX.Macro where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Expansion
import Control.Monad
import Control.Monad.Error.Class
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.OpenUnion
import TypeFun.Data.List (Elem)
import qualified Data.Map.Strict as Map
import Control.Lens.Getter (use)
import Control.Lens.Setter (assign)

data MacroDefPrefix = MacroDefPrefix
                      { prefixGlobal    :: !Bool
                      , prefixLong      :: !Bool
                      , prefixOuter     :: !Bool
                      , prefixProtected :: !Bool
                      }
                    deriving (Eq)

unprefixed :: MacroDefPrefix
unprefixed = MacroDefPrefix False False False False

globalPrefix :: MacroDefPrefix
globalPrefix = MacroDefPrefix True False False False

data MacroParamSpec
  = StandardMandatory { paramSpecIsLong   :: !ParamLong } -- m
  | DelimitedByLBrace { paramSpecIsLong   :: !ParamLong } -- l
  | RequiredDelimited { paramSpecIsLong   :: !ParamLong
                      , paramSpecBalanced :: !Bool
                      , paramSpecOpen     :: !TeXToken
                      , paramSpecClose    :: !TeXToken
                      , paramSpecDefault  :: [TeXToken] -- for error recovery
                      } -- r<char1><char2>, R<char1><char2>{<default>}
  | Until { paramSpecIsLong    :: !ParamLong
          , paramSpecDelimiter :: [TeXToken]
          } -- #n<token list>, u{<tokens>}
  | Verbatim -- v
  | OptionalDelimited { paramSpecIsLong       :: !ParamLong
                      , paramSpecConsumeSpace :: ConsumeSpace
                      , paramSpecBalanced     :: !Bool
                      , paramSpecOpen         :: !TeXToken
                      , paramSpecClose        :: !TeXToken
                      , paramSpecDefault      :: [TeXToken]
                      } -- o ("-NoValue-" if omitted), O{<default>}, d<char1><char2> ("-NoValue-" if omitted), D<char1><char2>{<default>}
  | OptionalChar { paramSpecConsumeSpace :: ConsumeSpace
                 , paramSpecToken        :: !TeXToken -- like '*'
                 , paramSpecIfTrue       :: [TeXToken] -- \BooleanTrue
                 , paramSpecIfFalse      :: [TeXToken] -- \BooleanFalse
                 } -- s, t
  | OptionalGroup { paramSpecIsLong       :: !ParamLong
                  , paramSpecConsumeSpace :: ConsumeSpace
                  , paramSpecDefault      :: [TeXToken]
                  } -- g ("-NoValue-" if omitted), G{<default>}
  -- TODO: 'g', embellishments
  deriving (Eq,Show)

data Macro = Macro { macroIsOuter :: !Bool
                   , macroIsProtected :: !Bool
                   , macroDelimiterBeforeFirstParam :: [TeXToken]
                   , macroParamSpec :: [MacroParamSpec]
                   , macroReplacement :: [TeXToken] -- TODO: Generate 'parameter token'
                   }
           deriving (Eq,Show)

-- Make a macro.
-- TODO: parameter token?
mkSimpleMacro :: [MacroParamSpec] -> [TeXToken] -> Macro
mkSimpleMacro param rep = Macro { macroIsOuter = False
                                , macroIsProtected = False
                                , macroDelimiterBeforeFirstParam = []
                                , macroParamSpec = param
                                , macroReplacement = rep
                                }

-- \def, defCommand
-- \gdef, gdefCommand
-- \edef, edefCommand
-- \xdef, xdefCommand
-- LaTeX2e:
-- \newcommand, newcommandCommand
-- \renewcommand, renewcommandCommand
-- \providecommand, providecommandCommand
-- \DeclareRobustCommand
-- xparse:
-- \NewDocumentCommand
-- \RenewDocumentCommand
-- \ProvideDocumentCommand
-- \DeclareDocumentCommand
-- \NewExpandableDocumentCommand
-- \RenewExpandableDocumentCommand
-- \ProvideExpandableDocumentCommand
-- \DeclareExpandableDocumentCommand

data ConsumeSpace = ConsumeSpaces
                  | Don'tConsumeSpace
                  | WarnIfConsumedSpaces
                  deriving (Eq,Show)

noValueMarker :: [TeXToken]
noValueMarker = [TTCharacter '-' CCLetter
                ,TTCharacter 'N' CCLetter
                ,TTCharacter 'o' CCLetter
                ,TTCharacter 'V' CCLetter
                ,TTCharacter 'a' CCLetter
                ,TTCharacter 'l' CCLetter
                ,TTCharacter 'u' CCLetter
                ,TTCharacter 'e' CCLetter
                ,TTCharacter '-' CCOther
                ]

testDelimiter :: (MonadTeXState s m, MonadError String m) => [TeXToken] -> m Bool
testDelimiter [] = return True
testDelimiter (d:ds) = do
  et <- nextEToken
  case et of
    Nothing -> return False
    Just et
      | d == fromEToken et -> do
          r <- testDelimiter ds
          if r
            then return True -- consumed
            else do unreadETokens 0 [et]
                    return False
      | otherwise -> do
          unreadETokens 0 [et]
          return False

testLBrace :: (MonadTeXState s m, MonadError String m) => ConsumeSpace -> m Bool
testLBrace !consumeSpace = do
  et <- nextEToken
  case et of
    Nothing -> return False
    Just (ETCharacter { etCatCode = CCBeginGroup }) -> return True
    Just (ETCharacter { etCatCode = CCSpace })
      | consumeSpace == ConsumeSpaces -> testLBrace consumeSpace
      | consumeSpace == WarnIfConsumedSpaces -> do
          -- TODO: Issue a warning
          testLBrace consumeSpace
    Just et -> do
      unreadETokens 0 [et]
      return False

readRequiredDelimiter :: (MonadTeXState s m, MonadError String m) => ConsumeSpace -> [TeXToken] -> m ()
readRequiredDelimiter !consumeSpace [] = return ()
readRequiredDelimiter !consumeSpace d@(d0:ds) = do
  et <- nextEToken
  case et of
    Nothing -> throwError "Unexpected end of input"
    Just et | fromEToken et == d0 -> rest ds
    Just (ETCharacter { etCatCode = CCSpace })
      | consumeSpace == ConsumeSpaces -> readRequiredDelimiter consumeSpace d -- gobble spaces
      | consumeSpace == WarnIfConsumedSpaces -> do
          -- TODO: Issue warning
          readRequiredDelimiter consumeSpace d
    Just et -> throwError "Unexpected token"
  where
    rest [] = return ()
    rest (d0:ds) = do
      et <- nextEToken
      case et of
        Nothing -> throwError "Unexpected end of input"
        Just et | fromEToken et == d0 -> rest ds
                | otherwise -> throwError "Unexpected token"

data DelimitedArgumentToken = Grouped !TeXToken [TeXToken] !TeXToken
                            | Bare !TeXToken

unArgToken :: [DelimitedArgumentToken] -> [TeXToken]
unArgToken [Grouped _ c _] = c -- outermost { } are stripped off if it is the sole argument text
unArgToken xs = loop xs
  where
    loop [] = []
    loop (Grouped t1 t2 t3 : xs) = t1 : (t2 ++ (t3 : loop xs))
    loop (Bare t : xs) = t : loop xs

readDelimitedArgument :: (MonadTeXState s m, MonadError String m) => ParamLong -> [TeXToken] -> m [TeXToken]
readDelimitedArgument !long !delimiter = unArgToken <$> loop
  where
    loop = do
      r <- testDelimiter delimiter
      if r
        then return []
        else do et <- nextEToken
                case et of
                  Nothing -> throwError "Unexpected end of input while reading argument"
                  Just (ETCommandName { etName = NControlSeq "par" })
                    | long == ShortParam -> throwError "Paragraph ended before argument was complete"
                  Just et@(ETCharacter { etCatCode = CCBeginGroup }) -> do
                    (t,c) <- readUntilEndGroup' long
                    (Grouped (fromEToken et) t c :) <$> loop
                  Just (ETCharacter { etCatCode = CCEndGroup }) ->
                    throwError "Argument of <macro> has an extra }"
                  Just et -> do
                    (Bare (fromEToken et) :) <$> loop

-- Reads an optional token (like '*' or '[') without gobbling any spaces
testOptionalToken :: (MonadTeXState s m, MonadError String m) => ConsumeSpace -> TeXToken -> m Bool
testOptionalToken !consume !t = do
  et <- nextEToken
  case et of
    Nothing -> return False
    Just (ETCharacter { etCatCode = CCSpace })
      | consume == ConsumeSpaces -> testOptionalToken consume t
      | consume == WarnIfConsumedSpaces -> do
          -- TODO: Issue a warning
          testOptionalToken consume t
    Just et | fromEToken et == t -> return True
            | otherwise -> do unreadETokens 0 [et]
                              return False

testStar :: (MonadTeXState s m, MonadError String m) => ConsumeSpace -> m Bool
testStar !consume = testOptionalToken consume (TTCharacter '*' CCOther)

-- Reads LaTeX-style optional argument
readOptional :: (MonadTeXState s m, MonadError String m) => ConsumeSpace -> ParamLong -> m (Maybe [TeXToken])
readOptional !consumeSpace !long = do
  x <- testOptionalToken consumeSpace (TTCharacter '[' CCOther)
  if x
    then Just <$> readDelimitedArgument long [TTCharacter ']' CCOther]
    else return Nothing

readUntilEndGroup' :: (MonadTeXState s m, MonadError String m) => ParamLong -> m ([TeXToken], TeXToken)
readUntilEndGroup' !long = loop (0 :: Int) []
  where
    loop !depth revTokens = do
      t <- nextEToken
      case t of
        Nothing -> throwError "unexpected end of input when reading an argument"
        Just t@(ETCharacter { etCatCode = CCEndGroup })
          | depth == 0 -> return (reverse revTokens, fromEToken t)
          | otherwise -> loop (depth - 1) (fromEToken t : revTokens)
        Just t@(ETCharacter { etCatCode = CCBeginGroup })
          -> loop (depth + 1) (fromEToken t : revTokens)
        Just (ETCommandName { etName = NControlSeq "par" })
          | long == ShortParam -> throwError "Paragraph ended before argument was compelete"
        Just t -> loop depth (fromEToken t : revTokens)

readUntilBeginGroup :: (MonadTeXState s m, MonadError String m) => ParamLong -> [TeXToken] -> m [TeXToken]
readUntilBeginGroup !long revTokens = do
  t <- nextEToken
  case t of
    Nothing -> throwError "Unexpected end of input when reading an argument"
    Just t@(ETCharacter { etCatCode = CCBeginGroup }) -> do
      unreadETokens 0 [t]
      return (reverse revTokens)
    Just t@(ETCharacter { etCatCode = CCEndGroup }) -> throwError "Argument of <macro> has an extra }"
    Just (ETCommandName { etName = NControlSeq "par" })
      | long == ShortParam -> throwError "Paragraph ended before argument was complete"
    Just t -> readUntilBeginGroup long (fromEToken t : revTokens)

readBalanced :: (MonadTeXState s m, MonadError String m) => ParamLong -> TeXToken -> TeXToken -> m [TeXToken]
readBalanced !long !c1 !c2 = unArgToken <$> loop (0 :: Int)
  where
    loop !depth = do
      et <- nextEToken
      case et of
        Nothing -> throwError "Unexpected end of input"
        Just (ETCommandName { etName = NControlSeq "par" })
          | long == ShortParam -> throwError "Paragraph ended before argument was complete"
        Just et@(ETCharacter { etCatCode = CCBeginGroup }) -> do
          (t,c) <- readUntilEndGroup' long
          (Grouped (fromEToken et) t c :) <$> loop depth
        Just (ETCharacter { etCatCode = CCEndGroup }) ->
          throwError "Argument of <macro> has an extra }"
        Just et | fromEToken et == c2, depth == 0 -> return []
                | fromEToken et == c2, depth > 0 -> (Bare (fromEToken et) :) <$> loop (depth - 1)
                | fromEToken et == c1 -> (Bare (fromEToken et) :) <$> loop (depth + 1)
                | otherwise -> (Bare (fromEToken et) :) <$> loop depth

readParam :: (MonadTeXState s m, MonadError String m) => MacroParamSpec -> m [TeXToken]
readParam spec = case spec of
  StandardMandatory { paramSpecIsLong = long } ->
    readArgument long

  DelimitedByLBrace { paramSpecIsLong = long } ->
    readUntilBeginGroup long []

  RequiredDelimited { paramSpecIsLong = long
                    , paramSpecOpen = c1
                    , paramSpecClose = c2
                    } -> do
    readRequiredDelimiter ConsumeSpaces [c1]
    readDelimitedArgument long [c2]

  Until { paramSpecIsLong = long, paramSpecDelimiter = delim } ->
    readDelimitedArgument long delim

  Verbatim {} -> throwError "Verbatim argument: not implemented yet"

  OptionalDelimited { paramSpecIsLong = long
                    , paramSpecConsumeSpace = consumeSpace
                    , paramSpecBalanced = False
                    , paramSpecOpen = c1
                    , paramSpecClose = c2
                    , paramSpecDefault = default_
                    } -> do
    x <- testOptionalToken consumeSpace c1
    if x
      then readDelimitedArgument long [c2]
      else return default_

  OptionalDelimited { paramSpecIsLong = long
                    , paramSpecConsumeSpace = consumeSpace
                    , paramSpecBalanced = True
                    , paramSpecOpen = c1
                    , paramSpecClose = c2
                    , paramSpecDefault = default_
                    } -> do
    x <- testOptionalToken consumeSpace c1
    if x
      then readBalanced long c1 c2
      else return default_

  OptionalChar { paramSpecConsumeSpace = consumeSpace
               , paramSpecToken = t
               , paramSpecIfTrue = then_
               , paramSpecIfFalse = else_
               } -> do
    x <- testOptionalToken consumeSpace t
    return $ if x
             then then_
             else else_

  OptionalGroup { paramSpecIsLong = long
                , paramSpecConsumeSpace = consumeSpace
                , paramSpecDefault = default_
                } -> do
    x <- testLBrace consumeSpace
    if x
      then readUntilEndGroup long
      else return default_

doMacro :: (MonadTeXState s m, MonadError String m) => Macro -> m [TeXToken]
doMacro m = do
  readRequiredDelimiter Don'tConsumeSpace $ macroDelimiterBeforeFirstParam m
  args <- mapM readParam (macroParamSpec m)
  let doReplace [] = return []
      doReplace (TTCharacter c CCParam : xs) = case xs of
        TTCharacter d CCOther : xss
          | isDigit d
          , d /= '0'
          , let u = digitToInt d - 1
          , u < length args -> (args !! u ++ ) <$> doReplace xss
        d@(TTCharacter _ CCParam) : xss -> (d :) <$> doReplace xss
        _ -> throwError "Illegal parameter number in definition of <macro>"
      doReplace (x : xs) = (x :) <$> doReplace xs
  doReplace (macroReplacement m)

instance IsExpandable Macro where
  isConditional _ = False

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExpand Macro m where
  doExpand m = map toEToken <$> doMacro m
  evalBooleanConditional _ = Nothing

-- LaTeX 2e
-- \newcommand, \renewcommand, \providecommand
newcommandFamily :: (Elem Macro set, Union set ~ Expandable s, MonadTeXState s m, MonadError String m) => (CommandName -> Bool -> m () -> m a) -> m a
newcommandFamily f = do
  star       <- testStar ConsumeSpaces
  name       <- readArgument ShortParam
  numOfArgs  <- readOptional ConsumeSpaces ShortParam
  defaultArg <- readOptional ConsumeSpaces LongParam
  repText    <- readArgument LongParam
  name <- case name of
    [TTCommandName name] -> return name
    _ -> throwError "Invalid command name"
  case name of
    NControlSeq name | name == "relax" || "end" `T.isPrefixOf` name ->
                       throwError $ "LaTeX Error: The name \\" ++ T.unpack name ++ " is illegal"
    _ -> return ()
  v <- use (localState . definitionAt name)
  let isUndefined | Right r <- v
                  , Just v <- toCommonValue r = case v of
                      Relax -> True -- TODO: Issue a warning?
                      Undefined _ -> True
                      _ -> False
                  | otherwise = False
  f name isUndefined $ do
    numOfArgs <- case numOfArgs of
                   Nothing -> return 0
                   Just [TTCharacter c CCOther]
                     | isDigit c -> return (digitToInt c)
                   _ -> throwError "Invalid number of arguments" -- TODO: Support internal integers?
    let long | star      = ShortParam -- \newcommand*
             | otherwise = LongParam  -- \newcommand
    macro <- case defaultArg of
               Nothing -> return $ Macro
                          { macroIsOuter = False
                          , macroIsProtected = False
                          , macroDelimiterBeforeFirstParam = []
                          , macroParamSpec = replicate numOfArgs $ StandardMandatory long
                          , macroReplacement = repText
                          }
               Just defaultArg
                 | numOfArgs == 0 -> throwError "Unexpected default argument"
                 | otherwise -> return $ Macro
                                { macroIsOuter = False
                                , macroIsProtected = False
                                , macroDelimiterBeforeFirstParam = []
                                , macroParamSpec = OptionalDelimited
                                                   { paramSpecIsLong = long
                                                   , paramSpecConsumeSpace = if numOfArgs == 1 then WarnIfConsumedSpaces else ConsumeSpaces
                                                   , paramSpecBalanced = False
                                                   , paramSpecOpen = TTCharacter '[' CCOther
                                                   , paramSpecClose = TTCharacter ']' CCOther
                                                   , paramSpecDefault = defaultArg
                                                   } : replicate (numOfArgs - 1) (StandardMandatory long)
                                , macroReplacement = repText
                                }
    assign (localState . definitionAt name) (Left $ liftUnion macro)

newcommandCommand :: (Elem Macro set, Union set ~ Expandable s, MonadTeXState s m, MonadError String m) => m ()
newcommandCommand = newcommandFamily $ \name isUndefined doDefine -> do
  unless isUndefined $ throwError $ "LaTeX Error: The name " ++ show name ++ " is already defined"
  doDefine

renewcommandCommand :: (Elem Macro set, Union set ~ Expandable s, MonadTeXState s m, MonadError String m) => m ()
renewcommandCommand = newcommandFamily $ \name isUndefined doDefine -> do
  when isUndefined $ throwError $ "LaTeX Error: The name " ++ show name ++ " undefined"
  doDefine

providecommandCommand :: (Elem Macro set, Union set ~ Expandable s, MonadTeXState s m, MonadError String m) => m ()
providecommandCommand = newcommandFamily $ \_name isUndefined doDefine -> do
  when isUndefined doDefine

defCommand :: (Elem Macro set, Union set ~ Expandable s, MonadTeXState s m, MonadError String m) => String -> MacroDefPrefix -> m ()
defCommand name !prefix = throwError $ "\\" ++ name ++ ": not implemented yet"

edefCommand :: (Elem Macro set, Union set ~ Expandable s, MonadTeXState s m, MonadError String m) => String -> MacroDefPrefix -> m ()
edefCommand name !prefix = throwError $ "\\" ++ name ++ ": not implemented yet"

doPrefix :: (Elem Macro eset, Union eset ~ Expandable s, MonadTeXState s m, MonadError String m, Union vset ~ Value s, Show (Union vset)) => String -> MacroDefPrefix -> m ()
doPrefix name !prefix = do
  (et,v) <- evalToken
  case toCommonValue v of
    Just Relax -> doPrefix name prefix -- ignore \relax
    Just (Character _ CCSpace) -> doPrefix name prefix -- ignore spaces
    _ -> (onMacroCommand @> (\_ -> throwError $ "Invalid prefix " ++ name ++ " on " ++ show v)) v
  where
    onMacroCommand Mdef = defCommand "def" prefix
    onMacroCommand Medef = edefCommand "edef" prefix
    onMacroCommand Mgdef = defCommand "gdef" prefix { prefixGlobal = True }
    onMacroCommand Mxdef = edefCommand "xdef" prefix { prefixGlobal = True }
    onMacroCommand Mouter = doPrefix "outer" prefix { prefixOuter = True }
    onMacroCommand Mlong = doPrefix "long" prefix { prefixLong = True }
    onMacroCommand Mprotected = doPrefix "protected" prefix { prefixProtected = True }
    onMacroCommand v = throwError $ "Invalid prefix " ++ name ++ " on " ++ show v

data MacroCommand = Mdef
                  | Medef
                  | Mgdef
                  | Mxdef
                  | Mouter
                  | Mlong
                  | Mprotected
                  | Mnewcommand
                  | Mrenewcommand
                  | Mprovidecommand
                  deriving (Eq,Show)

instance (Elem Macro eset, Union eset ~ Expandable s, MonadTeXState s m, MonadError String m, Monad m, Union vset ~ Value s, Show (Union vset)) => DoExecute MacroCommand m where
  doExecute Mdef       = defCommand "def" unprefixed
  doExecute Medef      = edefCommand "edef" unprefixed
  doExecute Mgdef      = defCommand "gdef" globalPrefix
  doExecute Mxdef      = edefCommand "xdef" globalPrefix
  doExecute Mouter     = doPrefix "outer" (unprefixed { prefixOuter = True })
  doExecute Mlong      = doPrefix "long" (unprefixed { prefixLong = True })
  doExecute Mprotected = doPrefix "protected" (unprefixed { prefixProtected = True })
  doExecute Mnewcommand     = newcommandCommand
  doExecute Mrenewcommand   = renewcommandCommand
  doExecute Mprovidecommand = providecommandCommand
  doGlobal Mdef       = Just $ defCommand "def" globalPrefix
  doGlobal Medef      = Just $ edefCommand "edef" globalPrefix
  doGlobal Mgdef      = Just $ defCommand "gdef" globalPrefix
  doGlobal Mxdef      = Just $ edefCommand "xdef" globalPrefix
  doGlobal Mouter     = Just $ doPrefix "outer" (globalPrefix { prefixOuter = True })
  doGlobal Mlong      = Just $ doPrefix "long" (globalPrefix { prefixLong = True })
  doGlobal Mprotected = Just $ doPrefix "protected" (globalPrefix { prefixProtected = True })
  doGlobal _          = Nothing
  getIntegerValue _ = Nothing

macroCommands :: (Elem MacroCommand set) => Map.Map Text (Union set)
macroCommands = Map.fromList
  [("def",           liftUnion Mdef)
  ,("edef",          liftUnion Medef)
  ,("gdef",          liftUnion Mgdef)
  ,("xdef",          liftUnion Mxdef)
  ,("outer",         liftUnion Mouter)
  ,("long",          liftUnion Mlong)
  ,("protected",     liftUnion Mprotected)
  ,("newcommand",    liftUnion Mnewcommand)
  ,("renewcommand",  liftUnion Mrenewcommand)
  ,("providecommand",liftUnion Mprovidecommand)
  ]
