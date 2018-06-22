{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Text.YuruMath.TeX.Macro where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Expansion
import Text.YuruMath.TeX.Meaning
import Control.Monad
import Control.Monad.Error.Class
import Data.List
import Data.Char
import Data.String
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.OpenUnion
import TypeFun.Data.List (Elem)
import qualified Data.Map.Strict as Map
import Control.Lens.Getter (use)
import Control.Lens.Setter (assign,mapped)

data MacroDefPrefix = MacroDefPrefix
                      { prefixGlobal    :: !Bool
                      , prefixLong      :: !ParamLong
                      , prefixOuter     :: !Bool
                      , prefixProtected :: !Bool
                      }
                    deriving (Eq)

unprefixed :: MacroDefPrefix
unprefixed = MacroDefPrefix False ShortParam False False

globalPrefix :: MacroDefPrefix
globalPrefix = MacroDefPrefix True ShortParam False False

data ParamDelimiter = ParamDelimiter
                      { delimiterToken :: [TeXToken] -- empty if undelimited
                      , delimitedByLBrace :: !Bool
                      }
                    deriving (Eq,Show)

data MacroParamSpec
  = StandardMandatory { paramSpecIsLong   :: !ParamLong } -- m
  | RequiredDelimited { paramSpecIsLong   :: !ParamLong
                      , paramSpecBalanced :: !Bool
                      , paramSpecOpen     :: !TeXToken
                      , paramSpecClose    :: !TeXToken
                      , paramSpecDefault  :: [TeXToken] -- for error recovery
                      } -- r<char1><char2>, R<char1><char2>{<default>}
  | Until { paramSpecIsLong    :: !ParamLong
          , paramSpecDelimiter :: !ParamDelimiter
          } -- #n<token list>, u{<tokens>}, l
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
                   , macroDelimiterBeforeFirstParam :: ParamDelimiter
                   , macroParamSpec :: [MacroParamSpec]
                   , macroReplacement :: [TeXToken] -- TODO: Generate 'parameter token'
                   }
           deriving (Eq,Show)

-- Make a macro.
-- TODO: parameter token?
mkSimpleMacro :: [MacroParamSpec] -> [TeXToken] -> Macro
mkSimpleMacro param rep = Macro { macroIsOuter = False
                                , macroIsProtected = False
                                , macroDelimiterBeforeFirstParam = ParamDelimiter [] False
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
            else do unreadEToken et
                    return False
      | otherwise -> do
          unreadEToken et
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
      unreadEToken et
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
            | otherwise -> do unreadEToken et
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
      unreadEToken t
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

readExplicitLBrace :: (MonadTeXState s m, MonadError String m) => m ()
readExplicitLBrace = do
  t <- required nextEToken
  case t of
    ETCharacter { etCatCode = CCBeginGroup } -> return ()
    _ -> throwError ("Expected `{', but got " ++ show t) -- Use of \foo doesn't match its definition.

expectExplicitLBrace :: (MonadTeXState s m, MonadError String m) => m ()
expectExplicitLBrace = do
  t <- required nextEToken
  case t of
    ETCharacter { etCatCode = CCBeginGroup } -> unreadEToken t
    _ -> throwError ("Expected `{', but got " ++ show t) -- Use of \foo doesn't match its definition.

readParam :: (MonadTeXState s m, MonadError String m) => MacroParamSpec -> m [TeXToken]
readParam spec = case spec of
  StandardMandatory { paramSpecIsLong = long } ->
    readArgument long

  RequiredDelimited { paramSpecIsLong = long
                    , paramSpecOpen = c1
                    , paramSpecClose = c2
                    } -> do
    readRequiredDelimiter ConsumeSpaces [c1]
    readDelimitedArgument long [c2]

  Until { paramSpecIsLong = long, paramSpecDelimiter = ParamDelimiter [] True } ->
    readUntilBeginGroup long []

  Until { paramSpecIsLong = long, paramSpecDelimiter = ParamDelimiter delim lbrace } -> do
    xs <- readDelimitedArgument long delim
    when lbrace expectExplicitLBrace
    return xs

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
  case macroDelimiterBeforeFirstParam m of
    ParamDelimiter d lbrace -> do
      readRequiredDelimiter Don'tConsumeSpace d
      when lbrace expectExplicitLBrace
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

showParamDelimiter :: ParamDelimiter -> MessageString
showParamDelimiter (ParamDelimiter t lbrace)
  | lbrace = mconcat (map showToken t) <> "{"
  | otherwise = mconcat (map showToken t)

showParamSpec :: ParamLong -> Int -> Int -> MacroParamSpec -> MessageString
showParamSpec !isLong !totalParam !i param = case param of
  StandardMandatory long -> withDescription (paramLong long)

  RequiredDelimited {} -> withDescriptionD (paramSpecOpen param) (paramSpecClose param) $
    ["delimited"]
    ++ (if paramSpecBalanced param then ["balanced"] else [])
    ++ defaultValue (paramSpecDefault param)

  Until long del -> withDescription (paramLong long) <> showParamDelimiter del

  Verbatim -> withDescription ["verbatim"]

  OptionalDelimited {} -> withDescriptionD (paramSpecOpen param) (paramSpecClose param) $
    ["optional"]
    ++ consumeSpaces (paramSpecConsumeSpace param)
    ++ (if paramSpecBalanced param then ["balanced"] else [])
    ++ defaultValue (paramSpecDefault param)

  OptionalChar {} -> withDescription $
    [if paramSpecToken param == TTCharacter '*' CCOther
     then "optional star"
     else "optional char " <> showToken (paramSpecToken param)
    ]
    ++ consumeSpaces (paramSpecConsumeSpace param)
    ++ ["iftrue={" <> mconcat (map showToken (paramSpecIfTrue param)) <> "}"]
    ++ ["iffalse={" <> mconcat (map showToken (paramSpecIfFalse param)) <> "}"]

  OptionalGroup {} -> withDescription $
    ["optional group"]
    ++ consumeSpaces (paramSpecConsumeSpace param)
    ++ defaultValue (paramSpecDefault param)

  where paramLong long | isLong == long = []
                       | long == LongParam = ["long"]
                       | otherwise = ["short"]
        consumeSpaces ConsumeSpaces | i /= totalParam = []
                                    | otherwise = ["gobble spaces"]
        consumeSpaces Don'tConsumeSpace = ["don't gobble spaces"]
        consumeSpaces WarnIfConsumedSpaces = ["gobble spaces (with a warning)"]
        defaultValue xs | xs == noValueMarker = []
                        | otherwise = ["default={" <> mconcat (map showToken xs) <> "}"]
        withDescription :: [MessageString] -> MessageString
        withDescription [] = "#" <> fromString (show i)
        withDescription desc = "<" <> mconcat (intersperse ", " desc) <> ">#" <> fromString (show i)
        withDescriptionD :: TeXToken -> TeXToken -> [MessageString] -> MessageString
        withDescriptionD t1 t2 [] = showToken t1 <> "#" <> fromString (show i) <> showToken t2
        withDescriptionD t1 t2 desc = showToken t1 <> "<" <> mconcat (intersperse ", " desc) <> ">#" <> fromString (show i) <> showToken t2

paramSpecIsLongM :: MacroParamSpec -> Maybe ParamLong
paramSpecIsLongM (StandardMandatory { paramSpecIsLong = long }) = Just long
paramSpecIsLongM (RequiredDelimited { paramSpecIsLong = long }) = Just long
paramSpecIsLongM (Until { paramSpecIsLong = long }) = Just long
paramSpecIsLongM (Verbatim {}) = Nothing
paramSpecIsLongM (OptionalDelimited { paramSpecIsLong = long }) = Just long
paramSpecIsLongM (OptionalChar {}) = Nothing
paramSpecIsLongM (OptionalGroup { paramSpecIsLong = long }) = Just long

instance Meaning Macro where
  meaningString m = prefix <> "macro:" <> beforeFirstParam <> paramSpec <> "->" <> repText
    where
      prefixP | macroIsProtected m = controlSequence "protected"
              | otherwise = ""
      prefixO | macroIsOuter m = controlSequence "outer"
              | otherwise = ""
      allLong = all (\p -> paramSpecIsLongM p /= Just ShortParam) (macroParamSpec m)
      allShort = all (\p -> paramSpecIsLongM p /= Just LongParam) (macroParamSpec m)
      prefixL | allShort = ""
              | allLong = controlSequence "long"
              | otherwise = "(partially)" <> controlSequence "long"
      hasPrefix = macroIsProtected m || macroIsOuter m || not allShort
      prefix = prefixP <> prefixL <> prefixO <> (if hasPrefix then " " else "")
      paramCount = length (macroParamSpec m) -- should be <= 9
      beforeFirstParam = showParamDelimiter (macroDelimiterBeforeFirstParam m)
      paramSpec = mconcat $ zipWith (showParamSpec (if allLong then LongParam else ShortParam) paramCount) [1..] (macroParamSpec m)
      repText = mconcat (map showToken (macroReplacement m)) <> if isLastLBrace then "{" else ""
      isLastLBrace | paramCount == 0 = delimitedByLBrace $ macroDelimiterBeforeFirstParam m
                   | otherwise = case last (macroParamSpec m) of
                                   Until _ d -> delimitedByLBrace d
                                   _ -> False

instance IsExpandable Macro where
  isConditional _ = False

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExpand Macro m where
  doExpand m = map toEToken <$> doMacro m
  doTotallyExpand t m | macroIsProtected m = Just (return [t])
                      | otherwise = Nothing
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
                          , macroDelimiterBeforeFirstParam = ParamDelimiter [] False
                          , macroParamSpec = replicate numOfArgs $ StandardMandatory long
                          , macroReplacement = repText
                          }
               Just defaultArg
                 | numOfArgs == 0 -> throwError "Unexpected default argument"
                 | otherwise -> return $ Macro
                                { macroIsOuter = False
                                , macroIsProtected = False
                                , macroDelimiterBeforeFirstParam = ParamDelimiter [] False
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

data ParameterTextToken = PTTNumbered -- #1..#9
                        | PTTDelimiter [TeXToken] !Bool
                        deriving (Show)

-- read <parameter text> and the left brace
readParameterText :: (MonadTeXState s m, MonadError String m) => ParamLong -> m (ParamDelimiter,[MacroParamSpec])
readParameterText !long = do
  pt <- readParameterTextToken 1
  return $ case pt of
             PTTDelimiter ts lbrace : xs -> (ParamDelimiter ts lbrace, parseRest xs)
             xs -> (ParamDelimiter [] False, parseRest xs)
  where
    readParameterTextToken !i = do
      t <- required nextEToken -- without expansion
      case t of
        ETCharacter { etCatCode = CCBeginGroup } -> return []
        ETCharacter { etCatCode = CCEndGroup } -> throwError "Unexpected `}' while reading parameter text"
        ETCharacter { etCatCode = CCParam } -> do
          t <- required nextEToken
          case t of
            -- #1 .. #9
            ETCharacter { etChar = c, etCatCode = CCOther }
              | isDigit c && i == digitToInt c -> (PTTNumbered :) <$> readParameterTextToken (i + 1)
            -- #{
            ETCharacter { etCatCode = CCBeginGroup } -> return [PTTDelimiter [] True]
            _ -> throwError "Parameters must be numbered consecutively"
        -- other token: delimiter
        t -> do xs <- readParameterTextToken i
                return $ case xs of
                           PTTDelimiter ts lbrace : xss -> PTTDelimiter (fromEToken t : ts) lbrace : xss
                           _ -> PTTDelimiter [fromEToken t] False : xs
    parseRest [] = []
    parseRest (PTTNumbered : PTTDelimiter ts lbrace : xs) = Until long (ParamDelimiter ts lbrace) : parseRest xs
    parseRest (PTTNumbered : xs) = StandardMandatory long : parseRest xs
    parseRest xs = error $ "internal error: readParameterText " ++ show xs

defCommand :: (Elem Macro (ExpandableSet s), MonadTeXState s m, MonadError String m) => String -> MacroDefPrefix -> m ()
defCommand _defcmdname !prefix = do
  name <- readCommandName
  (pd,ps) <- readParameterText (prefixLong prefix)
  repText <- readUntilEndGroup LongParam
  let macro = Macro { macroIsOuter = prefixOuter prefix
                    , macroIsProtected = prefixProtected prefix
                    , macroDelimiterBeforeFirstParam = pd
                    , macroParamSpec = ps
                    , macroReplacement = repText
                    }
  if prefixGlobal prefix
    then assign (localStates . mapped . definitionAt name) (Left $ liftUnion macro)
    else assign (localState . definitionAt name) (Left $ liftUnion macro)

edefCommand :: (Elem Macro (ExpandableSet s), MonadTeXState s m, MonadError String m) => String -> MacroDefPrefix -> m ()
edefCommand _defcmdname !prefix = do
  name <- readCommandName
  (pd,ps) <- readParameterText (prefixLong prefix)
  repText <- edefReadUntilEndGroup
  let macro = Macro { macroIsOuter = prefixOuter prefix
                    , macroIsProtected = prefixProtected prefix
                    , macroDelimiterBeforeFirstParam = pd
                    , macroParamSpec = ps
                    , macroReplacement = repText
                    }
  if prefixGlobal prefix
    then assign (localStates . mapped . definitionAt name) (Left $ liftUnion macro)
    else assign (localState . definitionAt name) (Left $ liftUnion macro)

doPrefix :: (Elem Macro (ExpandableSet s), MonadTeXState s m, MonadError String m, Meaning (Value s)) => String -> MacroDefPrefix -> m ()
doPrefix name !prefix = do
  (et,v) <- evalToken
  case toCommonValue v of
    Just Relax -> doPrefix name prefix -- ignore \relax
    Just (Unexpanded {}) -> doPrefix name prefix -- ignore \relax
    Just (Character _ CCSpace) -> doPrefix name prefix -- ignore spaces
    _ -> (onMacroCommand @> (\_ -> invalidPrefix name v)) v
  where
    onMacroCommand Mdef = defCommand "def" prefix
    onMacroCommand Medef = edefCommand "edef" prefix
    onMacroCommand Mgdef = defCommand "gdef" prefix { prefixGlobal = True }
    onMacroCommand Mxdef = edefCommand "xdef" prefix { prefixGlobal = True }
    onMacroCommand Mouter = doPrefix "outer" prefix { prefixOuter = True }
    onMacroCommand Mlong = doPrefix "long" prefix { prefixLong = LongParam }
    onMacroCommand Mprotected = doPrefix "protected" prefix { prefixProtected = True }
    onMacroCommand v = invalidPrefix name v

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

instance Meaning MacroCommand where
  meaningString Mdef = controlSequence "def"
  meaningString Medef = controlSequence "edef"
  meaningString Mgdef = controlSequence "gdef"
  meaningString Mxdef = controlSequence "xdef"
  meaningString Mouter = controlSequence "outer"
  meaningString Mlong = controlSequence "long"
  meaningString Mprotected = controlSequence "protected"
  meaningString Mnewcommand = controlSequence "newcommand"
  meaningString Mrenewcommand = controlSequence "renewcommand"
  meaningString Mprovidecommand = controlSequence "providecommand"

instance (Elem Macro (ExpandableSet s), MonadTeXState s m, MonadError String m, Monad m, Meaning (Value s)) => DoExecute MacroCommand m where
  doExecute Mdef       = defCommand "def" unprefixed
  doExecute Medef      = edefCommand "edef" unprefixed
  doExecute Mgdef      = defCommand "gdef" globalPrefix
  doExecute Mxdef      = edefCommand "xdef" globalPrefix
  doExecute Mouter     = doPrefix "outer" (unprefixed { prefixOuter = True })
  doExecute Mlong      = doPrefix "long" (unprefixed { prefixLong = LongParam })
  doExecute Mprotected = doPrefix "protected" (unprefixed { prefixProtected = True })
  doExecute Mnewcommand     = newcommandCommand
  doExecute Mrenewcommand   = renewcommandCommand
  doExecute Mprovidecommand = providecommandCommand
  doGlobal Mdef       = Just $ defCommand "def" globalPrefix
  doGlobal Medef      = Just $ edefCommand "edef" globalPrefix
  doGlobal Mgdef      = Just $ defCommand "gdef" globalPrefix
  doGlobal Mxdef      = Just $ edefCommand "xdef" globalPrefix
  doGlobal Mouter     = Just $ doPrefix "outer" (globalPrefix { prefixOuter = True })
  doGlobal Mlong      = Just $ doPrefix "long" (globalPrefix { prefixLong = LongParam })
  doGlobal Mprotected = Just $ doPrefix "protected" (globalPrefix { prefixProtected = True })
  doGlobal _          = Nothing
  getQuantity _ = NotQuantity

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
