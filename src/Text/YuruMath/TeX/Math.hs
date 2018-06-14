{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Text.YuruMath.TeX.Math where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Quantity
import Text.YuruMath.TeX.Tokenizer
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Expansion
import Text.YuruMath.TeX.Execution
import Text.YuruMath.TeX.Typeset
import Text.YuruMath.TeX.Math.List
import Text.YuruMath.TeX.Math.State
import qualified Data.Map.Strict as Map
import Data.Word
import Data.Bits
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Data.Monoid ((<>),mempty,Any(..),First(..))
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Identity
import Control.Lens.Lens (Lens')
import Control.Lens.Getter (use,uses)
import Control.Lens.Setter (set,assign,modifying)
import Control.Lens.Tuple (_1,_2,_3,_4,_5)
import Data.OpenUnion
import TypeFun.Data.List (SubList,Delete,(:++:))
import Data.Proxy

data MathToken m where
  MTChar       :: !Char -> MathToken m -- letter, other, \char or \chardef-ed
  MTMathChar   :: !MathCode -> MathToken m -- \mathchar or \mathchardef-ed
  MTDelimiter  :: !MathClass -> !DelimiterCode -> MathToken m -- \delimiter
  MTLBrace     :: MathToken m
  MTRBrace     :: MathToken m
  MTAtomSpec   :: !AtomType -> MathToken m -- \mathord, ..., \overline
  MTSup        :: MathToken m
  MTSub        :: MathToken m
  MTRadical    :: !DelimiterCode -> MathToken m
  MTRoot       :: !DelimiterCode -> MathToken m
  MTAccent     :: !MathCode -> MathToken m
  MTLimitsSpec :: !LimitsSpec -> MathToken m
  MTSetStyle   :: !MathStyle -> MathToken m -- \displaystyle, \textstyle, etc
  MTLeft       :: !DelimiterOptions -> !DelimiterCode -> MathToken m
  MTMiddle     :: !DelimiterOptions -> !DelimiterCode -> MathToken m
  MTRight      :: !DelimiterOptions -> !DelimiterCode -> MathToken m
  MTGenFrac    :: !GenFrac -> MathToken m -- \over, \atop, \above<dimen>, ..withdelims<delim><delim>
  MTUstack     :: MathToken m -- \Ustack
  MTChoice     :: MathToken m -- \mathchoice
  MTStopInline :: MathToken m -- `$' or \Ustopmath
  MTStopDisplay :: MathToken m -- `$$' or \Ustopdisplaymath
  MTSetVariant :: !MathVariant -> MathToken m
  MTSetSymbolMode :: !SymbolMode -> MathToken m
  MTGlue       :: !MathGlue -> MathToken m
  MTKern       :: !MathKern -> MathToken m
  MTUnKern     :: MathToken m
  MTUnSkip     :: MathToken m
  MTOther      :: (DoExecute v m, Show v) => v -> MathToken m

deriving instance Show (MathToken m)

type MonadMathState state set m
  = ( MonadTeXState state m
    , IsMathState state
    , Value state ~ Union set
    , DoExecute (Union (Delete TypesetCommand (Delete MathSymbolModeSet (Delete MathVariantSet (Delete MathCommands (Delete MathAtomCommand (Delete MathStyleSet (Delete CommonValue set)))))))) m
    , Show (Union (Delete TypesetCommand (Delete MathSymbolModeSet (Delete MathVariantSet (Delete MathCommands (Delete MathAtomCommand (Delete MathStyleSet (Delete CommonValue set))))))))
    )

readMathToken :: forall m state set. (MonadMathState state set m, MonadError String m) => m (Maybe (MathToken m))
readMathToken = do
  v <- evalToValue
  case v of
    Nothing -> return Nothing -- end of input
    Just v -> doMathToken v
  where
    doMathToken :: Union set -> m (Maybe (MathToken m))
    doMathToken = doCommonValue
                  @> (\(v :: MathStyleSet)    -> Just <$> doMathStyleSet v)
                  @> (\(v :: MathAtomCommand) -> Just <$> doMathAtom v)
                  @> (\(v :: MathCommands)    -> Just <$> doOtherMathCommand v)
                  @> (\(v :: MathVariantSet)  -> Just <$> doMathVariantSet v)
                  @> (\(v :: MathSymbolModeSet) -> Just <$> doMathSymbolModeSet v)
                  @> (\(v :: TypesetCommand)  -> Just <$> doTypesetCommand v)
                  @> (\v                      -> return $ Just $ MTOther v) -- other assignments, etc
    doCommonValue :: CommonValue -> m (Maybe (MathToken m))
    doCommonValue v = case v of
      Character c CCBeginGroup   -> return $ Just MTLBrace
      Character c CCEndGroup     -> return $ Just MTRBrace
      Character _ CCMathShift -> do
        m <- use mode
        if m == DisplayMathMode
          then do et <- nextEToken
                  case et of
                    Just (ETCharacter _ CCMathShift) -> return $ Just MTStopDisplay
                    _ -> throwError "Display math should end with $$."
          else return $ Just MTStopInline
      Character c CCAlignmentTab -> throwError "alignment tab: not implemented yet"
      Character c CCSup          -> return $ Just MTSup
      Character c CCSub          -> return $ Just MTSub
      Character c CCSpace        -> readMathToken -- do nothing
      Character c CCLetter       -> return $ Just $ MTChar c -- TODO: process 'math active'
      Character c CCOther        -> return $ Just $ MTChar c -- TODO: process 'math active'
      Character _ cc             -> throwError $ "Unexpected " ++ show cc ++ " character" -- endline, param, ignored, active, comment, invalid
      DefinedCharacter c         -> return $ Just $ MTChar c
      DefinedMathCharacter c     -> return $ Just $ MTMathChar c
      v -> doExecute v >> readMathToken -- \relax, etc
    doMathStyleSet (MathStyleSet style) = return $ MTSetStyle style
    doMathAtom (MathAtomCommand atomType) = return $ MTAtomSpec atomType
    doMathVariantSet (MathVariantSet var) = return $ MTSetVariant var
    doMathSymbolModeSet (MathSymbolModeSet mode) = return $ MTSetSymbolMode mode
    doTypesetCommand v = case v of
      -- \char<0-"10FFFF>
      Tchar         -> MTChar <$> readUnicodeScalarValue

      -- \kern<dimen>
      Tkern         -> (MTKern . MKKern) <$> readDimension

      -- \unkern, \unskip
      Tunkern       -> return MTUnKern
      Tunskip       -> return MTUnSkip

      -- \hskip<glue>, \hfil, \hfill, \hss, \hfilneg
      Thskip        -> (MTGlue . MGHSkip) <$> readGlue
      Thfil         -> return $ MTGlue $ MGHSkip $ Glue { glueSpace = zeroQ, glueStretch = InfiniteSS 65536 0, glueShrink = zeroQ }
      Thfill        -> return $ MTGlue $ MGHSkip $ Glue { glueSpace = zeroQ, glueStretch = InfiniteSS 65536 1, glueShrink = zeroQ }
      Thss          -> return $ MTGlue $ MGHSkip $ Glue { glueSpace = zeroQ, glueStretch = InfiniteSS 65536 0, glueShrink = InfiniteSS 1 0 }
      Thfilneg      -> return $ MTGlue $ MGHSkip $ Glue { glueSpace = zeroQ, glueStretch = InfiniteSS (-65536) 0, glueShrink = zeroQ }

      -- \noindent: has no effect in math modes
      Tnoindent     -> return $ MTOther Relax

      -- <control space>
      TControlSpace -> return $ MTGlue $ MGHSkip $ Glue { glueSpace = pt 10, glueStretch = zeroQ, glueShrink = zeroQ } -- ?

      -- \/
      TItalicCorrection -> return $ MTKern $ MKKern zeroQ

      Tspecial      -> throwError "\\special: not implemented yet"
      Tpenalty      -> throwError "\\penalty: not implemented yet"
      Tunpenalty    -> throwError "\\unpenalty: not implemented yet"
      Tmark         -> throwError "\\mark: not implemented yet"
      Tinsert       -> throwError "\\insert: not implemented yet"
      Tvadjust      -> throwError "\\vadjust: not implemented yet"
      Thalign       -> throwError "\\halign: not implemented yet"
      Tindent       -> throwError "\\indent: not implemented yet"
      Tvrule        -> throwError "\\vrule: not implemented yet"
      Traise        -> throwError "\\raise: not implemented yet"
      Tlower        -> throwError "\\lower: not implemented yet"
      Tdiscretionary       -> throwError "\\discretionary: not implemented yet"
      TDiscretionaryHyphen -> throwError "\\-: not implemented yet"
    doOtherMathCommand :: MathCommands -> m (MathToken m)
    doOtherMathCommand v = case v of

      -- \mathchar<15-bit integer>
      Mmathchar -> do
        (MTMathChar . MathCode . fromIntegral) <$> readIntBetween 0 0x7FFF

      -- \Umathchar<0-7><0-"FF><0-"10FFFF>
      MUmathchar -> do
        mathclass <- readIntBetween 0 7
        fam <- readIntBetween 0 0xFF
        c <- readUnicodeScalarValue
        return $ MTMathChar (mkUMathCode (toEnum mathclass) (fromIntegral fam) c)

      -- \Umathcharnum<signed 32-bit integer>
      MUmathcharnum -> do
        x <- readInt32
        let slot = 0x1FFFFF .&. (fromIntegral x :: Word32)
        if isUnicodeScalarValue slot
          then return $ MTMathChar (UMathCode x)
          else throwError "\\Umathcharnum: Invalid math code"

      -- \delimiter<27-bit integer>
      Mdelimiter -> do
        x <- readIntBetween 0 (2^(27::Int)-1)
        let mathclass = toEnum (x `shiftR` 24) :: MathClass
            value = fromIntegral (0xFFFFFF .&. x)
        return $ MTDelimiter mathclass (DelimiterCode value)

      -- \Udelimiter<0-7><0-"FF"><0-"10FFFF>
      MUdelimiter -> do
        mathclass <- toEnum <$> readIntBetween 0 7
        fam <- fromIntegral <$> readIntBetween 0 0xFF
        slot <- readUnicodeScalarValue
        return $ MTDelimiter mathclass $ mkUDelCode fam slot

      -- \radical<24-bit integer><math field>
      Mradical -> do
        x <- readIntBetween 0 (2^(24::Int)-1)
        -- x = <12-bit: small variant><12-bit: large variant>
        return $ MTRadical $ DelimiterCode $ fromIntegral x

      -- \Uradical<0-"FF><0-"10FFFF><math field>
      MUradical -> do
        fam <- readIntBetween 0 0xFF
        slot <- readUnicodeScalarValue
        return $ MTRadical $ mkUDelCode (fromIntegral fam) slot

      -- \Uroot<0-"FF><0-"10FFFF><math field><math field>
      MUroot -> do
        fam <- readIntBetween 0 0xFF
        slot <- readUnicodeScalarValue
        return $ MTRoot $ mkUDelCode (fromIntegral fam) slot

      -- \mathaccent<15-bit integer><math field>
      Mmathaccent -> do
        x <- readIntBetween 0 0x7FFF
        let value = fromIntegral $ x .&. 0x0FFF
        return $ MTAccent $ MathCode value

      -- \Umathaccent<0-7><0-"FF"><0-"10FFFF><math field>
      MUmathaccent -> do
        -- TODO: handle optional keywords
        mathclass <- readIntBetween 0 7
        fam <- readIntBetween 0 0xFF
        slot <- readUnicodeScalarValue
        return $ MTAccent $ mkUMathCode (toEnum mathclass) (fromIntegral fam) slot

      MUsuperscript -> return MTSup
      MUsubscript -> return MTSub

      -- \left<delim>, \middle<delim>, \right<delim>
      Mleft   -> MTLeft   defaultDelimiterOptions <$> readDelimiter
      Mmiddle -> MTMiddle defaultDelimiterOptions <$> readDelimiter
      Mright  -> MTRight  defaultDelimiterOptions <$> readDelimiter
      -- \Uleft<Uleft-like options><delim>, \Umiddle<Uleft-like options><delim>, \Uright<Uleft-like options><delim>
      MUleft           -> MTLeft   <$> readDelimiterOptions <*> readDelimiter
      MUmiddle         -> MTMiddle <$> readDelimiterOptions <*> readDelimiter
      MUright          -> MTRight  <$> readDelimiterOptions <*> readDelimiter

      -- \over, \atop
      Mover    -> return $ MTGenFrac $ WithoutDelims GFOver
      Matop    -> return $ MTGenFrac $ WithoutDelims GFAtop

      -- \above<optional "exact"><dimen>
      Mabove   -> do
        _exact <- readKeyword "exact" -- LuaTeX extension
        thickness <- readDimension
        return $ MTGenFrac (WithoutDelims (GFAbove thickness))

      -- \Uskewed<delim><options>
      MUskewed -> do
        slash <- readDelimiter
        (Any _exact,Any _noaxis) <- readKeywordArguments [("exact",pure (Any True,mempty)),("noaxis",pure (mempty,Any True))]
        -- TODO: Handle keywords ("exact", "noaxis")
        return $ MTGenFrac (WithoutDelims (GFSkewed slash))

      -- \overwithdelims<delim><delim>
      Moverwithdelims -> do
        leftDelim <- readDelimiter
        rightDelim <- readDelimiter
        return $ MTGenFrac (WithDelims leftDelim rightDelim GFOver)

      -- \atopwithdelims<delim><delim>
      Matopwithdelims -> do
        leftDelim <- readDelimiter
        rightDelim <- readDelimiter
        return $ MTGenFrac (WithDelims leftDelim rightDelim GFAtop)

      -- \abovewithdelims<left delim><right delim><optional "exact"><dimen>
      Mabovewithdelims -> do
        leftDelim <- readDelimiter
        rightDelim <- readDelimiter
        _exact <- readKeyword "exact" -- LuaTeX extension
        thickness <- readDimension
        return $ MTGenFrac (WithDelims leftDelim rightDelim (GFAbove thickness))

      -- \Uskewedwithdelims<slash delim><left delim><right delim><options>
      MUskewedwithdelims -> do
        slash <- readDelimiter
        leftDelim <- readDelimiter
        rightDelim <- readDelimiter
        (Any _exact,Any _noaxis) <- readKeywordArguments [("exact",pure (Any True,mempty)),("noaxis",pure (mempty,Any True))]
        -- TODO: Handle keywords ("noaxis", "exact")
        return $ MTGenFrac (WithDelims leftDelim rightDelim (GFSkewed slash))

      MUstack -> return MTUstack

      Mlimits        -> return $ MTLimitsSpec Limits
      Mnolimits      -> return $ MTLimitsSpec NoLimits
      Mdisplaylimits -> return $ MTLimitsSpec DisplayLimits

      -- \mathchoice<general text><general text><general text><general text>
      Mmathchoice -> return MTChoice

      MUstopmath -> return MTStopInline
      MUstopdisplaymath -> return MTStopDisplay

      -- \nonscript
      Mnonscript       -> return $ MTGlue MGNonscript

      -- \mkern<mudimen>
      Mmkern           -> (MTKern . MKMKern) <$> readMuDimension

      -- \mskip<muglue>
      Mmskip           -> (MTGlue . MGMSkip) <$> readMuGlue

      -- assignments
      Mfam         -> return $ MTOther Mfam
      Mthinmuskip  -> return $ MTOther Mthinmuskip
      Mmedmuskip   -> return $ MTOther Mmedmuskip
      Mthickmuskip -> return $ MTOther Mthickmuskip

      MUoverdelimiter  -> throwError "\\Uoverdelimiter: not implemented yet"
      MUunderdelimiter -> throwError "\\Uunderdelimiter: not implemented yet"
      MUdelimiterover  -> throwError "\\Udelimiterover: not implemented yet"
      MUdelimiterunder -> throwError "\\Udelimiterunder: not implemented yet"
      MUhextensible    -> throwError "\\Uhextensible: not implemented yet"
      MUnosuperscript  -> throwError "\\Unosuperscript: not implemented yet"
      MUnosubscript    -> throwError "\\Unosubscript: not implemented yet"

readDelimiter :: (MonadMathState state set m, MonadError String m) => m DelimiterCode
readDelimiter = do
  t <- readMathToken
  case t of
    Nothing -> throwError "Missing delimiter: got end of input"
    Just (MTChar c) -> do
      delimiterCodeOf c
    Just (MTDelimiter _ code) -> do
      return code
    Just t -> throwError $ "Missing delimiter: got " ++ show t

-- <Uleft-like option> ::= "height"<dimen> | "depth"<dimen> | "exact" | "axis" | "noaxis" | "class"<number>
readDelimiterOptions :: (MonadTeXState s m, MonadError String m) => m DelimiterOptions
readDelimiterOptions = do
  let emptyOptions = mempty :: (First Dimen,First Dimen, Any, First Bool, First MathClass)
  (First mheight,First mdepth,Any exact,First maxis,First mmathclass) <- readKeywordArguments
    [("height"
     ,do height <- readDimension
         pure (set _1 (First (Just height)) emptyOptions)
     )
    ,("depth"
     ,do depth <- readDimension
         pure (set _2 (First (Just depth)) emptyOptions)
     )
    ,("exact", pure (set _3 (Any True) emptyOptions))
    ,("axis",  pure (set _4 (First (Just True)) emptyOptions))
    ,("noaxis",pure (set _4 (First (Just False)) emptyOptions))
    ,("class"
     ,do mathclass <- toEnum <$> readIntBetween 0 7
         pure (set _5 (First (Just mathclass)) emptyOptions)
     )
    ]
  return $ DelimiterOptions mheight mdepth exact maxis mmathclass

withMathStyle :: (MonadTeXState state m, IsMathState state) => (MathStyle -> MathStyle) -> m a -> m a
withMathStyle f m = do
  oldStyle <- use currentMathStyle
  assign currentMathStyle (f oldStyle)
  x <- m
  assign currentMathStyle oldStyle
  return x

class (Functor f) => MathMaterialEnding f where
  onEndOfInput      :: MonadError String m => m MathList -> m (f MathList)
  onRightBrace      :: MonadError String m => m MathList -> m (f MathList)
  onMiddleDelim     :: MonadError String m => DelimiterOptions -> DelimiterCode -> m MathList -> m (f MathList)
  onRightDelim      :: MonadError String m => DelimiterOptions -> DelimiterCode -> m MathList -> m (f MathList)
  onStopInlineMath  :: MonadError String m => m MathList -> m (f MathList)
  onStopDisplayMath :: MonadError String m => m MathList -> m (f MathList)
  onGenFrac         :: MonadError String m => GenFrac -> m MathList -> m (f MathList)
  expectedEnding    :: Proxy f -> String
  onEndOfInput _      = throwError $ "Unexpected end of input: expected " ++ expectedEnding (Proxy :: Proxy f)
  onRightBrace _      = throwError $ "Unexpected `}': expected " ++ expectedEnding (Proxy :: Proxy f)
  onMiddleDelim _ _ _ = throwError $ "Unexpected \\middle: expected " ++ expectedEnding (Proxy :: Proxy f)
  onRightDelim _ _ _  = throwError $ "Unexpected \\right: expected " ++ expectedEnding (Proxy :: Proxy f)
  onStopInlineMath _  = throwError $ "Unexpected `$': expected " ++ expectedEnding (Proxy :: Proxy f)
  onStopDisplayMath _ = throwError $ "Unexpected `$$': expected " ++ expectedEnding (Proxy :: Proxy f)
  onGenFrac _ _       = throwError "Fraction must be preceded by \\Ustack"

newtype MMDGlobal a = MMDGlobal { runMMDGlobal :: a } deriving (Functor)
instance MathMaterialEnding MMDGlobal where
  onEndOfInput m = MMDGlobal <$> m
  expectedEnding _ = "end of input"

newtype MMDBrace a = MMDBrace { runMMDBrace :: a } deriving (Functor)
instance MathMaterialEnding MMDBrace where
  onRightBrace m = MMDBrace <$> m
  expectedEnding _ = "`}'"

data MMDLeftRight a = MMDRight !DelimiterOptions !DelimiterCode a
                    | MMDMiddle !DelimiterOptions !DelimiterCode a
                    deriving (Functor)
instance MathMaterialEnding MMDLeftRight where
  onMiddleDelim opt delim m = MMDMiddle opt delim <$> m
  onRightDelim opt delim m = MMDRight opt delim <$> m
  expectedEnding _ = "\\right"

newtype MMDInline a = MMDInline { runInlineMath :: a } deriving (Functor)
instance MathMaterialEnding MMDInline where
  onStopInlineMath m = MMDInline <$> m
  expectedEnding _ = "`$'"

newtype MMDDisplay a = MMDDisplay { runDisplayMath :: a } deriving (Functor)
instance MathMaterialEnding MMDDisplay where
  onStopDisplayMath m = MMDDisplay <$> m
  expectedEnding _ = "`$$'"

data MMDNumerator a = MMDNumerator !GenFrac a deriving (Functor)
instance MathMaterialEnding MMDNumerator where
  onGenFrac gf numerator = MMDNumerator gf <$> numerator
  onRightBrace _ = throwError "No fraction after \\Ustack"
  expectedEnding _ = "a generalized fraction command (like \\over)"

newtype MMDDenominator a = MMDDenominator { runMMDDenominator :: a } deriving (Functor)
instance MathMaterialEnding MMDDenominator where
  onRightBrace m = MMDDenominator <$> m
  onGenFrac _ _ = throwError "Ambiguous: you need another { and }"
  expectedEnding _ = "`}'"

readMathMaterial :: forall f state set m. (MathMaterialEnding f, MonadMathState state set m, MonadError String m) => m (f MathList)
readMathMaterial = loop []
  where
    loop :: MathList -> m (f MathList)
    loop revList = do
      t <- readMathToken
      let doAtom atom = loop (IAtom atom : revList)
          modifyLastAtom f = case revList of
            IAtom atom : revList' -> do
              atom' <- f atom
              loop (IAtom atom' : revList')
            _ -> do
              atom <- f emptyAtom
              loop (IAtom atom : revList)
      case t of
        Nothing -> onEndOfInput (return (reverse revList))
        Just t -> case t of
          -- <character>
          MTChar c -> do
            mc <- mathCodeOf c
            delcode <- delimiterCodeOf c
            let mathclass = mathcharClass mc
                atomType = mathclassToAtomType mathclass
            sym <- makeMathSymbol mathclass (mathcharFamily mc) (mathcharSlot mc)
            let atom = mkAtom atomType sym
            doAtom $ if delcode /= DelimiterCode (-1)
                     then markAtomAsDelimiter atom
                     else atom

          -- <math symbol>
          MTMathChar mc -> do -- \mathchar or \mathchardef-ed
            let mathclass = mathcharClass mc
                atomType = mathclassToAtomType mathclass
            sym <- makeMathSymbol mathclass (mathcharFamily mc) (mathcharSlot mc)
            doAtom (mkAtom atomType sym)

          -- <math symbol>
          MTDelimiter mathclass del -> do -- \delimiter
            sym <- makeMathSymbol mathclass (delimiterFamilySmall del) (delimiterSlotSmall del)
            let atomType = mathclassToAtomType mathclass
            doAtom (markAtomAsDelimiter (mkAtom atomType sym))

          -- { <math mode material> }
          MTLBrace -> do
            enterGroup ScopeByBrace
            content <- runMMDBrace <$> withMathStyle id readMathMaterial
            case content of
              [item@(IAtom (AccAtom {}))] -> loop (item : revList) -- single Acc atom
              _ -> doAtom (mkAtom AOrd (MFSubList content))

          -- `}'
          MTRBrace -> onRightBrace $ do
            leaveGroup ScopeByBrace
            return (reverse revList)

          -- <math atom><math field>
          MTAtomSpec atomType -> do
            x <- if atomType == AOver
                 then withMathStyle makeCramped readMathField
                 else withMathStyle id readMathField
            doAtom (mkAtom atomType x)

          -- <superscript><math field>
          MTSup -> do
            x <- withMathStyle superscriptStyle readMathField
            modifyLastAtom $ \atom ->
              if atomSuperscript atom == MFEmpty
              then return (atom { atomSuperscript = x })
              else throwError "Double superscript"

          -- <subscript><math field>
          MTSub -> do
            x <- withMathStyle subscriptStyle readMathField
            modifyLastAtom $ \atom ->
              if atomSubscript atom == MFEmpty
              then return (atom { atomSubscript = x })
              else throwError "Double subscript"

          -- \mathaccent<15-bit number><math field> or \Umathaccent<...><math field>
          MTAccent code -> do
            content <- withMathStyle makeCramped readMathField
            doAtom (mkAtom AAcc content)

          -- \radical<27-bit number><math field> or \Uradical<...><math field>
          MTRadical code -> do
            content <- withMathStyle makeCramped readMathField
            doAtom (mkAtom ARad content)

          -- \Uroot<...><math field><math field>
          MTRoot code -> do
            degree <- withMathStyle (const ScriptScriptStyle) readMathField
            content <- withMathStyle makeCramped readMathField
            doAtom (mkAtom ARad content)

          -- \limits, \nolimits, \displaylimits
          MTLimitsSpec spec -> do
            case revList of
              IAtom (atom@(OpAtom {})) : xs -> loop (IAtom (atom { atomLimits = spec }) : xs)
              _ -> throwError "Limit controls must follow a math operator."

          -- \displaystyle, \textstyle, etc
          MTSetStyle newStyle -> do
            assign currentMathStyle newStyle
            loop (IStyleChange newStyle : revList)

          -- \left<delim> <math mode material> (\middle<delim><math mode material>)* \right<delim>
          -- or \Uleft<Uleft-like options><delim> ...
          MTLeft leftOptions leftDelim -> do
            let readUntilRight revContentList = do
                  enterGroup ScopeByLeftRight
                  result <- withMathStyle id readMathMaterial -- :: m (MMDLeftRight _)
                  case result of
                    MMDMiddle middleOptions delim content -> do
                      readUntilRight ([IBoundary BoundaryMiddle middleOptions delim] : content : revContentList)
                    MMDRight rightOptions delim content -> do
                      let content' = concat $ reverse $ [IBoundary BoundaryRight rightOptions delim] : content : revContentList
                      loop (IAtom (mkAtom AInner (MFSubList content')) : revList)
            readUntilRight [[IBoundary BoundaryLeft leftOptions leftDelim]]

          -- \middle<delim> or \Umiddle<Uleft-like options><delim>
          MTMiddle options delim -> onMiddleDelim options delim $ do
            leaveGroup ScopeByLeftRight
            return (reverse revList)

          -- \right<delim> or \Uright<Uleft-like options><delim>
          MTRight options delim -> onRightDelim options delim $ do
            leaveGroup ScopeByLeftRight
            return (reverse revList)

          -- <generalized fraction command>
          MTGenFrac gf -> onGenFrac gf $ do
            -- do not leave a scope
            return (reverse revList)

          -- \Ustack { <math mode material> <generalized fraction command> <math mode material> }
          MTUstack -> do
            readLBrace
            MMDNumerator gf numerator <- withMathStyle smallerStyle readMathMaterial
            denominator <- runMMDDenominator <$> withMathStyle denominatorStyle readMathMaterial
            doAtom (mkAtom AOrd (MFSubList [IGenFrac gf numerator denominator]))

          -- \mathchoice<general text><general text><general text><general text>
          MTChoice -> do
            currentStyle <- use currentMathStyle
            let -- doChoiceBranch :: MathStyle -> m MathList
                doChoiceBranch !s
                  | makeCramped s == makeCramped currentStyle = do
                      -- we are in the 'right' branch
                      readLBrace
                      runMMDBrace <$> readMathMaterial
                  | otherwise = do
                      -- we are not in the 'right' branch
                      readLBrace
                      runMMDBrace <$> withMathStyle (const $ makeCrampedIf (isCramped currentStyle) s) readMathMaterial
            d  <- doChoiceBranch DisplayStyle
            t  <- doChoiceBranch TextStyle
            s  <- doChoiceBranch ScriptStyle
            ss <- doChoiceBranch ScriptScriptStyle
            loop (IChoice d t s ss : revList)

          MTStopInline -> onStopInlineMath $ do
            leaveGroup ScopeByMath
            return (reverse revList)

          MTStopDisplay -> onStopDisplayMath $ do
            leaveGroup ScopeByMath
            return (reverse revList)

          MTSetVariant var -> do
            assign (localState . currentVariant) var
            loop revList

          MTSetSymbolMode sm -> do
            assign (localState . currentSymbolMode) sm
            loop revList

          MTGlue g -> loop (IGlue g : revList)
          MTKern k -> loop (IKern k : revList)

          MTUnSkip ->
            case revList of
              IGlue _ : revList' -> loop revList'
              _ -> loop revList

          MTUnKern ->
            case revList of
              IKern _ : revList' -> loop revList'
              _ -> loop revList

          -- assignments, etc
          MTOther v -> do
            doExecute v
            loop revList

readLBrace :: (MonadMathState state set m, MonadError String m) => m ()
readLBrace = do
  t <- readMathToken
  case t of
    Just MTLBrace -> do
      enterGroup ScopeByBrace
    _ -> throwError "Expected `{'"

-- <math symbol> ::= <character> | <math character>
-- <math field> ::= <math symbol> | {<math mode material>}
readMathField :: (MonadMathState state set m, MonadError String m) => m MathField
readMathField = do
  t <- readMathToken
  case t of
    Nothing -> throwError "Unexpected end of input: expected a math field"
    Just t -> case t of
      MTChar c -> do
        mc <- mathCodeOf c
        makeMathSymbol (mathcharClass mc) (mathcharFamily mc) (mathcharSlot mc)
      MTMathChar mc -> do -- \mathchar or \mathchardef-ed
        makeMathSymbol (mathcharClass mc) (mathcharFamily mc) (mathcharSlot mc)
      MTDelimiter mathclass del -> do -- \delimiter
        makeMathSymbol mathclass (delimiterFamilySmall del) (delimiterSlotSmall del)
      MTLBrace -> do
        enterGroup ScopeByBrace
        content <- runMMDBrace <$> readMathMaterial
        return $ case content of
          [IAtom (OrdAtom { atomNucleus = nucleus, atomSuperscript = MFEmpty, atomSubscript = MFEmpty })] -> nucleus
          _ -> MFSubList content
      _ -> throwError $ "Unexpected " ++ show t ++ "; expected a symbol or `{'"

makeMathSymbol :: (MonadTeXState state m, IsMathState state) => MathClass -> Word8 -> Char -> m MathField
makeMathSymbol MathVar !fam !slot = do
  famP <- use (localState . famParam)
  v <- use (localState . currentVariant)
  sm <- use (localState . currentSymbolMode)
  if 0 <= famP && famP <= 255
    then return $ MFSymbol { symbolFamily = fromIntegral famP
                           , symbolVariant = v
                           , symbolMode = sm
                           , symbolContent = T.singleton slot
                           }
    else return $ MFSymbol { symbolFamily = fromIntegral fam
                           , symbolVariant = v
                           , symbolMode = sm
                           , symbolContent = T.singleton slot
                           }
makeMathSymbol !_mathclass !fam !slot
  = return $ MFSymbol { symbolFamily = fromIntegral fam
                      , symbolVariant = MVNormal
                      , symbolMode = SMSymbol
                      , symbolContent = T.singleton slot
                      }

famSet :: (MonadTeXState state m, MonadError String m, IsMathState state) => m (Assignment state)
famSet = do
  val <- readIntBetween 0 255
  texAssign famParam val

famGet :: (MonadTeXState state m, IsMathState state) => m Integer
famGet = uses (localState . famParam) fromIntegral

muskipParamSet :: (MonadTeXState s m, MonadError String m) => Lens' (LocalState s) (Glue MuDimen) -> m (Assignment s)
muskipParamSet muskip = readMuGlue >>= texAssign muskip

--
-- Setting math style
--

newtype MathStyleSet = MathStyleSet MathStyle
                     deriving (Eq,Show)

instance (Monad m, MonadTeXState state m, MonadError String m, IsMathState state) => DoExecute MathStyleSet m where
  doExecute (MathStyleSet s) = return () -- dummy
  getQuantity (MathStyleSet v) = QInteger $ return $ fromIntegral $ fromEnum v -- LuaTeX extension

--
-- Math atom command (like \mathord)
--

newtype MathAtomCommand = MathAtomCommand AtomType deriving (Eq,Show)

instance (Monad m, MonadTeXState state m, MonadError String m, IsMathState state) => DoExecute MathAtomCommand m where
  doExecute _ = return () -- dummy
  getQuantity _ = NotQuantity

--
-- Setting math variant
--

newtype MathVariantSet = MathVariantSet MathVariant deriving (Eq,Show)

instance (Monad m, MonadTeXState state m, MonadError String m, IsMathState state) => DoExecute MathVariantSet m where
  doExecute _ = return () -- dummy
  getQuantity _ = NotQuantity

--
-- Setting symbol mode
--

newtype MathSymbolModeSet = MathSymbolModeSet SymbolMode deriving (Eq,Show)

instance (Monad m, MonadTeXState state m, MonadError String m, IsMathState state) => DoExecute MathSymbolModeSet m where
  doExecute _ = return () -- dummy
  getQuantity _ = NotQuantity

--
-- Expandable math commands
--

data MathExpandable = Mmathstyle -- LuaTeX extension
                    deriving (Eq,Show)

-- LuaTeX extension: \mathstyle
mathstyleCommand :: (MonadTeXState state m, MonadError String m, IsMathState state) => m [ExpansionToken]
mathstyleCommand = do
  style <- use currentMathStyle
  stringToEToken $ show $ fromEnum style -- 0..7

instance IsExpandable MathExpandable where
  isConditional _ = False

instance (Monad m, MonadTeXState state m, MonadError String m, IsMathState state) => DoExpand MathExpandable m where
  doExpand Mmathstyle = mathstyleCommand
  evalBooleanConditional _ = Nothing

--
-- Other math commands
--

data MathCommands
  = Mmathchar
  | Mmathaccent
  | Mdelimiter
  | Mradical
  | Mdisplaylimits
  | Mlimits
  | Mnolimits
  | Mmathchoice
  | Mleft
  | Mright
  | Mover
  | Matop
  | Mabove
  | Moverwithdelims
  | Matopwithdelims
  | Mabovewithdelims
  | Mfam
  | Mthinmuskip
  | Mmedmuskip
  | Mthickmuskip
  | Mmkern
  | Mmskip
  | Mnonscript

    -- e-TeX extension:
  | Mmiddle

    -- LuaTeX extensions:
  | MUmathchar
  | MUmathaccent
  | MUdelimiter
  | MUradical
  | MUmathcharnum
  | MUroot
  | MUoverdelimiter
  | MUunderdelimiter
  | MUdelimiterover
  | MUdelimiterunder
  | MUhextensible
  | MUskewed
  | MUskewedwithdelims
  | MUstack
  | MUsuperscript
  | MUsubscript
  | MUnosuperscript
  | MUnosubscript
  | MUleft
  | MUmiddle
  | MUright
  | MUstopmath
  | MUstopdisplaymath
  -- \Ustartmath, \Ustartdisplaymath: not really math commands...

  deriving (Eq,Show)

instance (Monad m, MonadTeXState state m, MonadError String m, IsMathState state) => DoExecute MathCommands m where
  doExecute Mfam           = runLocal famSet
  doExecute Mthinmuskip    = runLocal (muskipParamSet thinmuskip)
  doExecute Mmedmuskip     = runLocal (muskipParamSet medmuskip)
  doExecute Mthickmuskip   = runLocal (muskipParamSet thickmuskip)
  doExecute x              = throwError $ "You can't use " ++ show x ++ " in non-math mode"
  doGlobal Mfam            = Just $ runGlobal famSet
  doGlobal Mthinmuskip     = Just $ runGlobal (muskipParamSet thinmuskip)
  doGlobal Mmedmuskip      = Just $ runGlobal (muskipParamSet medmuskip)
  doGlobal Mthickmuskip    = Just $ runGlobal (muskipParamSet thickmuskip)
  doGlobal _               = Nothing
  doAdvance Mfam           = Just $ runArithmetic $ advanceInt famParam
  doAdvance Mthinmuskip    = Just $ runArithmetic $ advanceQuantity thinmuskip
  doAdvance Mmedmuskip     = Just $ runArithmetic $ advanceQuantity medmuskip
  doAdvance Mthickmuskip   = Just $ runArithmetic $ advanceQuantity thickmuskip
  doAdvance _              = Nothing
  doMultiply Mfam          = Just $ runArithmetic $ multiplyInt famParam
  doMultiply Mthinmuskip   = Just $ runArithmetic $ multiplyQuantity thinmuskip
  doMultiply Mmedmuskip    = Just $ runArithmetic $ multiplyQuantity medmuskip
  doMultiply Mthickmuskip  = Just $ runArithmetic $ multiplyQuantity thickmuskip
  doMultiply _             = Nothing
  doDivide Mfam            = Just $ runArithmetic $ divideInt famParam
  doDivide Mthinmuskip     = Just $ runArithmetic $ divideQuantity thinmuskip
  doDivide Mmedmuskip      = Just $ runArithmetic $ divideQuantity medmuskip
  doDivide Mthickmuskip    = Just $ runArithmetic $ divideQuantity thickmuskip
  doDivide _               = Nothing
  getQuantity Mfam         = QInteger famGet
  getQuantity Mthinmuskip  = QMuGlue (use (localState . thinmuskip))
  getQuantity Mmedmuskip   = QMuGlue (use (localState . medmuskip))
  getQuantity Mthickmuskip = QMuGlue (use (localState . thickmuskip))
  getQuantity _            = NotQuantity

--
-- List of commands
--

type MathExpandableList = '[MathExpandable]
type MathNonExpandablePrimitiveList = '[MathCommands,MathAtomCommand,MathStyleSet,MathVariantSet,MathSymbolModeSet]

mathDefinitions :: (SubList MathExpandableList eset, SubList MathNonExpandablePrimitiveList vset) => Map.Map Text (Either (Union eset) (Union vset))
mathDefinitions = Map.fromList
  [("mathstyle", Left $ liftUnion Mmathstyle) -- LuaTeX extension
  ]
  <> fmap Right (Map.fromList
  [("mathchar",     liftUnion Mmathchar)
  ,("delimiter",    liftUnion Mdelimiter)
  ,("mathord",      liftUnion (MathAtomCommand AOrd))
  ,("mathop",       liftUnion (MathAtomCommand AOp))
  ,("mathbin",      liftUnion (MathAtomCommand ABin))
  ,("mathrel",      liftUnion (MathAtomCommand ARel))
  ,("mathopen",     liftUnion (MathAtomCommand AOpen))
  ,("mathclose",    liftUnion (MathAtomCommand AClose))
  ,("mathpunct",    liftUnion (MathAtomCommand APunct))
  ,("mathinner",    liftUnion (MathAtomCommand AInner))
  ,("underline",    liftUnion (MathAtomCommand AUnder))
  ,("overline",     liftUnion (MathAtomCommand AOver))
  ,("mathaccent",   liftUnion Mmathaccent)
  ,("radical",      liftUnion Mradical)
  ,("displaylimits",liftUnion Mdisplaylimits)
  ,("limits",       liftUnion Mlimits)
  ,("nolimits",     liftUnion Mnolimits)
  ,("mathchoice",   liftUnion Mmathchoice)
  ,("left",         liftUnion Mleft)
  ,("right",        liftUnion Mright)
  ,("over",         liftUnion Mover)
  ,("atop",         liftUnion Matop)
  ,("above",        liftUnion Mabove)
  ,("overwithdelims",liftUnion Moverwithdelims)
  ,("atopwithdelims",liftUnion Matopwithdelims)
  ,("abovewithdelims",liftUnion Mabovewithdelims)
  ,("fam",          liftUnion Mfam)
  ,("thinmuskip",   liftUnion Mthinmuskip)
  ,("medmuskip",    liftUnion Mmedmuskip)
  ,("thickmuskip",  liftUnion Mthickmuskip)
  ,("mkern",        liftUnion Mmkern)
  ,("mskip",        liftUnion Mmskip)
  ,("nonscript",    liftUnion Mnonscript)

  -- e-TeX extension:
  ,("middle",       liftUnion Mmiddle)

  -- LuaTeX extension:
  ,("Umathchar",      liftUnion MUmathchar)
  ,("Umathaccent",    liftUnion MUmathaccent)
  ,("Udelimiter",     liftUnion MUdelimiter)
  ,("Uradical",       liftUnion MUradical)
  ,("Umathcharnum",   liftUnion MUmathcharnum)
  ,("Uroot",          liftUnion MUroot)
  ,("Uoverdelimiter", liftUnion MUoverdelimiter)
  ,("Uunderdelimiter",liftUnion MUunderdelimiter)
  ,("Udelimiterover", liftUnion MUdelimiterover)
  ,("Udelimiterunder",liftUnion MUdelimiterunder)
  ,("Uhextensible",   liftUnion MUhextensible)
  ,("Uskewed",        liftUnion MUskewed)
  ,("Uskewedwithdelims",liftUnion MUskewedwithdelims)
  ,("Ustack",         liftUnion MUstack)
  ,("Usuperscript",   liftUnion MUsuperscript)
  ,("Usubscript",     liftUnion MUsubscript)
  ,("Unosuperscript", liftUnion MUnosuperscript)
  ,("Unosubscript",   liftUnion MUnosubscript)
  ,("Uleft",          liftUnion MUleft)
  ,("Umiddle",        liftUnion MUmiddle)
  ,("Uright",         liftUnion MUright)
  ,("Ustopmath",      liftUnion MUstopmath)
  ,("Ustopdisplaymath",liftUnion MUstopdisplaymath)

  ,("displaystyle",            liftUnion (MathStyleSet DisplayStyle))
  ,("textstyle",               liftUnion (MathStyleSet TextStyle))
  ,("scriptstyle",             liftUnion (MathStyleSet ScriptStyle))
  ,("scriptscriptstyle",       liftUnion (MathStyleSet ScriptScriptStyle))

  -- LuaTeX extensions:
  ,("crampeddisplaystyle",     liftUnion (MathStyleSet CrampedDisplayStyle))
  ,("crampedtextstyle",        liftUnion (MathStyleSet CrampedTextStyle))
  ,("crampedscriptstyle",      liftUnion (MathStyleSet CrampedScriptStyle))
  ,("crampedscriptscriptstyle",liftUnion (MathStyleSet CrampedScriptScriptStyle))

  ,("YuruMathSetNormal",             liftUnion (MathVariantSet MVNormal))
  ,("YuruMathSetBold",               liftUnion (MathVariantSet MVBold))
  ,("YuruMathSetItalic",             liftUnion (MathVariantSet MVItalic))
  ,("YuruMathSetBoldItalic",         liftUnion (MathVariantSet MVBoldItalic))
  ,("YuruMathSetDoubleStruck",       liftUnion (MathVariantSet MVDoubleStruck))
  ,("YuruMathSetBoldFraktur",        liftUnion (MathVariantSet MVBoldFraktur))
  ,("YuruMathSetScript",             liftUnion (MathVariantSet MVScript))
  ,("YuruMathSetBoldScript",         liftUnion (MathVariantSet MVBoldScript))
  ,("YuruMathSetFraktur",            liftUnion (MathVariantSet MVFraktur))
  ,("YuruMathSetSansSerif",          liftUnion (MathVariantSet MVSansSerif))
  ,("YuruMathSetBoldSansSerif",      liftUnion (MathVariantSet MVBoldSansSerif))
  ,("YuruMathSetSansSerifItalic",    liftUnion (MathVariantSet MVSansSerifItalic))
  ,("YuruMathSetSansSerifBoldItalic",liftUnion (MathVariantSet MVSansSerifBoldItalic))
  ,("YuruMathSetMonospace",          liftUnion (MathVariantSet MVMonospace))
  ,("YuruMathSetFunctionName",       liftUnion (MathVariantSet MVFunctionName))
  ,("YuruMathSetText",               liftUnion (MathSymbolModeSet SMText))
  ,("YuruMathSetSymbol",             liftUnion (MathSymbolModeSet SMSymbol))
  ])
