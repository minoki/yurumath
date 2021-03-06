{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Text.YuruMath.TeX.Math.Parser
 (MMDGlobal(..)
 ,MMDInline(..)
 ,MMDDisplay(..)
 ,readMathMaterial
 ) where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Quantity
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Expansion
import Text.YuruMath.TeX.Typeset
import Text.YuruMath.TeX.Meaning
import Text.YuruMath.TeX.Math.List
import Text.YuruMath.TeX.Math.State
import Text.YuruMath.TeX.Math.Command
import Data.Word
import Data.Bits
import qualified Data.Text as T
import Data.Monoid (mempty,Any(..),First(..))
import Control.Monad.Reader (local)
import Control.Monad.Except
import Control.Lens.Getter (use,view)
import Control.Lens.Setter (set,assign)
import Control.Lens.Iso (non)
import Control.Lens.Tuple (_1,_2,_3,_4,_5)
import Data.OpenUnion
import TypeFun.Data.List (Delete)
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
  MTAccent     :: !MathAccent -> !(Maybe Int) -> MathToken m
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
  MTBox        :: !Bool -> !BoxCommand -> MathToken m
  MTSizedDelimiter :: !Dimen -> !DelimiterCode -> MathToken m
  MTOther      :: (DoExecute v m, Show v) => v -> MathToken m

deriving instance Show (MathToken m)

type MonadMathState state m
  = ( MonadTeXState state m
    , IsMathState state
    , DoExecute (Union (Delete BoxCommand (Delete TypesetCommand (Delete MathSymbolModeSet (Delete MathVariantSet (Delete MathCommands (Delete MathAtomCommand (Delete MathStyleSet (Delete CommonValue (NValueSet state)))))))))) m
    , Show (Union (Delete BoxCommand (Delete TypesetCommand (Delete MathSymbolModeSet (Delete MathVariantSet (Delete MathCommands (Delete MathAtomCommand (Delete MathStyleSet (Delete CommonValue (NValueSet state))))))))))
    )

readMathToken :: forall m state. (MonadMathState state m, MonadError String m) => m (Maybe (ExpansionToken,MathToken m))
readMathToken = do
  v <- nextExpandedToken
  case v of
    Nothing -> return Nothing -- end of input
    Just (et,v) -> doMathToken et v
  where
    doMathToken :: ExpansionToken -> NValue state -> m (Maybe (ExpansionToken,MathToken m))
    doMathToken et = doCommonValue et
                     @> (\(v :: MathStyleSet)    -> just <$> doMathStyleSet v)
                     @> (\(v :: MathAtomCommand) -> just <$> doMathAtom v)
                     @> (\(v :: MathCommands)    -> just <$> doOtherMathCommand v)
                     @> (\(v :: MathVariantSet)  -> just <$> doMathVariantSet v)
                     @> (\(v :: MathSymbolModeSet) -> just <$> doMathSymbolModeSet v)
                     @> (\(v :: TypesetCommand)  -> just <$> doTypesetCommand v)
                     @> (\(v :: BoxCommand)      -> just <$> doBoxCommand v)
                     @> (\v                      -> return $ just $ MTOther v) -- other assignments, etc
      where just v = Just (et,v)
    doCommonValue :: ExpansionToken -> CommonValue -> m (Maybe (ExpansionToken,MathToken m))
    doCommonValue et v = case v of
      Character _ CCBeginGroup   -> return $ Just (et,MTLBrace)
      Character _ CCEndGroup     -> return $ Just (et,MTRBrace)
      Character _ CCMathShift -> do
        m <- view mode
        if m == DisplayMathMode
          then do et' <- nextUnexpandedToken -- without expansion
                  case et' of
                    Just (ETCharacter { etCatCode = CCMathShift }) -> return $ Just (et,MTStopDisplay)
                    _ -> throwError "Display math should end with $$."
          else return $ Just (et,MTStopInline)
      Character _ CCAlignmentTab -> throwError "alignment tab: not implemented yet"
      Character _ CCSup          -> return $ Just (et,MTSup)
      Character _ CCSub          -> return $ Just (et,MTSub)
      Character _ CCSpace        -> readMathToken -- do nothing
      Character c CCLetter       -> return $ Just (et,MTChar c)
      Character c CCOther        -> return $ Just (et,MTChar c)
      Character _ cc             -> throwError $ "Unexpected " ++ show cc ++ " character" -- endline, param, ignored, active, comment, invalid
      DefinedCharacter c         -> return $ Just (et,MTChar c)
      DefinedMathCharacter c     -> return $ Just (et,MTMathChar c)
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
      Thfil         -> return $ MTGlue $ MGHSkip $ zeroQ `plus` fil 1
      Thfill        -> return $ MTGlue $ MGHSkip $ zeroQ `plus` fill 1
      Thss          -> return $ MTGlue $ MGHSkip $ zeroQ `plus` fil 1 `minus` fil 1
      Thfilneg      -> return $ MTGlue $ MGHSkip $ zeroQ `plus` fil (-1)

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
        MTMathChar <$> readUMathCodeTriplet

      -- \Umathcharnum<signed 32-bit integer>
      MUmathcharnum -> do
        MTMathChar <$> readUMathCode32

      -- \delimiter<27-bit integer>
      Mdelimiter -> do
        x <- readIntBetween 0 (2^(27::Int)-1)
        let mathclass = toEnum (x `shiftR` 24) :: MathClass
            value = fromIntegral (0xFFFFFF .&. x)
        return $ MTDelimiter mathclass (DelimiterCode value)

      -- \Udelimiter<0-7><0-"FF><0-"10FFFF>
      MUdelimiter -> do
        mathclass <- toEnum <$> readIntBetween 0 7
        code <- readUDelimiterCodePair
        return $ MTDelimiter mathclass code

      -- \radical<24-bit integer><math field>
      Mradical -> do
        x <- readIntBetween 0 (2^(24::Int)-1)
        -- x = <12-bit: small variant><12-bit: large variant>
        return $ MTRadical $ DelimiterCode $ fromIntegral x

      -- \Uradical<0-"FF><0-"10FFFF><math field>
      MUradical -> do
        MTRadical <$> readUDelimiterCodePair

      -- \Uroot<0-"FF><0-"10FFFF><math field><math field>
      MUroot -> do
        MTRoot <$> readUDelimiterCodePair

      -- \mathaccent<15-bit integer><math field>
      Mmathaccent -> do
        x <- readIntBetween 0 0x7FFF
        let value = fromIntegral $ x .&. 0x0FFF
        return $ MTAccent (mkTopAccent True (MathCode value)) Nothing

      -- \Umathaccent ("fixed" | "bottom"<optional "fixed"> | "top"<optional "fixed"> | "overlay"<optional "fixed"> | <empty>)<0-7><0-"FF"><0-"10FFFF> ("fraction"<number>)?
      -- \Umathaccent "both" <optional "fixed"><0-7><0-"FF"><0-"10FFFF> <optional "fixed"><0-7><0-"FF"><0-"10FFFF>
      --  ...<math field>
      MUmathaccent -> do
        m <- readOneOfKeywordsV
             [("fixed"
              ,mkTopAccent True <$> readUMathCodeTriplet
              )
             ,("both"
              ,mkBothAccent <$> readKeyword "fixed" <*> readUMathCodeTriplet
                <*> readKeyword "fixed" <*> readUMathCodeTriplet
              )
             ,("bottom"
              ,mkBottomAccent <$> readKeyword "fixed" <*> readUMathCodeTriplet
              )
             ,("top"
              ,mkTopAccent <$> readKeyword "fixed" <*> readUMathCodeTriplet
              )
             ,("overlay"
              ,mkOverlayAccent <$> readKeyword "fixed" <*> readUMathCodeTriplet
              )
             ]
        a <- case m of
               Just m -> m
               Nothing -> mkTopAccent False <$> readUMathCodeTriplet
        fraction <- readKeyword "fraction"
        mfraction <- if fraction
                     then Just <$> readGeneralInt
                     else pure Nothing
        return $ MTAccent a mfraction

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

      -- \YuruMathSizedDelimiter<dimen><delim>
      MYuruMathSizedDelimiter -> MTSizedDelimiter <$> readDimension <*> readDelimiter

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

      -- \vcenter<box specification>{<vertical mode material>}
      Mvcenter -> return $ MTBox True Bvbox

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
      MYuruMathInternalMathStyle -> can'tUseThisCommandInCurrentMode v
    doBoxCommand box = return $ MTBox False box

readDelimiter :: (MonadMathState state m, MonadError String m) => m DelimiterCode
readDelimiter = do
  t <- readMathToken
  case t of
    Nothing -> throwError "Missing delimiter: got end of input"
    Just (_,MTChar c) -> do
      delimiterCodeOf c
    Just (_,MTDelimiter _ code) -> do
      return code
    Just (_,t) -> throwError $ "Missing delimiter: got " ++ show t

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
  oldStyle <- use (currentMathStyle . non (error "Internal error: currentMathStyle is Nothing"))
  assign currentMathStyle (Just (f oldStyle))
  x <- m
  assign currentMathStyle (Just oldStyle)
  return x

class (Functor f) => MathMaterialEnding f where
  onEndOfInput      :: MonadError String m => m (MathList a) -> m (f (MathList a))
  onRightBrace      :: MonadError String m => m (MathList a) -> m (f (MathList a))
  onMiddleDelim     :: MonadError String m => DelimiterOptions -> DelimiterCode -> m (MathList a) -> m (f (MathList a))
  onRightDelim      :: MonadError String m => DelimiterOptions -> DelimiterCode -> m (MathList a) -> m (f (MathList a))
  onStopInlineMath  :: MonadError String m => m (MathList a) -> m (f (MathList a))
  onStopDisplayMath :: MonadError String m => m (MathList a) -> m (f (MathList a))
  onGenFrac         :: MonadError String m => GenFrac -> m (MathList a) -> m (f (MathList a))
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

readMathMaterial :: forall f state m a. (MathMaterialEnding f, MonadMathState state m, MonadError String m, BoxReader a m) => m (f (MathList a))
readMathMaterial = loop []
  where
    loop :: MathList a -> m (f (MathList a))
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
        Just (et,t) -> case t of
          -- <character>
          MTChar c -> do
            mc <- mathCodeOf c
            if mc == mathActive
              then do -- math active
                      unreadToken (ETCommandName { etDepth = etDepth et + 1, etFlavor = ECNFPlain, etName = NActiveChar c })
                      loop revList
              else do delcode <- delimiterCodeOf c
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
              [item@(IAtom (AtomWithScripts { atomNucleus = AccAtom {} }))] -> loop (item : revList) -- single Acc atom
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
              if isEmptyField (atomSuperscript atom)
              then return (atom { atomSuperscript = x })
              else throwError "Double superscript"

          -- <subscript><math field>
          MTSub -> do
            x <- withMathStyle subscriptStyle readMathField
            modifyLastAtom $ \atom ->
              if isEmptyField (atomSubscript atom)
              then return (atom { atomSubscript = x })
              else throwError "Double subscript"

          -- \mathaccent<15-bit number><math field> or \Umathaccent<...><math field>
          MTAccent a fraction -> do
            content <- withMathStyle makeCramped readMathField
            doAtom $ withEmptyScripts $ (mkAtomNucleus AAcc content) { atomAccent = a, atomAccentFraction = fraction }

          -- \radical<27-bit number><math field> or \Uradical<...><math field>
          MTRadical code -> do
            content <- withMathStyle makeCramped readMathField
            doAtom $ withEmptyScripts $ (mkAtomNucleus ARad content) { atomDelimiter = code }

          -- \Uroot<...><math field><math field>
          MTRoot code -> do
            degree <- withMathStyle rootDegreeStyle readMathField -- SS or SS'
            content <- withMathStyle makeCramped readMathField
            doAtom $ withEmptyScripts $ (mkAtomNucleus ARoot content) { atomDelimiter = code, atomRootDegree = degree }

          -- \limits, \nolimits, \displaylimits
          MTLimitsSpec spec -> do
            case revList of
              IAtom atom@(AtomWithScripts { atomNucleus = nucleus@(OpAtom {}) }) : xs -> loop (IAtom (atom { atomNucleus = nucleus { atomLimits = spec } }) : xs)
              _ -> throwError "Limit controls must follow a math operator."

          -- \displaystyle, \textstyle, etc
          MTSetStyle newStyle -> do
            assign currentMathStyle (Just newStyle)
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

          MTSizedDelimiter dimen delim -> do
            loop (ISizedDelimiter dimen delim : revList)

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
            currentStyle <- use (currentMathStyle . non (error "Internal error: currentMathStyle is Nothing"))
            let -- doChoiceBranch :: MathStyle -> m (MathList a)
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
            let chosen = case currentStyle of
                  DisplayStyle             -> d
                  CrampedDisplayStyle      -> d
                  TextStyle                -> t
                  CrampedTextStyle         -> t
                  ScriptStyle              -> s
                  CrampedScriptStyle       -> s
                  ScriptScriptStyle        -> ss
                  CrampedScriptScriptStyle -> ss
            loop (reverse chosen ++ revList)

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

          MTBox isVCenter boxCommand -> do
            box <- readBox boxCommand
            doAtom (mkAtom (if isVCenter then AOrd else AVcent) (MFBox box))

          -- assignments, etc
          MTOther v -> do
            doExecute v
            loop revList

-- <math symbol> ::= <character> | <math character>
-- <math field> ::= <math symbol> | {<math mode material>}
readMathField :: (MonadMathState state m, MonadError String m, BoxReader a m) => m (MathField a)
readMathField = do
  t <- readMathToken
  case t of
    Nothing -> throwError "Unexpected end of input: expected a math field"
    Just (et,t) -> case t of
      MTChar c -> do
        mc <- mathCodeOf c
        if mc == mathActive
          then do -- math active
                  unreadToken (ETCommandName { etDepth = etDepth et + 1, etFlavor = ECNFPlain, etName = NActiveChar c })
                  readMathField
          else makeMathSymbol (mathcharClass mc) (mathcharFamily mc) (mathcharSlot mc)
      MTMathChar mc -> do -- \mathchar or \mathchardef-ed
        makeMathSymbol (mathcharClass mc) (mathcharFamily mc) (mathcharSlot mc)
      MTDelimiter mathclass del -> do -- \delimiter
        makeMathSymbol mathclass (delimiterFamilySmall del) (delimiterSlotSmall del)
      MTLBrace -> do
        enterGroup ScopeByBrace
        content <- runMMDBrace <$> readMathMaterial
        return $ case content of
          [IAtom (AtomWithScripts { atomNucleus = OrdAtom { nucleusField = n }
                                  , atomSuperscript = MFEmpty
                                  , atomSubscript = MFEmpty })] -> n
          _ -> MFSubList content
      _ -> throwError $ "Unexpected " ++ show t ++ "; expected a symbol or `{'"

makeMathSymbol :: (MonadTeXState state m, IsMathState state) => MathClass -> Word8 -> Char -> m (MathField a)
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
