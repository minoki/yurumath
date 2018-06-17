{-# LANGUAGE ScopedTypeVariables #-}
module Text.YuruMath.Convert.TeXToMML where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.State (delimiterSlotSmall)
import Text.YuruMath.TeX.Expansion (showDimension)
import Text.YuruMath.TeX.Math.List
import Text.YuruMath.TeX.Math.Postprocess
import Text.YuruMath.Builder.MathML3
import Text.YuruMath.Builder.MathML3.Attributes as A
import Data.Text (Text)
import qualified Data.Text as T
import Data.Semigroup ((<>))

toMML :: forall a. MathStyle -> MathList a -> [MathML]
toMML = doList
  where
    doList :: MathStyle -> MathList a -> [MathML]
    doList !style [] = []
    doList style (IAtom atom : xs) = doAtom style atom : doList style xs
    doList _ (IStyleChange style : xs) = doList style xs -- TODO: Emit <mstyle> with scriptlevel and displaystyle attributes
    doList style (IGenFrac gf num den : xs) = gfrac : doList style xs
      where gfrac = case gf of
                      WithoutDelims bar -> doGFLine bar (num' <> den')
                      WithDelims leftDelim rightDelim bar -> fromList $ doDelimiter leftDelim ++ [doGFLine bar (num' <> den')] ++ doDelimiter rightDelim
            doGFLine GFOver = mfrac
            doGFLine GFAtop = mfrac ! A.linethickness "0"
            doGFLine (GFAbove dimen) = mfrac ! A.linethickness (toValue $ showDimension dimen)
            doGFLine (GFSkewed _slash) = mfrac ! A.bevelled "true" -- the delimiter should be a forward slash
            num' = fromList $ doList (smallerStyle style) num
            den' = fromList $ doList (denominatorStyle style) den
    doList style (IHorizontalMaterial {} : xs) = doList style xs -- not implemented yet
    doList style (IVerticalMaterial {} : xs) = doList style xs -- not implemented yet
    doList style (IGlue {} : xs) = doList style xs -- not implemented yet; <mspace> or <mpadded>?
    doList style (IKern {} : xs) = doList style xs -- not implemented yet; <mspace> or <mpadded>?
    doList style (IBoundary _ _options delim : xs) = doDelimiter delim ++ doList style xs
    doList style (ISizedDelimiter dimen delim : xs) = doSizedDelimiter dimen delim ++ doList style xs
    doList style (IChoice d t s ss : xs) = doList style (doChoice style d t s ss ++ xs)

    doAtom :: MathStyle -> Atom a -> MathML
    doAtom style (atom@OpAtom { atomLimits = DisplayLimits })
      | style == DisplayStyle || style == CrampedDisplayStyle
      = addUnderOver style atom
        $ case atomNucleus atom of
            MFSymbol { symbolVariant = _, symbolContent = content } -> mo ! A.movablelimits "true" $ toMathML content
            _ -> doNucleus style (atomType atom) (atomNucleus atom)

    doAtom style (atom@OpAtom { atomLimits = Limits })
      = addUnderOver style atom
        $ case atomNucleus atom of
            MFSymbol { symbolVariant = _, symbolContent = content } -> mo ! A.movablelimits "false" $ toMathML content
            _ -> doNucleus style (atomType atom) (atomNucleus atom)

    doAtom style (atom@RadAtom {})
      = addSubSup style atom
        $ msqrt $ doNucleus (makeCramped style) ARad (atomNucleus atom)

    doAtom style (atom@RootAtom {})
      = addSubSup style atom
        $ mroot
        $ doNucleus (makeCramped style) ARoot (atomNucleus atom)
        <> index
      where index = case doField (rootDegreeStyle style) (atomRootDegree atom) of
                      Nothing -> mrow mempty
                      Just xs -> fromList xs

    -- doAtom style (atom@AccAtom {}) = _

    doAtom style atom
      = addSubSup style atom
        $ doNucleus (nucleusStyle (atomType atom) style) (atomType atom) (atomNucleus atom)

    addSubSup :: MathStyle -> Atom a -> MathML -> MathML
    addSubSup !style !atom nucleus
      = let sub = doField (subscriptStyle style) (atomSubscript atom)
            sup = doField (superscriptStyle style) (atomSuperscript atom)
        in case (sub,sup) of
             (Nothing, Nothing) -> nucleus
             (Just xs, Nothing) -> msub $ nucleus <> fromList xs
             (Nothing, Just xs) -> msup $ nucleus <> fromList xs
             (Just xs, Just ys) -> msubsup $ nucleus <> fromList xs <> fromList ys

    addUnderOver :: MathStyle -> Atom a -> MathML -> MathML
    addUnderOver !style !atom nucleus
      = let sub = doField (subscriptStyle style) (atomSubscript atom)
            sup = doField (superscriptStyle style) (atomSuperscript atom)
        in case (sub,sup) of
             (Nothing, Nothing) -> nucleus
             (Just xs, Nothing) -> munder $ nucleus <> fromList xs
             (Nothing, Just xs) -> mover $ nucleus <> fromList xs
             (Just xs, Just ys) -> munderover $ nucleus <> fromList xs <> fromList ys

    doNucleus :: MathStyle -> AtomType -> MathField a -> MathML
    doNucleus !style !atomType MFEmpty = mrow mempty
    doNucleus style AOrd (MFSymbol { symbolVariant = v, symbolContent = content })
      = makeMathIdentifier v content -- TODO: Handle numeric literal (<mn>)
    doNucleus style atomType (MFSymbol { symbolVariant = v, symbolContent = content })
      | atomType `elem` [AOp,ABin,ARel,AOpen,AClose,APunct]
      = mo $ toMathML content
    doNucleus style atomType (MFSymbol { symbolVariant = v, symbolContent = content })
      -- atomType == AInner,AOver,AUnder,AAcc,ARad,AVcent,ARoot
      = mo $ toMathML content

    doNucleus style atomType (MFSubList xs) = fromList $ doList style xs
    doNucleus style atomType _ = mrow mempty -- not supported yet

    doField :: MathStyle -> MathField a -> Maybe [MathML]
    doField !style MFEmpty = Nothing
    doField style (MFSymbol { symbolVariant = v, symbolContent = content })
      = Just [makeMathIdentifier v content] -- TODO: Handle numeric literal (<mn>)
    doField style (MFBox {}) = Nothing -- ignored for now
    doField style (MFSubList xs) = Just $ doList style xs

    doDelimiter (DelimiterCode 0) = []
    doDelimiter delim = [mo {- stretchy="true" -} $ toMathML (delimiterSlotSmall delim)] -- expected size?

    doSizedDelimiter dimen (DelimiterCode 0) = []
    doSizedDelimiter dimen delim = [mo ! A.maxsize (toValue $ showDimension dimen) ! A.minsize (toValue $ showDimension dimen) $ toMathML (delimiterSlotSmall delim)]

fromList :: [MathML] -> MathML
fromList [x] = x
fromList xs = mrow $ mconcat xs

makeMathIdentifier :: MathVariant -> Text -> MathML
makeMathIdentifier MVItalic content | T.length content == 1 = mi $ toMathML content
makeMathIdentifier MVNormal content | T.length content >= 2 = mi $ toMathML content
makeMathIdentifier MVFunctionName content | T.length content >= 2 = mi $ toMathML content
makeMathIdentifier v content = mi ! A.mathvariant (variantToAttr v) $ toMathML content

variantToAttr :: MathVariant -> AttributeValue
variantToAttr v = case v of
  MVNormal              -> "normal"
  MVBold                -> "bold"
  MVItalic              -> "italic"
  MVBoldItalic          -> "bold-italic"
  MVDoubleStruck        -> "double-struck"
  MVBoldFraktur         -> "bold-fraktur"
  MVScript              -> "script"
  MVBoldScript          -> "bold-script"
  MVFraktur             -> "fraktur"
  MVSansSerif           -> "sans-serif"
  MVBoldSansSerif       -> "bold-sans-serif"
  MVSansSerifItalic     -> "sans-serif-italic"
  MVSansSerifBoldItalic -> "sans-serif-bold-italic"
  MVMonospace           -> "monospace"
  MVFunctionName        -> "normal" -- TODO: Insert &ApplyFunction; at some layer
