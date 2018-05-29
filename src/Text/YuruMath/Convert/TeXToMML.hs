module Text.YuruMath.Convert.TeXToMML where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Math
import Text.YuruMath.TeX.Math.Style
import Text.YuruMath.TeX.PostMath
import Text.YuruMath.Builder.MathML3
import Text.YuruMath.Builder.MathML3.Attributes as A
import Data.Text (Text)
import qualified Data.Text as T

toMML :: MathStyle -> MathList -> [MathML]
toMML = doList
  where
    doList :: MathStyle -> MathList -> [MathML]
    doList !style [] = []
    doList style (IAtom atom : xs) = doAtom style atom : doList style xs
    doList _ (IStyleChange style : xs) = doList style xs
    doList style (IGenFrac gf num den : xs) = mfrac (fromList $ doList (smallerStyle style) num) (fromList $ doList (denominatorStyle style) den) : doList style xs
    doList style (_ : xs) = doList style xs -- not implemented yet
    doAtom :: MathStyle -> Atom -> MathML
    doAtom style (atom@OpAtom { atomLimits = DisplayLimits })
      = let n | MFSymbol { symbolVariant = _, symbolContent = content } <- atomNucleus atom = mo ! A.movablelimits "true" $ toMathML content
              | otherwise = doNucleus style (atomType atom) (atomNucleus atom)
            sub = doField (subscriptStyle style) (atomSubscript atom)
            sup = doField (superscriptStyle style) (atomSuperscript atom)
        in case (sub,sup) of
             (Nothing, Nothing) -> n
             (Just xs, Nothing) -> munder n (fromList xs)
             (Nothing, Just xs) -> mover n (fromList xs)
             (Just xs, Just ys) -> munderover n (fromList xs) (fromList ys)
    doAtom style (atom@OpAtom { atomLimits = Limits })
      = let n | MFSymbol { symbolVariant = _, symbolContent = content } <- atomNucleus atom = mo ! A.movablelimits "false" $ toMathML content
              | otherwise = doNucleus style (atomType atom) (atomNucleus atom)
            sub = doField (subscriptStyle style) (atomSubscript atom)
            sup = doField (superscriptStyle style) (atomSuperscript atom)
        in case (sub,sup) of
             (Nothing, Nothing) -> n
             (Just xs, Nothing) -> munder n (fromList xs)
             (Nothing, Just xs) -> mover n (fromList xs)
             (Just xs, Just ys) -> munderover n (fromList xs) (fromList ys)
    -- doAtom style (atom@RadAtom {}) = _
    -- doAtom style (atom@AccAtom {}) = _
    doAtom style atom
      = let n = doNucleus (nucleusStyle (atomType atom) style) (atomType atom) (atomNucleus atom)
            sub = doField (subscriptStyle style) (atomSubscript atom)
            sup = doField (superscriptStyle style) (atomSuperscript atom)
        in case (sub,sup) of
             (Nothing, Nothing) -> n
             (Just xs, Nothing) -> msub n (fromList xs)
             (Nothing, Just xs) -> msup n (fromList xs)
             (Just xs, Just ys) -> msubsup n (fromList xs) (fromList ys)
    doNucleus :: MathStyle -> AtomType -> MathField -> MathML
    doNucleus !style !atomType MFEmpty = mrow mempty
    doNucleus style AOrd (MFSymbol { symbolVariant = v, symbolContent = content })
      = makeMathIdentifier v content -- TODO: Handle numeric literal (<mn>)
    doNucleus style atomType (MFSymbol { symbolVariant = v, symbolContent = content })
      | atomType `elem` [AOp,ABin,ARel,AOpen,AClose,APunct]
      = mo $ toMathML content
    doNucleus style atomType (MFSubList xs) = fromList $ doList style xs
    doNucleus style atomType _ = mrow mempty -- not supported yet
      -- movable limits
    doField :: MathStyle -> MathField -> Maybe [MathML]
    doField !style MFEmpty = Nothing
    doField style (MFSymbol { symbolVariant = v, symbolContent = content })
      = Just [makeMathIdentifier v content] -- TODO: Handle numeric literal (<mn>)
    doField style MFBox = Nothing -- ignored for now
    doField style (MFSubList xs) = Just $ doList style xs

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
