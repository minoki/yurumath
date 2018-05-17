module Text.YuruMath.Convert.TeXToMML where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Math
import Text.YuruMath.TeX.PostMath
import Text.YuruMath.Builder.MathML3
import Text.YuruMath.Builder.MathML3.Attributes as A

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
      -- TODO: mark nucleus as "movablelimits=true"
      = let n = doNucleus style (atomType atom) (atomNucleus atom)
            sub = doField (subscriptStyle style) (atomSubscript atom)
            sup = doField (superscriptStyle style) (atomSuperscript atom)
        in case (sub,sup) of
             (Nothing, Nothing) -> n
             (Just xs, Nothing) -> munder n (fromList xs)
             (Nothing, Just xs) -> mover n (fromList xs)
             (Just xs, Just ys) -> munderover n (fromList xs) (fromList ys)
    doAtom style (atom@OpAtom { atomLimits = Limits })
      -- TODO: mark nucleus as "movablelimits=false"
      = let n = doNucleus style (atomType atom) (atomNucleus atom)
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
    doNucleus style AOrd (MFSymbol fam slot) = mi $ toMathML [slot]
    doNucleus style AOrd (MFTextSymbol fam s) = mi $ toMathML s -- TODO: Handle numeric literal (<mn>)
    doNucleus style atomType (MFSymbol fam slot) | atomType `elem` [AOp,ABin,ARel,AOpen,AClose,APunct] = mo $ toMathML [slot]
    doNucleus style atomType (MFTextSymbol fam s) | atomType `elem` [AOp,ABin,ARel,AOpen,AClose,APunct] = mo $ toMathML s
    doNucleus style atomType (MFSubList xs) = fromList $ doList style xs
    doNucleus style atomType _ = mrow mempty -- not supported yet
      -- movable limits
    doField :: MathStyle -> MathField -> Maybe [MathML]
    doField !style MFEmpty = Nothing
    doField style (MFSymbol fam slot) = Just [mi $ toMathML [slot]] -- TODO: Handle mn
    doField style (MFTextSymbol fam s) = Just [mi $ toMathML s]
    doField style MFBox = Nothing -- ignored for now
    doField style (MFSubList xs) = Just $ doList style xs

fromList :: [MathML] -> MathML
fromList [x] = x
fromList xs = mrow $ mconcat xs
