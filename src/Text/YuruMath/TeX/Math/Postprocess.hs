module Text.YuruMath.TeX.Math.Postprocess where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Math.List
import Data.Bifunctor
import Data.Semigroup ((<>))

doChoice :: MathStyle -> MathList -> MathList -> MathList -> MathList -> MathList
doChoice        DisplayStyle      d _ _ _  = d
doChoice CrampedDisplayStyle      d _ _ _  = d
doChoice        TextStyle         _ t _ _  = t
doChoice CrampedTextStyle         _ t _ _  = t
doChoice        ScriptStyle       _ _ s _  = s
doChoice CrampedScriptStyle       _ _ s _  = s
doChoice        ScriptScriptStyle _ _ _ ss = ss
doChoice CrampedScriptScriptStyle _ _ _ ss = ss

stripGlueOrKern :: MathList -> MathList
stripGlueOrKern (IGlue _ : xs) = xs
stripGlueOrKern (IKern _ : xs) = xs
stripGlueOrKern xs = xs

doNonScript :: MathStyle -> MathList -> MathList
doNonScript = doList
  where
    doList :: MathStyle -> MathList -> MathList
    doList !style [] = []
    doList !style (IGlue MGNonscript : xs)
      | isScriptOrSmaller style = doList style (stripGlueOrKern xs)
      | otherwise = doList style xs
    doList !style (IAtom atom : xs) = IAtom (doAtom style atom) : doList style xs
    doList !_ (i@(IStyleChange style) : xs) = i : doList style xs
    doList !style (IGenFrac gf num den : xs) = IGenFrac gf (doList (smallerStyle style) num) (doList (denominatorStyle style) den) : doList style xs
    doList !style (x : xs) = x : doList style xs
    doAtom :: MathStyle -> Atom -> Atom
    doAtom !style !atom = atom { atomNucleus = doField (nucleusStyle (atomType atom) style) (atomNucleus atom)
                               , atomSuperscript = doField (superscriptStyle style) (atomSuperscript atom)
                               , atomSubscript = doField (subscriptStyle style) (atomSubscript atom)
                               }
    doField :: MathStyle -> MathField -> MathField
    doField !style MFBox = MFBox -- TODO
    doField !style (MFSubList xs) = MFSubList (doList style xs)
    doField !style field = field

determineChoice :: MathStyle -> MathList -> MathList
determineChoice = doList
  where
    doList :: MathStyle -> MathList -> MathList
    doList !style [] = []
    doList !style (IAtom atom : xs) = IAtom (doAtom style atom) : doList style xs
    doList !_ (i@(IStyleChange style) : xs) = i : doList style xs -- keep IStyleChange for now
    doList !style (IGenFrac gf num den : xs) = IGenFrac gf (doList (smallerStyle style) num) (doList (denominatorStyle style) den) : doList style xs
    doList !style (IChoice d t s ss : xs) = doList style (doChoice style d t s ss ++ xs)
    doList !style (x : xs) = x : doList style xs
    doAtom :: MathStyle -> Atom -> Atom
    doAtom !style !atom = atom { atomNucleus = doField (nucleusStyle (atomType atom) style) (atomNucleus atom)
                               , atomSuperscript = doField (superscriptStyle style) (atomSuperscript atom)
                               , atomSubscript = doField (subscriptStyle style) (atomSubscript atom)
                               }
    doField :: MathStyle -> MathField -> MathField
    doField !style MFBox = MFBox -- TODO
    doField !style (MFSubList xs) = MFSubList (doList style xs)
    doField !style field = field

-- Assume that the choice is already done
nextAtomTypeInList :: MathList -> Maybe AtomType
nextAtomTypeInList [] = Nothing
nextAtomTypeInList (IAtom atom : _) = Just (atomType atom)
nextAtomTypeInList (IGenFrac _ _ _ : _) = Just AInner
nextAtomTypeInList (_ : xs) = nextAtomTypeInList xs

determineBinForm :: MathList -> MathList
determineBinForm = doList Nothing
  where
    doList :: Maybe AtomType -> MathList -> MathList
    doList _ [] = []
    doList prevAtomType (IAtom atom@(BinAtom {}) : xs)
      -- should really change to OrdAtom?
      | isPrefix || isPostfix = IAtom (doAtom atom { atomBinForm = form }) : doList (Just AOrd) xs
      | otherwise = IAtom (doAtom atom) : doList (Just ABin) xs
      where nextAtomType = nextAtomTypeInList xs
            isPrefix = prevAtomType `elem` [Nothing, Just ABin, Just AOp, Just ARel, Just AOpen, Just APunct]
            isPostfix = nextAtomType `elem` [Nothing, Just ARel, Just AClose, Just APunct]
            form | isPostfix = BinPostfix
                 | isPrefix = BinPrefix
                 | otherwise = BinInfix
    doList _ (IAtom atom : xs) = IAtom (doAtom atom) : doList (Just (atomType atom)) xs
    doList _ (IGenFrac gf num den : xs) = IGenFrac gf (doList Nothing num) (doList Nothing den) : doList (Just AInner) xs
    doList prevAtomType (x : xs) = x : doList prevAtomType xs -- keep IStyleChange for now
    doAtom :: Atom -> Atom
    doAtom !atom = atom { atomNucleus = doField (atomNucleus atom)
                        , atomSuperscript = doField (atomSuperscript atom)
                        , atomSubscript = doField (atomSubscript atom)
                        }
    doField :: MathField -> MathField
    doField MFBox = MFBox -- TODO
    doField (MFSubList xs) = MFSubList (doList Nothing xs)
    doField field = field

pairOpenClose :: MathList -> MathList
pairOpenClose xs = doList xs
  where
    doList :: MathList -> MathList
    doList [] = []
    doList (IAtom atom@(OpenAtom { atomIsDelimiter = True }) : xs)
      = case findClosing xs of
          (content,Nothing) -> {- no closing paren found -} IAtom (doAtom atom) : content
          (content,Just (closing,rest)) -> mkImplicitGroup (doAtom atom) content closing : doList rest
    doList (IAtom atom@(CloseAtom { atomIsDelimiter = True }) : xs) = IAtom (doAtom atom) : doList xs -- unmatched parenthesis
    doList (IAtom atom : xs) = IAtom (doAtom atom) : doList xs
    doList (IGenFrac gf num den : xs) = IGenFrac gf (doList num) (doList den) : doList xs
    doList (x : xs) = x : doList xs

    findClosing :: MathList -> (MathList,Maybe (Atom,MathList))
    findClosing [] = ([],Nothing)
    findClosing (IAtom atom@(OpenAtom { atomIsDelimiter = True }) : xs)
      = case findClosing xs of
          (content,Nothing) -> {- no closing paren found -} (IAtom (doAtom atom) : content, Nothing)
          (content,Just (closing,rest)) -> first (mkImplicitGroup (doAtom atom) content closing :) $ findClosing rest
    findClosing (IAtom atom@(CloseAtom { atomIsDelimiter = True }) : xs)
      = ([],Just (doAtom atom,xs))
    findClosing (IAtom atom : xs)
      = first (IAtom (doAtom atom) :) $ findClosing xs
    findClosing (x : xs)
      = first (x :) $ findClosing xs

    doAtom :: Atom -> Atom
    doAtom atom = atom { atomNucleus = doField (atomNucleus atom)
                       , atomSuperscript = doField (atomSuperscript atom)
                       , atomSubscript = doField (atomSubscript atom)
                       }

    doField :: MathField -> MathField
    doField (MFSubList xs) = MFSubList (doList xs)
    doField field = field

    mkImplicitGroup :: Atom -> MathList -> Atom -> MathItem
    mkImplicitGroup opening content closing
      = IAtom (mkAtom AOrd (MFSubList [IAtom opening
                                      ,IAtom (mkAtom AOrd (MFSubList content))
                                      ,IAtom closing'
                                      ])) { atomSuperscript = atomSuperscript closing
                                          , atomSubscript = atomSubscript closing
                                          }
      where closing' = closing { atomSuperscript = MFEmpty
                               , atomSubscript = MFEmpty
                               }

textSymbol :: MathList -> MathList
textSymbol = doList
  where
    doList :: MathList -> MathList
    doList [] = []
    doList (IAtom atom@(OrdAtom { atomNucleus = MFSymbol fam var SMText text, atomSuperscript = MFEmpty, atomSubscript = MFEmpty })
            : IAtom nextAtom@(OrdAtom { atomNucleus = MFSymbol fam' var' SMText text' }) : xs)
      | fam == fam', var == var' = doList (IAtom nextAtom { atomNucleus = MFSymbol fam var SMText (text <> text') } : xs)
    doList (IAtom atom : xs) = IAtom (doAtom atom) : doList xs
    doList (IGenFrac gf num den : xs) = IGenFrac gf (doList num) (doList den) : doList xs
    doList (x : xs) = x : doList xs
    doAtom :: Atom -> Atom
    doAtom !atom = atom { atomNucleus = doField (atomNucleus atom)
                        , atomSuperscript = doField (atomSuperscript atom)
                        , atomSubscript = doField (atomSubscript atom)
                        }
    doField :: MathField -> MathField
    doField MFBox = MFBox -- TODO
    doField (MFSubList xs) = MFSubList (doList xs)
    doField field = field

-- TODO: Handle generalized fraction with delims
