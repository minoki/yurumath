{-# LANGUAGE ScopedTypeVariables #-}
module Text.YuruMath.TeX.Math.Postprocess where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Math.List
import Data.Bifunctor
import Data.Semigroup ((<>))

doChoice :: MathStyle -> MathList a -> MathList a -> MathList a -> MathList a -> MathList a
doChoice        DisplayStyle      d _ _ _  = d
doChoice CrampedDisplayStyle      d _ _ _  = d
doChoice        TextStyle         _ t _ _  = t
doChoice CrampedTextStyle         _ t _ _  = t
doChoice        ScriptStyle       _ _ s _  = s
doChoice CrampedScriptStyle       _ _ s _  = s
doChoice        ScriptScriptStyle _ _ _ ss = ss
doChoice CrampedScriptScriptStyle _ _ _ ss = ss

stripGlueOrKern :: MathList a -> MathList a
stripGlueOrKern (IGlue _ : xs) = xs
stripGlueOrKern (IKern _ : xs) = xs
stripGlueOrKern xs = xs

doEachFieldWithStyle :: (MathStyle -> MathField a -> MathField a) -> MathStyle -> Atom a -> Atom a
doEachFieldWithStyle doField !style !atom@(RootAtom {})
  = atom { atomNucleus     = doField (makeCramped style)      (atomNucleus atom)
         , atomSuperscript = doField (superscriptStyle style) (atomSuperscript atom)
         , atomSubscript   = doField (subscriptStyle style)   (atomSubscript atom)
         , atomRootDegree  = doField (rootDegreeStyle style)  (atomRootDegree atom)
         }
doEachFieldWithStyle doField !style !atom
  = atom { atomNucleus     = doField (nucleusStyle (atomType atom) style) (atomNucleus atom)
         , atomSuperscript = doField (superscriptStyle style) (atomSuperscript atom)
         , atomSubscript   = doField (subscriptStyle style)   (atomSubscript atom)
         }

doEachField :: (MathField a -> MathField a) -> Atom a -> Atom a
doEachField doField !atom@(RootAtom {})
  = atom { atomNucleus     = doField (atomNucleus atom)
         , atomSuperscript = doField (atomSuperscript atom)
         , atomSubscript   = doField (atomSubscript atom)
         , atomRootDegree  = doField (atomRootDegree atom)
         }
doEachField doField !atom
  = atom { atomNucleus     = doField (atomNucleus atom)
         , atomSuperscript = doField (atomSuperscript atom)
         , atomSubscript   = doField (atomSubscript atom)
         }

doNonScript :: forall a. MathStyle -> MathList a -> MathList a
doNonScript = doList
  where
    doList :: MathStyle -> MathList a -> MathList a
    doList !style [] = []
    doList !style (IGlue MGNonscript : xs)
      | isScriptOrSmaller style = doList style (stripGlueOrKern xs)
      | otherwise = doList style xs
    doList !style (IAtom atom : xs) = IAtom (doAtom style atom) : doList style xs
    doList !_ (i@(IStyleChange style) : xs) = i : doList style xs
    doList !style (IGenFrac gf num den : xs) = IGenFrac gf (doList (smallerStyle style) num) (doList (denominatorStyle style) den) : doList style xs
    doList !style (x : xs) = x : doList style xs

    doAtom :: MathStyle -> Atom a -> Atom a
    doAtom = doEachFieldWithStyle doField

    doField :: MathStyle -> MathField a -> MathField a
    doField !style f@(MFBox {}) = f -- TODO
    doField !style (MFSubList xs) = MFSubList (doList style xs)
    doField !style field = field

determineChoice :: forall a. MathStyle -> MathList a -> MathList a
determineChoice = doList
  where
    doList :: MathStyle -> MathList a -> MathList a
    doList !style [] = []
    doList !style (IAtom atom : xs) = IAtom (doAtom style atom) : doList style xs
    doList !_ (i@(IStyleChange style) : xs) = i : doList style xs -- keep IStyleChange for now
    doList !style (IGenFrac gf num den : xs) = IGenFrac gf (doList (smallerStyle style) num) (doList (denominatorStyle style) den) : doList style xs
    doList !style (IChoice d t s ss : xs) = doList style (doChoice style d t s ss ++ xs)
    doList !style (x : xs) = x : doList style xs

    doAtom :: MathStyle -> Atom a -> Atom a
    doAtom = doEachFieldWithStyle doField

    doField :: MathStyle -> MathField a -> MathField a
    doField !style f@(MFBox {}) = f -- TODO
    doField !style (MFSubList xs) = MFSubList (doList style xs)
    doField !style field = field

-- Assume that the choice is already done
nextAtomTypeInList :: MathList a -> Maybe AtomType
nextAtomTypeInList [] = Nothing
nextAtomTypeInList (IAtom atom : _) = Just (atomType atom)
nextAtomTypeInList (IGenFrac _ _ _ : _) = Just AInner
nextAtomTypeInList (_ : xs) = nextAtomTypeInList xs

determineBinForm :: forall a. MathList a -> MathList a
determineBinForm = doList Nothing
  where
    doList :: Maybe AtomType -> MathList a -> MathList a
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

    doAtom :: Atom a -> Atom a
    doAtom = doEachField doField

    doField :: MathField a -> MathField a
    doField f@(MFBox {}) = f -- TODO
    doField (MFSubList xs) = MFSubList (doList Nothing xs)
    doField field = field

pairOpenClose :: forall a. MathList a -> MathList a
pairOpenClose xs = doList xs
  where
    doList :: MathList a -> MathList a
    doList [] = []
    doList (IAtom atom@(OpenAtom { atomIsDelimiter = True }) : xs)
      = case findClosing xs of
          (content,Nothing) -> {- no closing paren found -} IAtom (doAtom atom) : content
          (content,Just (closing,rest)) -> mkImplicitGroup (doAtom atom) content closing : doList rest
    doList (IAtom atom@(CloseAtom { atomIsDelimiter = True }) : xs) = IAtom (doAtom atom) : doList xs -- unmatched parenthesis
    doList (IAtom atom : xs) = IAtom (doAtom atom) : doList xs
    doList (IGenFrac gf num den : xs) = IGenFrac gf (doList num) (doList den) : doList xs
    doList (x : xs) = x : doList xs

    findClosing :: MathList a -> (MathList a,Maybe (Atom a,MathList a))
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

    doAtom :: Atom a -> Atom a
    doAtom = doEachField doField

    doField :: MathField a -> MathField a
    doField (MFSubList xs) = MFSubList (doList xs)
    doField field = field

    mkImplicitGroup :: Atom a -> MathList a -> Atom a -> MathItem a
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

textSymbol :: forall a. MathList a -> MathList a
textSymbol = doList
  where
    doList :: MathList a -> MathList a
    doList [] = []
    doList (IAtom atom@(OrdAtom { atomNucleus = MFSymbol fam var SMText text, atomSuperscript = MFEmpty, atomSubscript = MFEmpty })
            : IAtom nextAtom@(OrdAtom { atomNucleus = MFSymbol fam' var' SMText text' }) : xs)
      | fam == fam', var == var' = doList (IAtom nextAtom { atomNucleus = MFSymbol fam var SMText (text <> text') } : xs)
    doList (IAtom atom : xs) = IAtom (doAtom atom) : doList xs
    doList (IGenFrac gf num den : xs) = IGenFrac gf (doList num) (doList den) : doList xs
    doList (x : xs) = x : doList xs

    doAtom :: Atom a -> Atom a
    doAtom = doEachField doField

    doField :: MathField a -> MathField a
    doField f@(MFBox {}) = f -- TODO
    doField (MFSubList xs) = MFSubList (doList xs)
    doField field = field

-- TODO: Handle generalized fraction with delims
