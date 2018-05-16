{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
import Test.HUnit
import Data.Semigroup
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Tokenizer
import Text.YuruMath.TeX.Expansion
import Text.YuruMath.TeX.Execution
import Text.YuruMath.TeX.Math
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Lens.Cons (_head)
import Control.Lens.Setter (modifying)
import qualified Data.Map.Strict as Map
import Data.OpenUnion

defineBuiltins :: (MonadTeXState s m, MonadError String m, Value s ~ CommonValue, Expandable s ~ Union '[ConditionalMarkerCommand, CommonExpandable, CommonBoolean]) => m ()
defineBuiltins = do
  modifying (localState . tsDefinitions)
    $ mappend (fmap Left expandableDefinitions <> Map.singleton "endcsname" (Right (injectCommonValue Endcsname)))

tokenizeAll :: (MonadTeXState s m, MonadError String m, Value s ~ CommonValue, Expandable s ~ Union '[ConditionalMarkerCommand, CommonExpandable, CommonBoolean]) => m [TeXToken]
tokenizeAll = do
  t <- nextToken
  case t of
    Nothing -> return []
    Just t -> (t:) <$> tokenizeAll

tokenizeAllString :: String -> Either String [TeXToken]
tokenizeAllString input = runExcept (evalStateT tokenizeAll (initialState input))

expandAll :: (MonadTeXState s m, MonadError String m) => m [Value s]
expandAll = do
  t <- evalToValue
  case t of
    Nothing -> return []
    Just v -> (v:) <$> expandAll

expandAllString :: String -> Either String [Value (CommonState (CommonLocalState (Union '[ConditionalMarkerCommand, CommonExpandable, CommonBoolean]) CommonValue))]
expandAllString input = runExcept (evalStateT (defineBuiltins >> expandAll) (initialState input))

type MathExpandableT = Union '[ConditionalMarkerCommand, CommonExpandable, CommonBoolean]
type MathValue = Union '[CommonValue,MathStyleSet,MathAtomCommand,MathCommands,CommonExecutable]
type MathLocalState = CommonLocalState MathExpandableT MathValue
runMathList :: Bool -> String -> Either String MathList
runMathList !isDisplay input = runExcept $ evalStateT action (initialMathState isDisplay $ initialState input)
  where
    action :: StateT (MathState MathLocalState) (Except String) MathList
    action = do
      modifying (localState . tsDefinitions)
        $ \m -> mconcat [fmap Left expandableDefinitions
                        ,fmap Right executableDefinitions
                        ,fmap Right mathDefinitions
                        ,m
                        ]
      runMMDGlobal <$> readMathMaterial defaultMathMaterialContext

ttest1 = TestCase $ assertEqual "Tokenize \\foo bar \\ 1\\23" expected (tokenizeAllString "\\foo bar \\ 1\\23")
  where
    expected = Right [TTCommandName (NControlSeq "foo")
                     ,TTCharacter 'b' CCLetter
                     ,TTCharacter 'a' CCLetter
                     ,TTCharacter 'r' CCLetter
                     ,TTCharacter ' ' CCSpace
                     ,TTCommandName (NControlSeq " ")
                     ,TTCharacter '1' CCOther
                     ,TTCommandName (NControlSeq "2")
                     ,TTCharacter '3' CCOther
                     ]

etest1 = TestCase $ assertEqual "Expand" expected (expandAllString "\\ifnum\"F>14 Y\\else N\\fi")
  where
    expected = Right [Character 'Y' CCLetter
                     ]

etest2 = TestCase $ assertEqual "Expand" expected (expandAllString "\\iftrue \\csname fo\\else \\fi o\\endcsname")
  where
    expected = Right [Relax
                     ]

etest3 = TestCase $ assertEqual "Expand" expected (expandAllString "\\romannumeral'123 ")
  where
    expected = Right [Character 'l' CCOther
                     ,Character 'x' CCOther
                     ,Character 'x' CCOther
                     ,Character 'x' CCOther
                     ,Character 'i' CCOther
                     ,Character 'i' CCOther
                     ,Character 'i' CCOther
                     ]

etest4 = TestCase $ assertEqual "Expand" expected (expandAllString "\\iftrue\\ifnum1<0\\else x\\fi\\fi")
  where
    expected = Right [Character 'x' CCLetter
                     ]

etest5 = TestCase $ assertEqual "Expand" expected (expandAllString "\\iftrue\\number\"1\\else\\fi 0 ")
  where
    expected = Right [Character '1' CCOther
                     ,Character '6' CCOther
                     ]

mtest1 = TestCase $ assertEqual "Math" expected (runMathList True "1+1")
  where
{-
$$1+1\showlists$$ ->
\mathord
.\fam0 1
\mathbin
.\fam0 +
\mathord
.\fam0 1
-}
    expected = Right [IAtom (mkAtom AOrd (MFSymbol 0 '1'))
                     ,IAtom (mkAtom ABin (MFSymbol 0 '+'))
                     ,IAtom (mkAtom AOrd (MFSymbol 0 '1'))
                     ]

mtest2 = TestCase $ assertEqual "Math" expected (runMathList True "(-1)^n")
  where
{-
$$(-1)^n\showlists$$ ->
\mathopen
.\fam0 (
\mathbin
.\fam2 ^@
\mathord
.\fam0 1
\mathclose
.\fam0 )
^\fam1 n
-}
    expected = Right [IAtom (mkAtom AOpen  (MFSymbol 0 '(')) { atomIsDelimiter = True }
                     ,IAtom (mkAtom ABin   (MFSymbol 0 '\x2212')) -- U+2212: MINUS SIGN
                     ,IAtom (mkAtom AOrd   (MFSymbol 0 '1'))
                     ,IAtom (mkAtom AClose (MFSymbol 0 ')')) { atomIsDelimiter = True
                                                             , atomSuperscript = MFSymbol 1 'n'
                                                             }
                     ]

mtest3 = TestCase $ assertEqual "Math" expected (runMathList True "a<b")
  where
{-
$$a<b\showlists$$ ->
\mathord
.\fam1 a
\mathrel
.\fam1 <
\mathord
.\fam1 b
-}
    expected = Right [IAtom (mkAtom AOrd (MFSymbol 1 'a'))
                     ,IAtom (mkAtom ARel (MFSymbol 1 '<'))
                     ,IAtom (mkAtom AOrd (MFSymbol 1 'b'))
                     ]

tests = TestList [TestLabel "Tokenization 1" ttest1
                 ,TestLabel "Expansion 1" etest1
                 ,TestLabel "Expansion 2" etest2
                 ,TestLabel "Expansion 3" etest3
                 ,TestLabel "Expansion 4" etest4
                 ,TestLabel "Expansion 5" etest5
                 ,TestLabel "Math 1" mtest1
                 ,TestLabel "Math 2" mtest2
                 ,TestLabel "Math 3" mtest3
                 ]

main = runTestTT tests
