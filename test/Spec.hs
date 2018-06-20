{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
import Test.HUnit
import Data.Semigroup
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.State
import qualified Text.YuruMath.TeX.Tokenizer as Tok
import Text.YuruMath.TeX.Expansion
import Text.YuruMath.TeX.Primitive
import Text.YuruMath.TeX.Interaction
import Text.YuruMath.TeX.Math
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Lens.Getter (use)
import Control.Lens.Setter (modifying)
import qualified Data.Text as T
import Data.OpenUnion
import TypeFun.Data.List ((:++:))
import Data.Void

defineBuiltins :: (MonadTeXState s m, MonadError String m, ValueSet s ~ NonExpandablePrimitiveList, ExpandableSet s ~ ExpandablePrimitiveList) => m ()
defineBuiltins = do
  modifying (localState . controlSeqDef)
    $ \s -> primitiveDefinitions <> s

tokenizeAll :: (MonadTeXState s m, MonadError String m, ValueSet s ~ '[CommonValue], ExpandableSet s ~ ExpandablePrimitiveList) => m [TeXToken]
tokenizeAll = do
  t <- Tok.nextToken
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
    Just (_,v) -> (v:) <$> expandAll

expandAllString :: String -> Either String [Value (CommonState (CommonLocalState ExpandablePrimitiveList NonExpandablePrimitiveList))]
expandAllString input = runExcept (evalStateT (defineBuiltins >> expandAll) (initialState input))

type MathExpandableT = ExpandablePrimitiveList :++: MathExpandableList
type MathValue = NonExpandablePrimitiveList :++: MathNonExpandablePrimitiveList
type MathLocalState' = MathLocalState MathExpandableT MathValue
runMathList :: Bool -> String -> Either String (MathList Void)
runMathList !isDisplay input = runExcept $ evalStateT action (initialMathState isDisplay $ initialStateWithLocalState initialLocalMathState input)
  where
    action :: StateT (MathState MathLocalState') (Except String) (MathList Void)
    action = do
      modifying (localState . controlSeqDef)
        $ \m -> mconcat [primitiveDefinitions
                        ,mathDefinitions
                        ,m
                        ]
      runMMDGlobal <$> readMathMaterial

runMessage :: String -> Either String [String]
runMessage input = runExcept $ evalStateT (action >> use outputLines) initialState
  where
    initialState = initialStateWithOutputLines $ initialMathState True $ initialStateWithLocalState initialLocalMathState input
    action :: StateT (StateWithOutputLines (MathState (MathLocalState MathExpandableT (MathValue :++: '[InteractionCommand]))))
              (Except String) (MathList Void)
    action = do
      modifying (localState . controlSeqDef)
        $ \m -> mconcat [primitiveDefinitions
                        ,mathDefinitions
                        ,interactionCommands
                        ,m
                        ]
      runMMDGlobal <$> readMathMaterial

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
    expected = Right $ map liftUnion
      [Character 'Y' CCLetter
      ]

etest2 = TestCase $ assertEqual "Expand" expected (expandAllString "\\iftrue \\csname fo\\else \\fi o\\endcsname")
  where
    expected = Right $ map liftUnion
      [Relax
      ]

etest3 = TestCase $ assertEqual "Expand" expected (expandAllString "\\romannumeral'123 ")
  where
    expected = Right $ map liftUnion
      [Character 'l' CCOther
      ,Character 'x' CCOther
      ,Character 'x' CCOther
      ,Character 'x' CCOther
      ,Character 'i' CCOther
      ,Character 'i' CCOther
      ,Character 'i' CCOther
      ]

etest4 = TestCase $ assertEqual "Expand" expected (expandAllString "\\iftrue\\ifnum1<0\\else x\\fi\\fi")
  where
    expected = Right $ map liftUnion
      [Character 'x' CCLetter
      ]

etest5 = TestCase $ assertEqual "Expand" expected (expandAllString "\\iftrue\\number\"1\\else\\fi 0 ")
  where
    expected = Right $ map liftUnion
      [Character '1' CCOther
      ,Character '6' CCOther
      ]

etest6 = TestCase $ assertEqual "\\numexpr" expected (expandAllString "\\number\\numexpr(10+2*3)/  ( 3+6 ) * 4+-14/\\numexpr-2-3\\relax\\relax")
  where
    expected = Right $ map liftUnion
      [Character '1' CCOther
      ,Character '1' CCOther
      ]

etest7 = TestCase $ assertEqual "\\dimexpr" expected (expandAllString "\\the\\dimexpr(10pt+2in*3)/  ( 3+6 ) * 4+-14cm/\\numexpr-2-3\\relax\\relax")
  where
    expected = Right $ map liftUnion
      [Character '2' CCOther
      ,Character '7' CCOther
      ,Character '6' CCOther
      ,Character '.' CCOther
      ,Character '8' CCOther
      ,Character '3' CCOther
      ,Character '2' CCOther
      ,Character '1' CCOther
      ,Character '4' CCOther
      ,Character 'p' CCOther
      ,Character 't' CCOther
      ]

etest10 = TestCase $ assertEqual "\\unexpanded" expected $ runMessage $ concat
          ["\\def\\foo{bar}\\message{\\foo\\unexpanded{\\foo}}"
          ,"\\edef\\bar{\\foo\\unexpanded{\\foo}}\\def\\baz{bar\\foo}\\message{\\ifx\\bar\\baz Y\\else N\\fi}"
          ,"\\edef\\bar{\\number\"1\\unexpanded{\\number\"0}}\\def\\baz{16}\\message{\\ifx\\bar\\baz Y\\else N\\fi}"
          ,"\\edef\\bar{\\number\"1 \\unexpanded{\\number\"0}}\\def\\baz{1\\number\"0}\\message{\\ifx\\bar\\baz Y\\else N\\fi}"
          ]
  where
    expected = Right ["bar\\foo "
                     ,"Y"
                     ,"Y"
                     ,"Y"
                     ]

etest11 = TestCase $ assertEqual "\\detokenize" expected (expandAllString "\\detokenize{\\hell\\o\\ \\world\\!}")
  where
    expected = Right $ map liftUnion
      [Character '\\' CCOther
      ,Character 'h' CCOther
      ,Character 'e' CCOther
      ,Character 'l' CCOther
      ,Character 'l' CCOther
      ,Character ' ' CCSpace
      ,Character '\\' CCOther
      ,Character 'o' CCOther
      ,Character ' ' CCSpace
      ,Character '\\' CCOther
      ,Character ' ' CCSpace
      ,Character '\\' CCOther
      ,Character 'w' CCOther
      ,Character 'o' CCOther
      ,Character 'r' CCOther
      ,Character 'l' CCOther
      ,Character 'd' CCOther
      ,Character ' ' CCSpace
      ,Character '\\' CCOther
      ,Character '!' CCOther
      ]

etest12 = TestCase $ assertEqual "\\detokenize" expected $ runMessage $ concat
          ["\\escapechar=-1 \\message{\\detokenize{\\hell\\o\\ \\world\\!}}"
          ]
  where
    expected = Right ["hell o  world !"
                     ]

etest13 = TestCase $ assertEqual "\\strcmp" expected $ runMessage $ concat
          ["\\message{\\strcmp{a}{z}}"
          ,"\\def\\z{a} \\message{\\strcmp{a}{\\z}}"
          ,"\\def\\a{z} \\message{\\strcmp{\\a}{\\z}}"
          ,"\\edef\\b{\\string b} \\message{\\strcmp{b}{\\b}}"
          ,"\\escapechar=-1 \\message{\\strcmp{\\relax}{relax}}"
          ,"\\message{\\strcmp{\\relax}{relax }}"
          ]
  where
    expected = Right ["-1"
                     ,"0"
                     ,"1"
                     ,"0"
                     ,"1"
                     ,"0"
                     ]

macrotest1 = TestCase $ assertEqual "Macro 1" expected (runMathList True "\\edef\\foo{\\number\"FF}\\def\\bar{255}\\ifx\\foo\\bar Y\\else N\\fi")
  where
    expected = Right $ map (IAtom . mkAtom AOrd . MFSymbol 1 MVItalic SMSymbol . T.singleton) "Y"

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
    expected = Right [IAtom (mkAtom AOrd (MFSymbol 0 MVItalic SMSymbol "1"))
                     ,IAtom (mkAtom ABin (MFSymbol 0 MVNormal SMSymbol "+"))
                     ,IAtom (mkAtom AOrd (MFSymbol 0 MVItalic SMSymbol "1"))
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
    expected = Right [IAtom (mkAtom AOpen  (MFSymbol 0 MVNormal SMSymbol "(")) { atomIsDelimiter = True }
                     ,IAtom (mkAtom ABin   (MFSymbol 0 MVNormal SMSymbol "\x2212")) -- U+2212: MINUS SIGN
                     ,IAtom (mkAtom AOrd   (MFSymbol 0 MVItalic SMSymbol "1"))
                     ,IAtom (mkAtom AClose (MFSymbol 0 MVNormal SMSymbol ")"))
                      { atomIsDelimiter = True
                      , atomSuperscript = MFSymbol 1 MVItalic SMSymbol "n"
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
    expected = Right [IAtom (mkAtom AOrd (MFSymbol 1 MVItalic SMSymbol "a"))
                     ,IAtom (mkAtom ARel (MFSymbol 0 MVNormal SMSymbol "<"))
                     ,IAtom (mkAtom AOrd (MFSymbol 1 MVItalic SMSymbol "b"))
                     ]

mtest4 = TestCase $ assertEqual "Math" expected (runMathList True "\\newcommand\\frac[2]{\\Ustack{{#1} \\over #2}}\\frac12+\\frac{a}{b+c}")
  where
{-
*$${1\over2}+{a\over b+c}\showlists$$ ->
\mathord
.\fraction, thickness = default
.\\mathord
.\.\fam0 1
./\mathord
./.\fam0 2
\mathbin
.\fam0 +
\mathord
.\fraction, thickness = default
.\\mathord
.\.\fam1 a
./\mathord
./.\fam1 b
./\mathbin
./.\fam0 +
./\mathord
./.\fam1 c
-}
    group l = IAtom (mkAtom AOrd (MFSubList l))
    expected = Right
      [group [IGenFrac (WithoutDelims GFOver)
               [group [IAtom (mkAtom AOrd (MFSymbol 0 MVItalic SMSymbol "1"))]]
               [IAtom (mkAtom AOrd (MFSymbol 0 MVItalic SMSymbol "2"))]
             ]
      ,IAtom (mkAtom ABin (MFSymbol 0 MVNormal SMSymbol "+"))
      ,group [IGenFrac (WithoutDelims GFOver)
               [group [IAtom (mkAtom AOrd (MFSymbol 1 MVItalic SMSymbol "a"))]]
               [IAtom (mkAtom AOrd (MFSymbol 1 MVItalic SMSymbol "b"))
               ,IAtom (mkAtom ABin (MFSymbol 0 MVNormal SMSymbol "+"))
               ,IAtom (mkAtom AOrd (MFSymbol 1 MVItalic SMSymbol "c"))
               ]
             ]
      ]

arithtest1 = TestCase $ assertEqual "Arithmetic" expected (runMathList True "\\countdef\\foo=0 \\foo=100000 \\multiply\\foo by 2000000 \\the\\foo")
  where
    expected = Right $ map (IAtom . mkAtom AOrd . MFSymbol 0 MVItalic SMSymbol . T.singleton) "200000000000"

looptest1 = TestCase $ assertEqual "Prevent Infinite Loop" expected (runMathList True "\\newcommand\\loop\\loop \\loop")
  where
    expected = Left "recursion too deep"

looptest2 = TestCase $ assertEqual "Prevent Infinite Loop" expected (runMathList True "\\newcommand\\loop{\\loop\\loop\\loop} \\loop")
  where
    expected = Left "token list too long"

looptest3 = TestCase $ assertEqual "Prevent Infinite Loop (uppercase)" expected (runMathList True "\\newcommand\\loop{\\uppercase{\\loop}} \\loop")
  where
    expected = Left "recursion too deep"

looptest4 = TestCase $ assertEqual "Prevent Infinite Loop (math active)" expected (runMathList True "\\newcommand\\foo{'}{\\catcode`\\'=13 \\global\\let'=\\foo} '")
  where
    expected = Left "recursion too deep"

tests = TestList [TestLabel "Tokenization 1" ttest1
                 ,TestLabel "Expansion 1" etest1
                 ,TestLabel "Expansion 2" etest2
                 ,TestLabel "Expansion 3" etest3
                 ,TestLabel "Expansion 4" etest4
                 ,TestLabel "Expansion 5" etest5
                 ,TestLabel "Expansion 6 (\\numexpr)" etest6
                 ,TestLabel "Expansion 7 (\\dimexpr)" etest7
                 --,TestLabel "Expansion 8 (\\glueexpr)" etest8
                 --,TestLabel "Expansion 9 (\\muexpr)" etest9
                 ,TestLabel "Expansion 10 (\\unexpanded)" etest10
                 ,TestLabel "Expansion 11 (\\detokenize)" etest11
                 ,TestLabel "Expansion 12 (\\detokenize)" etest12
                 ,TestLabel "Expansion 13 (\\strcmp)" etest13
                 ,TestLabel "Macro 1" macrotest1
                 ,TestLabel "Math 1" mtest1
                 ,TestLabel "Math 2" mtest2
                 ,TestLabel "Math 3" mtest3
                 ,TestLabel "Math 4" mtest4
                 ,TestLabel "Arithmetic 1" arithtest1
                 ,TestLabel "Loop 1" looptest1
                 ,TestLabel "Loop 2" looptest2
                 ,TestLabel "Loop 3" looptest3
                 ,TestLabel "Loop 4" looptest4
                 ]

main = runTestTT tests
