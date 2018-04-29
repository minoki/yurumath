{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
import Test.HUnit
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Tokenizer
import Text.YuruMath.TeX.Expansion
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Lens.Cons (_head)
import Control.Lens.Setter (modifying)
import qualified Data.Map.Strict as Map

defineBuiltins :: (MonadState (TeXState a) m, MonadError String m) => m ()
defineBuiltins = do
  modifying (localStates . _head . tsDefinitions)
    $ mappend (Map.fromList [("expandafter",ecmd expandafterCommand)
                            ,("noexpand",ecmd noexpandCommand)
                            ,("csname",ecmd csnameCommand)
                            ,("string",ecmd stringCommand)
                            ,("number",ecmd numberCommand)
                            ,("romannumeral",ecmd romannumeralCommand)
                            ,("endcsname",Right Endcsname)
                            ,("else",Left (ExpandableValue Eelse))
                            ,("fi",Left (ExpandableValue Efi))
                            ,("or",Left (ExpandableValue Eor))
                            ,("ifcase",Left (ExpandableCommand IfCase))
                            ,("iftrue",Left (ExpandableCommand (BooleanConditionalCommand iftrueCommand)))
                            ,("iffalse",Left (ExpandableCommand (BooleanConditionalCommand iffalseCommand)))
                            ,("if",Left (ExpandableCommand (BooleanConditionalCommand ifCommand)))
                            ,("ifcat",Left (ExpandableCommand (BooleanConditionalCommand ifcatCommand)))
                            ,("ifx",Left (ExpandableCommand (BooleanConditionalCommand ifxCommand)))
                            ,("ifnum",Left (ExpandableCommand (BooleanConditionalCommand ifnumCommand)))
                            ,("ifodd",Left (ExpandableCommand (BooleanConditionalCommand ifoddCommand)))
                            ,("ifdefined",Left (ExpandableCommand (BooleanConditionalCommand ifdefinedCommand)))
                            ,("ifcsname",Left (ExpandableCommand (BooleanConditionalCommand ifcsnameCommand)))
                            ,("unless",ecmd unlessCommand)
                            ,("unexpanded",ecmd unexpandedCommand)
                            ])
  where
    ecmd :: (forall m. (MonadState (TeXState a) m, MonadError String m) => m [ExpansionToken]) -> Either (Expandable a) v
    ecmd c = Left (ExpandableCommand (MkExpandableCommand c))

tokenizeAll :: (MonadState (TeXState a) m, MonadError String m) => m [TeXToken]
tokenizeAll = do
  t <- nextToken
  case t of
    Nothing -> return []
    Just t -> (t:) <$> tokenizeAll

tokenizeAllString :: String -> Either String [TeXToken]
tokenizeAllString input = runExcept (evalStateT tokenizeAll (initialState input))

expandAll :: (MonadState (TeXState a) m, MonadError String m) => m [Value a]
expandAll = do
  t <- evalToValue
  case t of
    Nothing -> return []
    Just v -> (v:) <$> expandAll

expandAllString :: String -> Either String [Value ()]
expandAllString input = runExcept (evalStateT (defineBuiltins >> expandAll) (initialState input))

ttest1 = TestCase $ assertEqual "Tokenize \\foo bar \\ 1\\23" expected (tokenizeAllString "\\foo bar \\ 1\\23")
  where
    expected = Right [TTControlSeq "foo"
                     ,TTCharacter 'b' CCLetter
                     ,TTCharacter 'a' CCLetter
                     ,TTCharacter 'r' CCLetter
                     ,TTCharacter ' ' CCSpace
                     ,TTControlSeq " "
                     ,TTCharacter '1' CCOther
                     ,TTControlSeq "2"
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

tests = TestList [TestLabel "Tokenization 1" ttest1
                 ,TestLabel "Expansion 1" etest1
                 ,TestLabel "Expansion 2" etest2
                 ,TestLabel "Expansion 3" etest3
                 ,TestLabel "Expansion 4" etest4
                 ,TestLabel "Expansion 5" etest5
                 ]

main = runTestTT tests
