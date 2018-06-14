{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Tokenizer
import Text.YuruMath.TeX.Expansion
import Text.YuruMath.TeX.Math
import Text.YuruMath.TeX.Math.List
import Text.YuruMath.TeX.PostMath
import Text.YuruMath.TeX.Primitive
import Text.YuruMath.TeX.LaTeX
import Text.YuruMath.Convert.TeXToMML
import Text.YuruMath.Builder.MathML3
import Text.Blaze.Renderer.Pretty as Pretty
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Lens.Setter (modifying)
import Data.OpenUnion
import TypeFun.Data.List ((:++:))

type MathExpandableT = Union (ExpandablePrimitiveList :++: MathExpandableList)
type MathValue = Union (NonExpandablePrimitiveList :++: MathNonExpandablePrimitiveList)
type MathLocalState' = MathLocalState MathExpandableT MathValue
runMathList :: Bool -> String -> Either String MathList
runMathList !isDisplay input = runExcept $ evalStateT action (initialMathState isDisplay $ initialStateWithLocalState initialLocalMathState input)
  where
    action :: StateT (MathState MathLocalState') (Except String) MathList
    action = do
      modifying (localState . tsDefinitions)
        $ \m -> mconcat [primitiveDefinitions
                        ,mathDefinitions
                        ,latexDefinitions
                        ,m
                        ]
      runMMDGlobal <$> readMathMaterial defaultMathMaterialContext

main :: IO ()
main = do
  input <- getLine
  case runMathList True input of
    Left err -> putStrLn ("error: " ++ err)
    Right mathList -> do
      putStrLn ("math list: " ++ show mathList)
      let mathList' = textSymbol $ pairOpenClose $ determineBinForm $ doNonScript DisplayStyle $ determineChoice DisplayStyle mathList
      putStrLn ("after some post-processing: " ++ show mathList')
      let mml = math $ mconcat $ toMML DisplayStyle mathList'
      putStrLn $ "MathML: " ++ Pretty.renderMarkup mml
