{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Math
import Text.YuruMath.TeX.Math.Postprocess
import Text.YuruMath.TeX.Primitive
import Text.YuruMath.TeX.LaTeX
import Text.YuruMath.Convert.TeXToMML
import Text.YuruMath.Builder.MathML3
import Text.Blaze.Renderer.Pretty as Pretty
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Lens.Setter (modifying)
import TypeFun.Data.List ((:++:))
import Data.Void

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
                        ,latexDefinitions
                        ,m
                        ]
      runMMDGlobal <$> readMathMaterial

main :: IO ()
main = do
  input <- getLine
  case runMathList True input of
    Left err -> putStrLn ("error: " ++ err)
    Right mathList -> do
      putStrLn ("math list: " ++ show mathList)
      let mathList' = textSymbol $ pairOpenClose $ pairSizedOpenClose $ determineBinForm $ doNonScript DisplayStyle $ determineChoice DisplayStyle mathList
      putStrLn ("after some post-processing: " ++ show mathList')
      let mml = math $ mconcat $ toMML DisplayStyle mathList'
      putStrLn $ "MathML: " ++ Pretty.renderMarkup mml
