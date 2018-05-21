{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Main where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Tokenizer
import Text.YuruMath.TeX.Expansion
import Text.YuruMath.TeX.Execution
import Text.YuruMath.TeX.Macro
import Text.YuruMath.TeX.Math
import Text.YuruMath.TeX.Math.Style
import Text.YuruMath.TeX.MathData
import Text.YuruMath.TeX.PostMath
import Text.YuruMath.TeX.LaTeX
import Text.YuruMath.Convert.TeXToMML
import Text.YuruMath.Builder.MathML3
import Text.Blaze.Renderer.Pretty as Pretty
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Lens.Setter (modifying)
import Data.OpenUnion

type MathExpandableT = Union '[ConditionalMarkerCommand, CommonExpandable, CommonBoolean, Macro]
type MathValue = Union '[CommonValue,MathStyleSet,MathAtomCommand,MathCommands,CommonExecutable,MacroCommand]
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
                        ,fmap Right macroCommands
                        ,fmap (Right . liftUnion . DefinedMathCharacter) mathCommands
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
      let mathList' = determineChoice DisplayStyle mathList
          mathList'' = determineBinForm mathList'
          mathList''' = pairOpenClose mathList''
          mathList'''' = textSymbol mathList'''
      putStrLn ("after some post-processing: " ++ show mathList'''')
      let mml = math $ mconcat $ toMML DisplayStyle mathList''''
      putStrLn $ "MathML: " ++ Pretty.renderMarkup mml
