{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.State hiding (initialState)
import Text.YuruMath.TeX.Math
import Text.YuruMath.TeX.Math.Postprocess
import Text.YuruMath.TeX.Primitive
import Text.YuruMath.TeX.Interaction
import Text.YuruMath.TeX.LaTeX
import Text.YuruMath.Convert.TeXToMML
import Text.YuruMath.Builder.MathML3
import Text.Blaze.Renderer.Pretty as Pretty
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Lens.Getter (use)
import Control.Lens.Setter (assign,modifying)
import TypeFun.Data.List ((:++:))
import Data.Void
import System.Console.Haskeline

type MathExpandableT = ExpandablePrimitiveList :++: MathExpandableList
type MathValue = NonExpandablePrimitiveList :++: MathNonExpandablePrimitiveList :++: '[InteractionCommand]
type MathLocalState' = MathLocalState MathExpandableT MathValue
type REPLState = StateWithOutputLines (MathState MathLocalState')

initialState :: REPLState
initialState = execState init s0
  where
    s0 = initialStateWithOutputLines $ initialMathState True $ initialStateWithLocalState initialLocalMathState ""
    init = modifying (localState . controlSeqDef)
           $ \m -> mconcat [primitiveDefinitions
                           ,mathDefinitions
                           ,latexDefinitions
                           ,interactionCommands
                           ,m
                           ]

repl :: StateT REPLState (InputT IO) ()
repl = do
  input <- lift $ getInputLine "*"
  case input of
    Nothing -> return ()
    Just "" -> lift (outputStrLn "Press Control-C to exit") >> repl
    Just input -> do
      assign tokenizerState (TokenizerState [input] SSNewLine)
      assign esPendingTokenList []
      assign conditionalStack []
      result <- runExceptT $ runReaderT (runMMDGlobal <$> readMathMaterial) (initialContext DisplayMathMode)
      output <- use outputLines
      lift $ forM_ output outputStrLn
      assign outputLines []
      case result :: Either String (MathList Void) of
        Left err -> lift (outputStrLn ("error: " ++ err)) >> repl
        Right [] -> repl
        Right mathList -> do
          let mathList' = textSymbol $ pairOpenClose $ pairSizedOpenClose $ determineBinForm $ doNonScript DisplayStyle mathList
          let mml = math $ mconcat $ toMML DisplayStyle mathList'
          lift $ outputStrLn $ "MathML: " ++ Pretty.renderMarkup mml
          repl

main :: IO ()
main = runInputT defaultSettings (evalStateT repl initialState)
