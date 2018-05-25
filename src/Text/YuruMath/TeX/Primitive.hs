{-# LANGUAGE DataKinds #-}
module Text.YuruMath.TeX.Primitive (ExpandablePrimitiveList,NonExpandablePrimitiveList,primitiveDefinitions) where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Expansion (ConditionalMarkerCommand, CommonExpandable, CommonBoolean, expandableDefinitions)
import Text.YuruMath.TeX.Execution (CommonExecutable, executableDefinitions)
import Text.YuruMath.TeX.Macro     (Macro, MacroCommand, macroCommands)
import Text.YuruMath.TeX.Expr      (ExprCommand, exprCommands)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.OpenUnion (Union)
import TypeFun.Data.List (SubList)

type ExpandablePrimitiveList = '[ConditionalMarkerCommand, CommonExpandable, CommonBoolean, Macro]
type NonExpandablePrimitiveList = '[CommonValue, CommonExecutable, MacroCommand, ExprCommand]

primitiveDefinitions :: (SubList ExpandablePrimitiveList eset, SubList NonExpandablePrimitiveList vset) => Map.Map Text (Either (Union eset) (Union vset))
primitiveDefinitions
  = fmap Left expandableDefinitions
  <> fmap Right executableDefinitions
  <> fmap Right macroCommands -- includes \newcommand...
  <> fmap Right exprCommands

-- Math commands?
