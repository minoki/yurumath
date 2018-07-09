{-# LANGUAGE DataKinds #-}
module Text.YuruMath.TeX.Primitive (ExpandablePrimitiveList,NonExpandablePrimitiveList,primitiveDefinitions) where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Primitive.Expandable
import Text.YuruMath.TeX.Primitive.Register
import Text.YuruMath.TeX.Execution (CommonExecutable, executableDefinitions)
import Text.YuruMath.TeX.Macro     (Macro, MacroCommand, macroCommands)
import Text.YuruMath.TeX.Expr      (ExprCommand, exprCommands)
import Text.YuruMath.TeX.Typeset   (TypesetCommand, BoxCommand, typesetCommands)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.OpenUnion (Union)
import TypeFun.Data.List (SubList)

type ExpandablePrimitiveList = '[ConditionalMarkerCommand, CommonExpandable, CommonBoolean, Macro]
type NonExpandablePrimitiveList = '[CommonValue, CommonExecutable, RegisterCommand, MacroCommand, ExprCommand, TypesetCommand, BoxCommand, CountReg, DimenReg, SkipReg, MuskipReg, ToksReg]

primitiveDefinitions :: (SubList ExpandablePrimitiveList eset, SubList NonExpandablePrimitiveList vset) => Map.Map Text (Either (Union eset) (Union vset))
primitiveDefinitions
  = fmap Left expandableDefinitions
  <> fmap Right executableDefinitions
  <> fmap Right registerCommands
  <> fmap Right macroCommands -- includes \newcommand...
  <> fmap Right exprCommands
  <> fmap Right typesetCommands

-- Math commands?
