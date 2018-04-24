{-# LANGUAGE FlexibleContexts #-}
module Text.YuruMath.TeX.State where
import Text.YuruMath.TeX.Types
import Data.Char (isLetter)
import Data.Text (Text)
import Data.Map (Map)
import Control.Monad.State.Class
import Control.Monad.Error.Class
import qualified Data.Map as Map
import Control.Lens.Cons (_head)
import Control.Lens.Getter (use)
import Control.Lens.Setter (assign,modifying)

initialState :: String -> TeXState a
initialState input = TeXState
                     { _ttInput = input
                     , _ttSpacingState = SSNewLine
                     , _esMaxDepth = 100
                     , _esMaxPendingToken = 100
                     , _esPendingTokenList = []
                     , _localStates = [initialLocalState]
                     , _isMathMode = True
                     , _conditionals = []
                     }
  where initialLocalState = LocalState
                            { _ttCategoryCodeOf = Map.empty
                            , _tsDefinitions = Map.empty
                            , _tsActiveDefinitions = Map.empty
                            , _mathCodes = Map.empty
                            }

defaultCategoryCodeOf :: Char -> CatCode
defaultCategoryCodeOf c = case c of
  '\\' -> CCEscape
  '{'  -> CCBeginGroup
  '}'  -> CCEndGroup
  '$'  -> CCMathShift
  '&'  -> CCAlignmentTab
  '\n' -> CCEndLine
  '#'  -> CCParam
  '^'  -> CCSup
  '_'  -> CCSub
  '\0' -> CCIgnored
  ' '  -> CCSpace
  '\t' -> CCSpace -- LaTeX
  '~'  -> CCActive
  '%'  -> CCComment
  '\127' -> CCInvalid
  c | isLetter c -> CCLetter
  _ -> CCOther

categoryCodeOf :: MonadTeXState a m => Char -> m CatCode
categoryCodeOf c = do
  m <- use (localStates . _head . ttCategoryCodeOf)
  pure (Map.findWithDefault (defaultCategoryCodeOf c) c m)

categoryCodeFn :: MonadTeXState a m => m (Char -> CatCode)
categoryCodeFn = do
  m <- use (localStates . _head . ttCategoryCodeOf)
  pure (\c -> Map.findWithDefault (defaultCategoryCodeOf c) c m)

setCategoryCodeOf :: MonadTeXState a m => Char -> CatCode -> m ()
setCategoryCodeOf c cc = do
  modifying (localStates . _head . ttCategoryCodeOf) (Map.insert c cc)

defaultMathCodeOf :: Char -> MathChar
defaultMathCodeOf c = MathChar MathOrd 0 c

mathCodeOf :: MonadTeXState a m => Char -> m MathChar
mathCodeOf c = do
  m <- use (localStates . _head . mathCodes)
  pure (Map.findWithDefault (defaultMathCodeOf c) c m)

isMathActive :: MonadTeXState a m => Char -> m Bool
isMathActive c = do
  ismm <- use isMathMode
  if ismm
    then do
    mc <- mathCodeOf c
    return (mc == MathActive)
    else return False

enterGroup :: MonadTeXState a m => m ()
enterGroup = do
  modifying localStates (\ss -> head ss : ss)

leaveGroup :: (MonadTeXState a m, MonadError String m) => m ()
leaveGroup = do
  ss <- use localStates
  case ss of
    [] -> throwError "Cannot leave global scope"
    _:ss -> assign localStates ss
