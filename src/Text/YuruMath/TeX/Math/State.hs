{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Text.YuruMath.TeX.Math.State
  (IsMathLocalState(..)
  ,IsMathState(..)
  ,MathLocalState
  ,MathState
  ,initialLocalMathState
  ,initialMathState
  ) where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Math.List
import Control.Lens.Lens
import Data.OpenUnion (Union)
import TypeFun.Data.List (Elem)

class (IsLocalState localstate) => IsMathLocalState localstate where
  famParam          :: Lens' localstate Int
  currentVariant    :: Lens' localstate MathVariant
  currentSymbolMode :: Lens' localstate SymbolMode

class (IsState state, IsMathLocalState (LocalState state)) => IsMathState state where
  currentMathStyle :: Lens' state (Maybe MathStyle)

data MathLocalState ecommand value
  = MathLocalState
    { _mCommonLocalState :: !(CommonLocalState ecommand value)
    , _famParam :: !Int
    -- , _textfontMap :: Map.Map Word Font
    -- , _scriptfontMap :: Map.Map Word Font
    -- , _scriptscriptfontMap :: Map.Map Word Font
    , _currentVariant :: !MathVariant
    , _currentSymbolMode :: !SymbolMode
    }

data MathState localstate
  = MathState
    { _mCommonState :: !(CommonState localstate)
    , _currentMathStyle :: !(Maybe MathStyle) -- math style is not affected by \begingroup..\endgroup
    }

initialLocalMathState :: MathLocalState e v
initialLocalMathState
  = MathLocalState { _mCommonLocalState = initialLocalState
                   , _famParam = -1
                   , _currentVariant = MVItalic
                   , _currentSymbolMode = SMSymbol
                   }

initialMathState :: Bool -> CommonState localstate -> MathState localstate
initialMathState !isDisplay !commonState
  = MathState { _mCommonState = commonState
              , _currentMathStyle = Just $ if isDisplay then DisplayStyle else TextStyle
              }

instance (IsExpandable (Union ecommand), IsNValue (Union value), Elem CommonValue value) => IsLocalState (MathLocalState ecommand value) where
  type ExpandableSetT (MathLocalState ecommand value) = ecommand
  type NValueSetT (MathLocalState ecommand value) = value
  commonLocalState = lens _mCommonLocalState (\s v -> s { _mCommonLocalState = v })

instance (IsExpandable (Union ecommand), IsNValue (Union value), Elem CommonValue value) => IsMathLocalState (MathLocalState ecommand value) where
  famParam          = lens _famParam          (\s v -> s { _famParam = v })
  currentVariant    = lens _currentVariant    (\s v -> s { _currentVariant = v })
  currentSymbolMode = lens _currentSymbolMode (\s v -> s { _currentSymbolMode = v })

instance (IsLocalState localstate) => IsState (MathState localstate) where
  type LocalState (MathState localstate) = localstate
  commonState = lens _mCommonState (\s v -> s { _mCommonState = v })

instance (IsMathLocalState localstate) => IsMathState (MathState localstate) where
  currentMathStyle = lens _currentMathStyle (\s v -> s { _currentMathStyle = v })
