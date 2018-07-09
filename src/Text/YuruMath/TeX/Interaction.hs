{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Text.YuruMath.TeX.Interaction where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Meaning
import Text.YuruMath.TeX.Expansion
import Text.YuruMath.TeX.Math.State
import Control.Monad.Error.Class
import Control.Lens.Lens (Lens',lens)
import Control.Lens.Setter (modifying)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.OpenUnion (Union,liftUnion)
import TypeFun.Data.List (Elem)

class (IsState state) => IsInteractiveState state where
  outputLines :: Lens' state [String]

data StateWithOutputLines basestate = StateWithOutputLines
  { _basestate :: basestate
  , _outputLines :: [String]
  }

initialStateWithOutputLines :: basestate -> StateWithOutputLines basestate
initialStateWithOutputLines s = StateWithOutputLines s []

instance (IsState basestate) => IsState (StateWithOutputLines basestate) where
  type LocalState (StateWithOutputLines basestate) = LocalState basestate
  commonState = lens _basestate (\s v -> s { _basestate = v }) . commonState

instance (IsMathState basestate) => IsMathState (StateWithOutputLines basestate) where
  currentMathStyle = lens _basestate (\s v -> s { _basestate = v }) . currentMathStyle

instance (IsState basestate) => IsInteractiveState (StateWithOutputLines basestate) where
  outputLines = lens _outputLines (\s v -> s { _outputLines = v })

data InteractionCommand = Imessage
                        | Ishow
                        | Ishowthe
                        | Ishowtokens
                        deriving (Eq,Show,Enum,Bounded)

instance IsPrimitive InteractionCommand where
  primitiveName Imessage = "message"
  primitiveName Ishow = "show"
  primitiveName Ishowthe = "showthe"
  primitiveName Ishowtokens = "showtokens"

instance Meaning InteractionCommand

instance (Monad m, MonadTeXState s m, MonadError String m, IsInteractiveState s, Meaning (Value s)) => DoExecute InteractionCommand m where
  doExecute Imessage = do
    content <- readExpandedGeneralText
    contentS <- showMessageStringM $ mconcat (map showToken content)
    modifying outputLines (++ [contentS])
  doExecute Ishow = do
    t <- required nextUnexpandedToken
    value <- meaningWithoutExpansion t
    line <- showMessageStringM $ case t of
              ETCommandName { etName = name } -> showCommandName name <> "=" <> meaningString value
              ETCharacter {} -> meaningString value
    modifying outputLines (++ ["> " ++ line])
  doExecute Ishowthe = do
    content <- theString "\\showthe"
    str <- showMessageStringM (mconcat $ map showToken content)
    modifying outputLines (++ ["> " ++ str])
  doExecute Ishowtokens = do
    content <- readUnexpandedGeneralText
    contentS <- showMessageStringM $ mconcat (map showToken content)
    modifying outputLines (++ [contentS])
  getQuantity _ = NotQuantity

interactionCommands :: (Elem InteractionCommand set) => Map.Map Text (Either eset (Union set))
interactionCommands = Map.fromList
  [("message", Right $ liftUnion Imessage)
  ,("show", Right $ liftUnion Ishow)
  ,("showthe", Right $ liftUnion Ishowthe)
  ,("showtokens", Right $ liftUnion Ishowtokens)
  ]
