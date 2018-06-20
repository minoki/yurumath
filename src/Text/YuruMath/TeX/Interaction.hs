{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
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
                        deriving (Eq,Show)

instance Meaning InteractionCommand where
  meaningString Imessage = controlSequence "message"
  meaningString Ishow = controlSequence "show"
  meaningString Ishowthe = controlSequence "showthe"
  meaningString Ishowtokens = controlSequence "showtokens"

instance (Monad m, MonadTeXState s m, MonadError String m, IsInteractiveState s, Meaning (Expandable s), Meaning (Value s)) => DoExecute InteractionCommand m where
  doExecute Imessage = do
    readFillerAndLBrace
    content <- edefReadUntilEndGroup
    contentS <- showMessageStringM $ mconcat (map showToken content)
    modifying outputLines (++ [contentS])
  doExecute Ishow = do
    t <- required nextEToken
    value <- meaningWithoutExpansion t
    line <- showMessageStringM $ case t of
              ETCommandName { etName = name } -> showCommandName name <> "=" <> meaningString value
              ETCharacter {} -> meaningString value
    modifying outputLines (++ ["> " ++ line])
  doExecute Ishowthe = do
    str <- theString "\\showthe"
    modifying outputLines (++ ["> " ++ str])
  doExecute Ishowtokens = do
    readFillerAndLBrace
    content <- readUntilEndGroup LongParam
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
