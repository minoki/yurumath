{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Text.YuruMath.TeX.Interaction where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Meaning
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Expansion
import Text.YuruMath.TeX.Math.State
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Control.Lens.Lens (Lens',lens)
import Control.Lens.Getter (use)
import Control.Lens.Setter (modifying)
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
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

prependEscapechar :: (MonadState s m, IsState s) => String -> m String
prependEscapechar s = do
  ec <- use (localState . escapechar)
  return $ if isUnicodeScalarValue ec
           then chr ec : s
           else s

showToken :: (MonadState s m, IsState s) => TeXToken -> m String
showToken (TTCommandName (NControlSeq name)) = case T.unpack name of
  [] -> do x <- prependEscapechar "csname"
           y <- prependEscapechar "endcsname "
           return (x ++ y)
  s@[c] -> do cc <- categoryCodeOf c
              if cc == CCLetter
                then prependEscapechar [c,' ']
                else prependEscapechar s
  s@(_:_) -> prependEscapechar (s ++ " ")
showToken (TTCommandName (NActiveChar c)) = return [c]
showToken (TTCharacter c _) = return [c]

showCommandName :: (MonadState s m, IsState s) => CommandName -> m String
showCommandName (NControlSeq "") = (++) <$> prependEscapechar "csname" <*> prependEscapechar "endcsname"
showCommandName (NControlSeq name) = prependEscapechar (T.unpack name)
showCommandName (NActiveChar c) = return [c]

instance (Monad m, MonadTeXState s m, MonadError String m, IsInteractiveState s, Meaning (Expandable s), Meaning (Value s)) => DoExecute InteractionCommand m where
  doExecute Imessage = do
    readFillerAndLBrace
    content <- edefReadUntilEndGroup
    contentS <- concat <$> mapM showToken content
    modifying outputLines (++ [contentS])
  doExecute Ishow = do
    t <- required nextEToken
    value <- meaningWithoutExpansion t
    ec <- use (localState . escapechar)
    let meaning = meaningWithEscapecharAsInt ec value
    line <- case t of
              ETCommandName { etName = name } -> (++ "=" ++ meaning) <$> showCommandName name
              ETCharacter {} -> return meaning
    modifying outputLines (++ ["> " ++ line])
  doExecute Ishowthe = do
    str <- theString "\\showthe"
    modifying outputLines (++ ["> " ++ str])
  doExecute Ishowtokens = do
    readFillerAndLBrace
    content <- readUntilEndGroup LongParam
    contentS <- concat <$> mapM showToken content
    modifying outputLines (++ [contentS])
  getQuantity _ = NotQuantity

interactionCommands :: (Elem InteractionCommand set) => Map.Map Text (Either eset (Union set))
interactionCommands = Map.fromList
  [("message", Right $ liftUnion Imessage)
  ,("show", Right $ liftUnion Ishow)
  ,("showthe", Right $ liftUnion Ishowthe)
  ,("showtokens", Right $ liftUnion Ishowtokens)
  ]
