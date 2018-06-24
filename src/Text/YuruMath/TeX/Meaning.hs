{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
module Text.YuruMath.TeX.Meaning
  (MessageString(..)
  ,MessageContext(..)
  ,showMessageStringM
  ,throwErrorMessage
  ,Meaning(..)
  ,controlSequence
  ,showCommandName
  ,showToken
  ,can'tUseThisCommandInCurrentMode
  ,invalidPrefix
  ) where
import Text.YuruMath.TeX.Types hiding (_escapechar,_catcodeMap)
import Text.YuruMath.TeX.State (defaultCategoryCodeOf)
import Numeric
import Data.Char
import Data.String
import Data.Semigroup (Semigroup,(<>))
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError,throwError)
import Control.Lens.Getter (use)
import Data.OpenUnion
import TypeFun.Data.List (Delete)
import Data.Typeable (Typeable)

data MessageContext = MessageContext { _escapechar :: !Int
                                     , _catcodeMap :: !(Map.Map Char CatCode)
                                     }

newtype MessageString = MessageString { runMessageString :: MessageContext -> String }

instance IsString MessageString where
  fromString s = MessageString (\_ -> s)

instance Semigroup MessageString where
  x <> y = MessageString (\ctx -> runMessageString x ctx <> runMessageString y ctx)

instance Monoid MessageString where
  mempty = MessageString (\_ -> "")
  mappend = (<>)
  mconcat xs = MessageString (\ctx -> concatMap (\x -> runMessageString x ctx) xs)

showMessageStringM :: (MonadState s m, IsState s) => MessageString -> m String
showMessageStringM m = do
  e <- use (localState . escapechar)
  cm <- use (localState . catcodeMap)
  return $ runMessageString m (MessageContext e cm)

throwErrorMessage :: (MonadState s m, IsState s, MonadError String m) => MessageString -> m a
throwErrorMessage m = do
  e <- use (localState . escapechar)
  cm <- use (localState . catcodeMap)
  throwError $ runMessageString m (MessageContext e cm)

class Meaning a where
  meaningString :: a -> MessageString

can'tUseThisCommandInCurrentMode :: (Meaning a, MonadTeXState s m, MonadError String m) => a -> m b
can'tUseThisCommandInCurrentMode value = do
  name <- showMessageStringM (meaningString value)
  m <- use mode
  let modeStr = case m of
        HorizontalMode -> "horizontal mode"
        RestrictedHorizontalMode -> "restricted horizontal mode"
        VerticalMode -> "vertical mode"
        InternalVerticalMode -> "internal vertical mode"
        MathMode -> "math mode"
        DisplayMathMode -> "display math mode"
  throwError $ "You can't use `" <> name <> "' in " <> modeStr

invalidPrefix :: (Meaning a, MonadTeXState s m, MonadError String m) => String -> a -> m b
invalidPrefix prefixName v = do
  -- You can't use a prefix with ...
  throwErrorMessage $ "You can't use a prefix (" <> controlSequence prefixName <> ") with `" <> meaningString v <> "'"

prependEscapechar :: Int -> String -> String
prependEscapechar e s | isUnicodeScalarValue e = chr e : s
                      | otherwise = s

controlSequence :: String -> MessageString
controlSequence name = MessageString $
  \ctx -> prependEscapechar (_escapechar ctx) name

-- Show a command name, without a space appended. Used by \show
showCommandName :: CommandName -> MessageString
showCommandName (NControlSeq "") = controlSequence "csname" <> controlSequence "endcsname"
showCommandName (NControlSeq name) = controlSequence (T.unpack name)
showCommandName (NActiveChar c) = fromString [c]

-- Show a token, with a space appended if necessary.
showToken :: TeXToken -> MessageString
showToken (TTCommandName (NControlSeq name)) = case T.unpack name of
  [] -> controlSequence "csname" <> controlSequence "endcsname "
  s@[c] -> MessageString $ \ctx ->
    let cc = Map.findWithDefault (defaultCategoryCodeOf c) c (_catcodeMap ctx)
    in prependEscapechar (_escapechar ctx)
       $ if cc == CCLetter
         then [c,' ']
         else s
  s@(_:_) -> MessageString $ \ctx -> prependEscapechar (_escapechar ctx) (s ++ " ")
showToken (TTCommandName (NActiveChar c)) = fromString [c]
showToken (TTCharacter c _) = fromString [c]

instance Meaning (Union '[]) where
  meaningString = typesExhausted

instance (Meaning a, Meaning (Union (Delete a as)), Typeable a) => Meaning (Union (a : as)) where
  meaningString = (meaningString :: a -> MessageString)
                  @> (meaningString :: Union (Delete a as) -> MessageString)

instance (Meaning a, Meaning b) => Meaning (Either a b) where
  meaningString (Left x) = meaningString x
  meaningString (Right y) = meaningString y

instance (Meaning a) => Meaning (Maybe a) where
  meaningString Nothing = "undefined"
  meaningString (Just x) = meaningString x

instance Meaning CommonValue where
  meaningString (Character c cc) = fromString (ccstring ++ [' ',c])
    where ccstring = case cc of
            CCEscape       -> "escape character" -- ?
            CCBeginGroup   -> "begin-group character"
            CCEndGroup     -> "end-group character"
            CCMathShift    -> "math shift character"
            CCAlignmentTab -> "alignment tab character"
            CCEndLine      -> "end line character" -- ?
            CCParam        -> "macro parameter character"
            CCSup          -> "superscript character"
            CCSub          -> "subscript character"
            CCIgnored      -> "ignored character" -- ?
            CCSpace        -> "blank space"
            CCLetter       -> "the letter"
            CCOther        -> "the character"
            CCActive       -> "active character" -- ?
            CCComment      -> "comment character" -- ?
            CCInvalid      -> "invalid character" -- ?
  meaningString (DefinedCharacter c)
    = controlSequence "char" <> fromString ((showChar '"' . showHex (ord c)) "")
  meaningString (DefinedMathCharacter (MathCode c))
    = controlSequence "mathchar" <> fromString ((showChar '"' . showHex c) "")
  meaningString (DefinedMathCharacter c@(UMathCode _))
    = controlSequence "Umathchar"
      <> fromString ((showChar '"' . showHex (fromEnum (mathcharClass c))
                      . showChar '"' . showHex (mathcharFamily c)
                      . showChar '"' . showHex (ord (mathcharSlot c))) "")
  meaningString (IntegerConstant x)
    = fromString ("integer constant " ++ show x)
  meaningString Relax = controlSequence "relax"
  meaningString Endcsname = controlSequence "endcsname"
