{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.YuruMath.TeX.Execution where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Tokenizer
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Expansion
import Data.Char
import Data.Text (Text)
import Control.Monad
import Control.Monad.Error.Class
import qualified Data.Map.Strict as Map
import Control.Lens.Getter (view,use,uses)
import Control.Lens.Setter (assign,modifying)

letCommand :: (MonadTeXState a m, MonadError String m) => m ()
letCommand = do
  name <- readCommandName
  readEquals
  t <- required nextEToken
  t <- case t of
    ETCharacter _ CCSpace -> required nextEToken -- one optional space
    _ -> return t
  v <- meaning t
  assign (localState . definitionAt name) v

uppercaseCommand :: (MonadTeXState a m, MonadError String m) => m ()
uppercaseCommand = do
  text <- readGeneralText
  toUpper <- ucCodeFn
  let makeUpper (TTCharacter c cc) | d <- toUpper c, d /= '\0' = TTCharacter d cc
      makeUpper t = t
  let text' = map makeUpper text
  unreadETokens 0 (map toEToken text')

lowercaseCommand :: (MonadTeXState a m, MonadError String m) => m ()
lowercaseCommand = do
  text <- readGeneralText
  toLower <- lcCodeFn
  let makeLower (TTCharacter c cc) | d <- toLower c, d /= '\0' = TTCharacter d cc
      makeLower t = t
  let text' = map makeLower text
  unreadETokens 0 (map toEToken text')

chardefCommand :: (MonadTeXState a m, MonadError String m) => m ()
chardefCommand = do
  name <- readCommandName
  t <- required nextEToken
  if t /= ETCharacter '=' CCOther
    then unreadETokens 0 [t]
    else return ()
  v <- readNumber
  -- Neither LuaTeX nor XeTeX seems to forbid surrogate codes ("D800-"DFFF)
  when (v < 0 || v >= 0x110000 || (0xD800 <= v && v < 0xE000))
    $ throwError $ "Bad character code (" ++ show v ++ ")"
  let w = DefinedCharacter (chr (fromInteger v))
  assign (localState . definitionAt name) (Right w)

-- \mathchardef
-- \Umathchardef \x = 7+FF+10FFFF
-- \Umathcharnumdef
-- \countdef, \dimendef, \muskipdef, \skipdef, \toksdef

defCommand :: (MonadTeXState a m, MonadError String m) => m ()
defCommand = do
  cs <- required nextEToken
  throwError "\\def: not implemented yet"
