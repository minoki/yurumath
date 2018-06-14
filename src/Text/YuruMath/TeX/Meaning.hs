{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
module Text.YuruMath.TeX.Meaning
  (MeaningContext
  ,Meaning(..)
  ,controlSequence
  ,meaningWithEscapechar
  ,meaningWithCustomEscapechar
  ,meaningWithEscapecharAsInt
  ,meaningWithoutEscapechar
  ) where
import Text.YuruMath.TeX.Types hiding (_escapechar)
import Text.YuruMath.TeX.State
import Numeric
import Data.Char
import Data.OpenUnion
import TypeFun.Data.List (Delete)
import Data.Typeable (Typeable)

newtype MeaningContext = MeaningContext { _escapechar :: Int
                                        }

class Meaning a where
  meaningString :: a -> MeaningContext -> String

meaningWithEscapechar :: (Meaning a) => a -> String
meaningWithEscapechar = meaningWithCustomEscapechar '\\'

meaningWithCustomEscapechar :: (Meaning a) => Char -> a -> String
meaningWithCustomEscapechar c value = meaningString value (MeaningContext (ord c))

meaningWithEscapecharAsInt :: (Meaning a) => Int -> a -> String
meaningWithEscapecharAsInt c value = meaningString value (MeaningContext c)

meaningWithoutEscapechar :: (Meaning a) => a -> String
meaningWithoutEscapechar value = meaningString value (MeaningContext (-1))

controlSequence :: String -> MeaningContext -> String
controlSequence name ctx
  | isUnicodeScalarValue (_escapechar ctx) = chr (_escapechar ctx) : name
  | otherwise = name

instance Meaning (Union '[]) where
  meaningString = typesExhausted

instance (Meaning a, Meaning (Union (Delete a as)), Typeable a) => Meaning (Union (a : as)) where
  meaningString = (meaningString :: a -> MeaningContext -> String)
                  @> (meaningString :: Union (Delete a as) -> MeaningContext -> String)

instance (Meaning a, Meaning b) => Meaning (Either a b) where
  meaningString (Left x) = meaningString x
  meaningString (Right y) = meaningString y

instance Meaning CommonValue where
  meaningString (Character c cc) = pure (ccstring ++ [' ',c])
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
    = (++) <$> controlSequence "char" <*> pure ((showChar '"' . showHex (ord c)) "")
  meaningString (DefinedMathCharacter (MathCode c))
    = (++) <$> controlSequence "mathchar" <*> pure ((showChar '"' . showHex c) "")
  meaningString (DefinedMathCharacter c@(UMathCode _))
    = (++) <$> controlSequence "Umathchar"
      <*> pure ((showChar '"' . showHex (fromEnum (mathcharClass c))
                 . showChar '"' . showHex (mathcharFamily c)
                 . showChar '"' . showHex (ord (mathcharSlot c))) "")
  meaningString (IntegerConstant x)
    = pure ("integer constant " ++ show x)
  meaningString Relax = controlSequence "relax"
  meaningString (Unexpanded _) = controlSequence "relax"
  meaningString (Undefined _) = pure "undefined"
  meaningString Endcsname = controlSequence "endcsname"
