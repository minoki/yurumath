{-# LANGUAGE OverloadedStrings #-}
module Text.YuruMath.TeX.Math where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Tokenizer
import Text.YuruMath.TeX.State
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Identity

makeCramped :: MathStyle -> MathStyle
makeCramped MathDisplayStyle = MathDisplayStyleCramped
makeCramped MathTextStyle = MathTextStyleCramped
makeCramped MathScriptStyle = MathScriptStyleCramped
makeCramped MathScriptScriptStyle = MathScriptScriptStyleCramped
makeCramped style = style

superscriptStyle :: MathStyle -> MathStyle
superscriptStyle MathDisplayStyle = MathScriptStyle
superscriptStyle MathDisplayStyleCramped = MathScriptStyleCramped
superscriptStyle MathTextStyle = MathScriptStyle
superscriptStyle MathTextStyleCramped = MathScriptStyleCramped
superscriptStyle MathScriptStyle = MathScriptScriptStyle
superscriptStyle MathScriptStyleCramped = MathScriptScriptStyleCramped
superscriptStyle scriptscriptstyle = scriptscriptstyle

subscriptStyle :: MathStyle -> MathStyle
subscriptStyle = makeCramped . superscriptStyle


--data MathFamily = FamilyRoman -- (0)
--                | FamilyMathItalic -- (1)

data DelimiterVariant = DelimiterVariant !MathFamily !Char
data DelimiterCode = DelimiterCode {-small variant-} !DelimiterVariant {-large variant-} !DelimiterVariant
-- "uvvxyy (27-bit number)
-- uvv: the small variant of the delimiter
-- xyy: the large variant
-- u, x: the font families of the variant
-- vv, yy: locations

-- Radicals

-- Math accents
-- "xyzz
-- y: the family
-- zz: the character

-- mathClassOf :: Char -> MathClass
-- Map.Map Char MathClass

data Formula = Identifier !Text
             | Numeral !Text
             | Operator !Text -- ?
             | Subformula !MathClass [Formula]
             | Frac Formula Formula
             | Superscript Formula Formula
             | Subscript Formula Formula
             | SubSup Formula Formula Formula
             | Underscript Formula Formula
             | Overscript Formula Formula
             | UnderOver Formula Formula Formula

mathCommands :: Map.Map Text MathChar
mathCommands = Map.fromList
  -- Ordinary Symbols
  [("infty",MathChar MathOrd 0 '\x221E')
  ,("forall",MathChar MathOrd 0 '\x2200')
  ,("exists",MathChar MathOrd 0 '\x2203')
  ,("emptyset",MathChar MathOrd 0 '\x2205')

  -- Large Operators
  ,("sum",MathChar MathOp 0 '\x2211')
  ,("int",MathChar MathOp 0 '\x222B') -- nolimits
  ,("prod",MathChar MathOp 0 '\x220F')

  -- Binary Symbols
  ,("cap",MathChar MathBin 0 '\x2229')
  ,("cup",MathChar MathBin 0 '\x222A')
  ,("pm",MathChar MathBin 0 '\x00B1')
  ,("mp",MathChar MathBin 0 '\x2213')
  ,("times",MathChar MathBin 0 '\x00D7')

  -- Relations
  ,("mid",MathChar MathRel 0 '|') -- ?
  ,("leftarrow",MathChar MathRel 0 '\x2190')
  ,("rightarrow",MathChar MathRel 0 '\x2192')
  ,("in",MathChar MathRel 0 '\x2208')
  ,("ni",MathChar MathRel 0 '\x220B')
  ,("leq",MathChar MathRel 0 '\x2264')
  ,("le",MathChar MathRel 0 '\x2264')
  ,("geq",MathChar MathRel 0 '\x2265')
  ,("ge",MathChar MathRel 0 '\x2265')
  ,("subset",MathChar MathRel 0 '\x2282')
  ,("supset",MathChar MathRel 0 '\x2283')

  -- Punctuation symbols
  ,("colon",MathChar MathPunct 0 ':')
  ,("cdots",MathChar MathPunct 0 '\x22EF')
  ,("vdots",MathChar MathPunct 0 '\x22EE')
  ,("ddots",MathChar MathPunct 0 '\x22F1')

  -- TODO: Accents
  -- TODO: Radicals
  ]

data LimitsSpec = Limits
                | NoLimits
                deriving (Eq,Show)

mathFunctions :: [(String,LimitsSpec)]
mathFunctions = [("log",NoLimits)
                ,("lim",Limits)
                ,("sin",NoLimits)
                ,("cos",NoLimits)
                ,("tan",NoLimits)
                ,("sinh",NoLimits)
                ,("cosh",NoLimits)
                ,("tanh",NoLimits)
                ,("max",Limits)
                ,("min",Limits)
                ,("sup",Limits)
                ,("inf",Limits)
                ,("arg",NoLimits)
                ,("ker",NoLimits)
                ,("dim",NoLimits)
                ,("hom",NoLimits)
                ,("det",Limits)
                ,("exp",NoLimits)
                ,("gcd",Limits)
                ,("deg",NoLimits)
                ]

-- \frac

data Command = Action ()
             | MathCharacter !MathChar

data TeXToMathState = TeXToMathState { definedCommands :: Map.Map Text Command
                                     }

{-
type LaTeXMathReaderT m = StateT TeXToMathState (StateT (TeXState m) (ExceptT String m))
type LaTeXMathReader = LaTeXMathReaderT Identity

nextToken' :: LaTeXMathReader (Maybe TeXToken)
nextToken' = lift nextToken

nextRequiredToken :: String -> LaTeXMathReader TeXToken
nextRequiredToken m = do
  t <- lift nextToken
  case t of
    Just t -> return t
    Nothing -> throwError m

readRestOfBalancedText :: Bool -> Bool -> LaTeXMathReader [TeXToken]
readRestOfBalancedText !includeEndGroup !isLong = do
  t <- nextRequiredToken "unexpected EOF"
  case t of
    TTCharacter _ CCBeginGroup -> do
      u <- readRestOfBalancedText True isLong
      v <- readRestOfBalancedText includeEndGroup isLong
      return $ t : (u ++ v)
    TTCharacter _ CCEndGroup -> do
      return (if includeEndGroup then [t] else [])
    TTControlSeq "par" | not isLong -> throwError "unexpected \\par"
    t -> do
      (t:) <$> readRestOfBalancedText includeEndGroup isLong

nextNonBlankToken :: String -> LaTeXMathReader TeXToken
nextNonBlankToken m = do
  t <- nextRequiredToken m
  case t of
    TTCharacter _ CCSpace -> nextNonBlankToken m
    _ -> return t

readArgument :: Bool -> LaTeXMathReader [TeXToken]
readArgument !isLong = do
  t <- nextNonBlankToken "expected an argument, but got EOF"
  case t of
    TTCharacter _ CCBeginGroup -> do
      readRestOfBalancedText False isLong
    TTCharacter _ CCEndGroup -> throwError "unexpected end of group"
    TTControlSeq "par" | not isLong -> throwError "unexpected \\par"
    t -> return [t]
-}

--readOptionalArgument :: LaTeXMathReader (Maybe [TeXToken])
--hasStar :: LaTeXMathReader Bool
--table =
{-
('.',MathChar MathOrd 1 '.') -- "013A
('/',MathChar MathOrd 1 '/') -- "013D
('\\',MathChar MathOrd 2 '\\') -- "026E
('|',MathChar MathOrd 2 '|') -- "026A
('+',MathChar MathBin 0 '+') -- "202B
('-',MathChar MathBin 2 '-') -- "2200
('*',MathChar MathBin 2 '*') -- "2203
(':',MathChar MathRel 0 ':') -- "303A
('=',MathChar MathRel 0 '=') -- "303D
('<',MathChar MathRel 1 '<') -- "303C
('>',MathChar MathRel 1 '>') -- "303E
('(',MathChar MathOpen 0 '(') -- "4028
('[',MathChar MathOpen 0 '[') -- "405B
('{',MathChar MathOpen 2 '{') -- "4266
('!',MathChar MathClose 0 '!') -- "5021
(')',MathChar MathClose 0 ')') -- "5029
('?',MathChar MathClose 0 '?') -- "503F
(']',MathChar MathClose 0 ']') -- "505D
('}',MathChar MathClose 2 '}') -- "5267
(';',MathChar MathPunct 0 ';') -- "603B
(',',MathChar MathPunct 1 ',') -- "613B
Active: ' ', '\'', '_'

Ordinary:
\partial
\lat
\natural
\sharp
\ell
\imath
\jmath
\wp
\prime
\infty
\triangle
\forall
\exists
\neg
\emptyset
\Re
\Im
\top
\bot
\aleph
\nabla
\clubsuit
\diamondsuit
\heartsuit
\spadesuit

Large Operators:
\smallint
\bigsqcup
\ointop
\digodot
\bigoplus
\bigotimes
\sum
\prod
\intop
\bigcup
\bigcap
\biguplus
\bigwedge
\bigvee
\coprod

Binary Operations:
\triangleright
\triangleleft
\star
\cdot
\times
\ast
\div
\diamond
\pm
\mp
\oplus
\ominus
\otimes
\oslash
\odot
\bigcirc
\circ
\bullet
\bigtriangleup
\bigtriangledown
\cup
\cap
\uplus
\wedge
\vee
\setminus
\wr
\amalg
\sqcup
\sqcap
\dagger
\ddagger

Relations:
\leftharpoonup
\leftharpoondown
\rightharpoonup
\rightharpoondown
\smile
\frown
\asymp
\equiv
\subseteq
\supseteq
\leq
\geq
\preceq
\succeq
\sim
\approx
\subset
\supset
\ll
\gg
\prec
\succ
\leftarrow
\rightarrow
\leftrightarrow
\nearrow
\searrow
\simeq
\Leftarrow
\Rightarrow
\Leftrightarrow
\nwarrow
\swarrow
\propto
\in
\ni
\not
\mapstochar
\perp
\vdash
\dashv
\mid
\parallel
\sqsubseteq
\sqsupseteq
-}
