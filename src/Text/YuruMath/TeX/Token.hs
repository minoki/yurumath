module Text.YuruMath.TeX.Token where
import Data.Text (Text)

data CatCode = CCEscape -- 0
             | CCBeginGroup -- 1
             | CCEndGroup -- 2
             | CCMathShift -- 3
             | CCAlignmentTab -- 4
             | CCEndLine -- 5
             | CCParam -- 6
             | CCSup -- 7
             | CCSub -- 8
             | CCIgnored -- 9
             | CCSpace -- 10
             | CCLetter -- 11
             | CCOther -- 12
             | CCActive -- 13
             | CCComment -- 14
             | CCInvalid -- 15
             deriving (Eq,Show,Enum,Bounded)

data TeXToken = TTControlSeq !Text
              | TTCharacter !Char !CatCode
              | TTParameter !Int
              deriving (Eq,Show)

data MathStyle = MathDisplayStyle
               | MathTextStyle
               | MathScriptStyle
               | MathScriptScriptStyle
               deriving (Eq,Show,Enum,Bounded)

data MathStyle2 = MathStyleNormal !MathStyle
                | MathStyleCramped !MathStyle
                deriving (Eq,Show)

nextMathStyle :: MathStyle -> MathStyle
nextMathStyle MathDisplayStyle = MathScriptStyle -- ?
nextMathStyle MathTextStyle = MathScriptStyle
nextMathStyle MathScriptStyle = MathScriptScriptStyle
nextMathStyle MathScriptScriptStyle = MathScriptScriptStyle

makeCramped :: MathStyle2 -> MathStyle2
makeCramped (MathStyleNormal s) = MathStyleCramped s
makeCramped s@(MathStyleCramped _) = s

data MathClass = MathOrd -- \mathord, ordinary object (1)
               | MathOp -- \mathop, large operator (2)
               | MathBin -- \mathbin, binary operation (3)
               | MathRel -- \mathrel, relation (4)
               | MathOpen -- \mathopen, opening symbol (5)
               | MathClose -- \mathclose, closing symbol (6)
               | MathPunct -- \mathpunct, punctuation symbol (7)
               -- variable family? (8)
               | MathInner -- \mathinner, inner formula
               deriving (Eq,Show,Enum,Bounded)

data MathFamily = FamilyRoman -- (0)
                | FamilyMathItalic -- (1)

data MathChar = MathChar !MathClass !MathFamily !Char
-- "xyzz (15-bit number)

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
