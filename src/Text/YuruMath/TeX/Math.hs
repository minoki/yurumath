{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.YuruMath.TeX.Math where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Tokenizer
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Expansion
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Identity

-- sqrt, overline
makeCramped :: MathStyle -> MathStyle
makeCramped MathDisplayStyle = MathDisplayStyleCramped
makeCramped MathTextStyle = MathTextStyleCramped
makeCramped MathScriptStyle = MathScriptStyleCramped
makeCramped MathScriptScriptStyle = MathScriptScriptStyleCramped
makeCramped style = style

-- superscript
superscriptStyle :: MathStyle -> MathStyle
superscriptStyle MathDisplayStyle = MathScriptStyle
superscriptStyle MathDisplayStyleCramped = MathScriptStyleCramped
superscriptStyle MathTextStyle = MathScriptStyle
superscriptStyle MathTextStyleCramped = MathScriptStyleCramped
superscriptStyle MathScriptStyle = MathScriptScriptStyle
superscriptStyle MathScriptStyleCramped = MathScriptScriptStyleCramped
superscriptStyle scriptscriptstyle = scriptscriptstyle

-- subscript
subscriptStyle :: MathStyle -> MathStyle
subscriptStyle = makeCramped . superscriptStyle

-- numerator
smallerStyle :: MathStyle -> MathStyle
smallerStyle MathDisplayStyle = MathTextStyle
smallerStyle MathDisplayStyleCramped = MathTextStyleCramped
smallerStyle MathTextStyle = MathScriptStyle
smallerStyle MathTextStyleCramped = MathScriptStyleCramped
smallerStyle MathScriptStyle = MathScriptScriptStyle
smallerStyle MathScriptStyleCramped = MathScriptScriptStyleCramped
smallerStyle scriptscriptstyle = scriptscriptstyle

-- denominator
denominatorStyle :: MathStyle -> MathStyle
denominatorStyle = makeCramped . smallerStyle

-- Radicals

-- Math accents
-- "xyzz
-- y: the family
-- zz: the character

data AtomType = AOrd   -- ordinary
              | AOp    -- large operator
              | ABin   -- binary operation
              | ARel   -- relation
              | AOpen  -- opening
              | AClose -- closing
              | APunct -- punctuation
              | AInner -- inner
              | AOver  -- overline
              | AUnder -- underline
              | AAcc   -- accented
              | ARad   -- radical
              | AVcent -- vcenter
              deriving (Eq,Show)

data MathField = MFEmpty
               | MFSymbol {-family number-} !Word {-position number-} !Char
               | MFBox
               | MFSubList MathList

data Atom = Atom { atomType        :: !AtomType
                 , atomNucleus     :: !MathField
                 , atomSuperscript :: !MathField
                 , atomSubscript   :: !MathField
                 }

-- generalized fraction
data GenFrac = GFOver
             | GFAtop
             | GFAbove {-dimen-}
             | GFOverWithDelims {-delim-} {-delim-}
             | GFAtopWithDelims {-delim-} {-delim-}
             | GFAboveWithDelims {-delim-} {-delim-} {-dimen-}

data BoundaryType = BoundaryLeft | BoundaryRight
                  deriving (Eq)

data MathItem = IAtom !Atom
              | IHorizontalMaterial
              | IVerticalMaterial -- \mark or \insert or \vadjust
              | IGlue -- \hskip or \mskip or \nonscript
              | IKern -- \kern or \mkern
              | IStyleChange !MathStyle -- \displaystyle, \textstyle, etc
              | IGenFrac !GenFrac MathList MathList -- \above, \over, etc -- \over | \atop | above
              | IBoundary !BoundaryType {-delimiter-} -- \left or \right
              | IChoice MathList MathList MathList MathList -- \mathchoice

type MathList = [MathItem]

{-
<character> ::= <letter> | <otherchar> | \char<8-bit number> | <chardef token>
<math character> ::= \mathchar<15-bit number> | <mathchardef token> | \delimiter<27-bit number>
<math symbol> ::= <character> | <math character>
<math field> ::= <math symbol> | <filler>{<math mode material>}
<delim> ::= <filler>\delimiter<27-bit number> | <filler><letter> | <filler><otherchar>
-}

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

mathCommands :: Map.Map Text MathCode
mathCommands = Map.fromList
  -- Ordinary Symbols
  [("infty",mkMathCode MathOrd 0 '\x221E')
  ,("forall",mkMathCode MathOrd 0 '\x2200')
  ,("exists",mkMathCode MathOrd 0 '\x2203')
  ,("emptyset",mkMathCode MathOrd 0 '\x2205')

  -- Large Operators
  ,("sum",mkMathCode MathOp 0 '\x2211')
  ,("int",mkMathCode MathOp 0 '\x222B') -- nolimits
  ,("prod",mkMathCode MathOp 0 '\x220F')

  -- Binary Symbols
  ,("cap",mkMathCode MathBin 0 '\x2229')
  ,("cup",mkMathCode MathBin 0 '\x222A')
  ,("pm",mkMathCode MathBin 0 '\x00B1')
  ,("mp",mkMathCode MathBin 0 '\x2213')
  ,("times",mkMathCode MathBin 0 '\x00D7')

  -- Relations
  ,("mid",mkMathCode MathRel 0 '|') -- ?
  ,("leftarrow",mkMathCode MathRel 0 '\x2190')
  ,("rightarrow",mkMathCode MathRel 0 '\x2192')
  ,("in",mkMathCode MathRel 0 '\x2208')
  ,("ni",mkMathCode MathRel 0 '\x220B')
  ,("leq",mkMathCode MathRel 0 '\x2264')
  ,("le",mkMathCode MathRel 0 '\x2264')
  ,("geq",mkMathCode MathRel 0 '\x2265')
  ,("ge",mkMathCode MathRel 0 '\x2265')
  ,("subset",mkMathCode MathRel 0 '\x2282')
  ,("supset",mkMathCode MathRel 0 '\x2283')

  -- Punctuation symbols
  ,("colon",mkMathCode MathPunct 0 ':')
  ,("cdots",mkMathCode MathPunct 0 '\x22EF')
  ,("vdots",mkMathCode MathPunct 0 '\x22EE')
  ,("ddots",mkMathCode MathPunct 0 '\x22F1')

  -- TODO: Accents
  -- TODO: Radicals
  ]

data LimitsSpec = Limits
                | NoLimits
                | DisplayLimits
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

execMath :: (MonadTeXState a m, MonadError String m) => m ()
execMath = return ()
{-
'{' ... '}'
   '{' -> new math list
   '}' -> new Ord atom or single Acc atom
<math symbol>
  -> new atom
<math atom><math field>
  (<math atom> = \mathord | \mathop | \mathbin | \mathrel | \mathopen
                   | \mathclose | \mathpunct | \mathinner | \underline | \overline)
  -> new atom
\mathaccent<15-bit number><math field>
  -> new Acc atom
\radical<27-bit number><math field>
  -> new Rad atom
<superscript><math field>
  -> new Ord with empty field if the current list does not end with an atom
     the superscript field of this atom is filled by <math field>
<subscript><math field>
  -> like <superscript> but with subscript field
\displaylimits, \limits, \nolimits
  -> the current list must end with an Op atom
     modify a special field in that Op atom
\/
  -> ...
\discretionaly<general text><general text><general text>
  -> ...
\- = \discretionary{ - }{}{}
\mathchoice<general text><general text><general text><general text>
  -> ....
\displaystyle, \textstyle, \scriptstyle, \scriptscriptstyle
[LuaTeX: \crampeddisplaystyle, \crampedtextstyle, \crampedscriptstyle, \crampedscriptscriptstyle]
  -> style-change item
\left<delim><math mode material>\right<delim>
  -> ...
<generalized fraction command>
[LuaTeX: \Ustack {... <generalized fraction command> ...}]
  -> ...
-}

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
