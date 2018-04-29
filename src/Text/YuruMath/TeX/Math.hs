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
makeCramped DisplayStyle      = CrampedDisplayStyle
makeCramped TextStyle         = CrampedTextStyle
makeCramped ScriptStyle       = CrampedScriptStyle
makeCramped ScriptScriptStyle = CrampedScriptScriptStyle
makeCramped crampedstyle = crampedstyle

-- superscript
superscriptStyle :: MathStyle -> MathStyle
superscriptStyle        DisplayStyle =        ScriptStyle
superscriptStyle CrampedDisplayStyle = CrampedScriptStyle
superscriptStyle        TextStyle    =        ScriptStyle
superscriptStyle CrampedTextStyle    = CrampedScriptStyle
superscriptStyle        ScriptStyle  =        ScriptScriptStyle
superscriptStyle CrampedScriptStyle  = CrampedScriptScriptStyle
superscriptStyle scriptscriptstyle = scriptscriptstyle

-- subscript
subscriptStyle :: MathStyle -> MathStyle
subscriptStyle = makeCramped . superscriptStyle

-- numerator
smallerStyle :: MathStyle -> MathStyle
smallerStyle        DisplayStyle =        TextStyle
smallerStyle CrampedDisplayStyle = CrampedTextStyle
smallerStyle        TextStyle    =        ScriptStyle
smallerStyle CrampedTextStyle    = CrampedScriptStyle
smallerStyle        ScriptStyle  =        ScriptScriptStyle
smallerStyle CrampedScriptStyle  = CrampedScriptScriptStyle
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
