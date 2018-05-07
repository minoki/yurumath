{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
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
import Control.Lens.Getter (use,uses)
import Data.OpenUnion
import TypeFun.Data.List (Elem,SubList)

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

--
-- Setting math style
--

newtype MathStyleSet = MathStyleSet MathStyle
                     deriving (Eq,Show)

instance (Monad m, MonadError String m) => DoExecute MathStyleSet m where
  doExecute (MathStyleSet s) = throwError "not implemented yet"
  getIntegerValue (MathStyleSet v) = Just $ return $ fromIntegral $ fromEnum v -- LuaTeX extension

--
-- Expandable math commands
--

data MathExpandable = Mmathstyle -- LuaTeX extension
                    deriving (Eq,Show)

-- LuaTeX extension: \mathstyle
mathstyleCommand :: (MonadTeXState s m, MonadError String m) => m [ExpansionToken]
mathstyleCommand = do
  ismm <- uses mode isMMode
  if ismm
    then do
    style <- use (localState . mathStyle)
    stringToEToken $ show $ fromEnum style
    else stringToEToken "-1"

instance IsExpandable MathExpandable where
  isConditional _ = False

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExpand MathExpandable m where
  doExpand Mmathstyle = mathstyleCommand
  evalBooleanConditional _ = Nothing

--
-- Other math commands
--

data MathCommands = Mchar -- ?
                  | Mmathchar
                  | Mmathaccent
                  | Mdelimiter
                  | Mradical
                  | Mmathord
                  | Mmathop
                  | Mmathbin
                  | Mmathrel
                  | Mmathopen
                  | Mmathclose
                  | Mmathpunct
                  | Mmathinner
                  | Munderline
                  | Moverline
                  | Mdisplaylimits
                  | Mlimits
                  | Mnolimits
                  | Mdiscretionaly
                  | Mmathchoice
                  | Mleft
                  | Mright
                  | Mover
                  | Matop

                    -- e-TeX extension:
                  | Mmiddle

                    -- LuaTeX extensions:
                  | MUmathchar
                  | MUmathaccent
                  | MUdelimiter
                  | MUradical
                  | MUmathcharnum
                  | MUroot
                  | MUoverdelimiter
                  | MUunderdelimiter
                  | MUdelimiterover
                  | MUdelimiterunder
                  | MUhextensible
                  | MUstack
                  -- \Usuperscript, \Usubscript, \Ustartmath, \Ustopmath, \Ustartdisplaymath, \Ustopdisplaymath
                  -- \Unosuperscript, \Unosubscript

                  deriving (Eq,Show)

instance (Monad m, MonadError String m) => DoExecute MathCommands m where
  doExecute _ = throwError "not implemented yet"
  getIntegerValue _ = Nothing

--
-- List of commands
--

mathDefinitionsE :: (Elem MathExpandable set) => Map.Map Text (Union set)
mathDefinitionsE = Map.fromList
  [("mathstyle", liftUnion Mmathstyle) -- LuaTeX extension
  ]

mathDefinitions :: (SubList '[MathCommands,MathStyleSet] set) => Map.Map Text (Union set)
mathDefinitions = Map.fromList
  [("char",         liftUnion Mchar)
  ,("mathchar",     liftUnion Mmathchar)
  ,("delimiter",    liftUnion Mdelimiter)
  ,("mathord",      liftUnion Mmathord)
  ,("mathop",       liftUnion Mmathop)
  ,("mathbin",      liftUnion Mmathbin)
  ,("mathrel",      liftUnion Mmathrel)
  ,("mathopen",     liftUnion Mmathopen)
  ,("mathclose",    liftUnion Mmathclose)
  ,("mathpunct",    liftUnion Mmathpunct)
  ,("mathinner",    liftUnion Mmathinner)
  ,("underline",    liftUnion Munderline)
  ,("overline",     liftUnion Moverline)
  ,("mathaccent",   liftUnion Mmathaccent)
  ,("radical",      liftUnion Mradical)
  ,("displaylimits",liftUnion Mdisplaylimits)
  ,("limits",       liftUnion Mlimits)
  ,("nolimits",     liftUnion Mnolimits)
  ,("discretionaly",liftUnion Mdiscretionaly)
  ,("mathchoice",   liftUnion Mmathchoice)
  ,("left",         liftUnion Mleft)
  ,("right",        liftUnion Mright)
  ,("over",         liftUnion Mover)
  ,("atop",         liftUnion Matop)

  -- e-TeX extension:
  ,("middle",       liftUnion Mmiddle)

  -- LuaTeX extension:
  ,("Ustack",       liftUnion MUstack)

  ,("displaystyle",            liftUnion (MathStyleSet DisplayStyle))
  ,("textstyle",               liftUnion (MathStyleSet TextStyle))
  ,("scriptstyle",             liftUnion (MathStyleSet ScriptStyle))
  ,("scriptscriptstyle",       liftUnion (MathStyleSet ScriptScriptStyle))

  -- LuaTeX extensions:
  ,("crampeddisplaystyle",     liftUnion (MathStyleSet CrampedDisplayStyle))
  ,("crampedtextstyle",        liftUnion (MathStyleSet CrampedTextStyle))
  ,("crampedscriptstyle",      liftUnion (MathStyleSet CrampedScriptStyle))
  ,("crampedscriptscriptstyle",liftUnion (MathStyleSet CrampedScriptScriptStyle))
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
