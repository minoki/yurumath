{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Text.YuruMath.TeX.Typeset where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Quantity
import Text.YuruMath.TeX.Meaning
import Text.YuruMath.TeX.Expansion
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.OpenUnion
import TypeFun.Data.List (Elem)
import Control.Monad.Except
import Data.Monoid ((<>))
import Data.Void

-- Commands that are available in multiple modes
data TypesetCommand
  = Tspecial
  | Tpenalty
  | Tkern
  | Tunpenalty
  | Tunkern
  | Tunskip
  | Tmark
  | Tinsert
  | Tvadjust
  | Thalign -- Vertical & Math
  | Tindent
  | Tnoindent
  | Tvrule
  -- <leaders> ::= \leaders | \cleaders | \xleaders
  -- <box> ::= \box, \copy, \lastbox, \vsplit, \hbox, \vbox, \vtop

  -- Horizontal & Math
  | Tchar
  | Thskip
  | Thfil
  | Thfill
  | Thss
  | Thfilneg
  | TControlSpace -- "\ "
  | Traise
  | Tlower
  | TItalicCorrection -- \/
  | Tdiscretionary
  | TDiscretionaryHyphen -- \-
  | TUstartmath
  | TUstartdisplaymath
  deriving (Eq,Show,Enum,Bounded)

instance IsPrimitive TypesetCommand where
  primitiveName Tspecial = "special"
  primitiveName Tpenalty = "penalty"
  primitiveName Tkern = "kern"
  primitiveName Tunpenalty = "unpenalty"
  primitiveName Tunkern = "unkern"
  primitiveName Tunskip = "unskip"
  primitiveName Tmark = "mark"
  primitiveName Tinsert = "insert"
  primitiveName Tvadjust = "vadjust"
  primitiveName Thalign = "halign"
  primitiveName Tindent = "indent"
  primitiveName Tnoindent = "noindent"
  primitiveName Tvrule = "vrule"
  primitiveName Tchar = "char"
  primitiveName Thskip = "hskip"
  primitiveName Thfil = "hfil"
  primitiveName Thfill = "hfill"
  primitiveName Thss = "hfss"
  primitiveName Thfilneg = "hfilneg"
  primitiveName TControlSpace = " "
  primitiveName Traise = "raise"
  primitiveName Tlower = "lower"
  primitiveName TItalicCorrection = "/"
  primitiveName Tdiscretionary = "discretionary"
  primitiveName TDiscretionaryHyphen = "-"
  primitiveName TUstartmath = "Ustartmath"
  primitiveName TUstartdisplaymath = "Ustartdisplaymath"

instance Meaning TypesetCommand

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute TypesetCommand m where
  doExecute = can'tUseThisCommandInCurrentMode
  doGlobal _ = Nothing
  getQuantity _ = NotQuantity

data BoxCommand = Bbox
                | Bcopy
                | Blastbox
                | Bvsplit
                | Bhbox
                | Bvbox
                | Bvtop
                deriving (Eq,Show,Enum,Bounded)

instance IsPrimitive BoxCommand where
  primitiveName Bbox = "box"
  primitiveName Bcopy = "copy"
  primitiveName Blastbox = "lastbox"
  primitiveName Bvsplit = "vsplit"
  primitiveName Bhbox = "hbox"
  primitiveName Bvbox = "vbox"
  primitiveName Bvtop = "vtop"

instance Meaning BoxCommand

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute BoxCommand m where
  doExecute _ = throwError "box commands are not implemented yet"
  doGlobal _ = Nothing
  getQuantity _ = NotQuantity

class BoxReader box m where
  readBox :: BoxCommand -> m box

instance (MonadError String m) => BoxReader Void m where
  readBox _ = throwError "box commands are not supported"

-- <box specification> ::= "to"<dimen><filler> | "spread"<dimen><filler> | <filler>
data BoxSpec = BoxWithoutSpec
             | BoxTo Dimen
             | BoxSpread Dimen
             deriving (Eq,Show)

readBoxSpecification :: (MonadTeXState s m, MonadError String m) => m BoxSpec
readBoxSpecification = do
  mspec <- readOneOfKeywordsV [("to",BoxTo <$> readDimension)
                              ,("spread",BoxSpread <$> readDimension)
                              ]
  case mspec of
    Nothing -> pure BoxWithoutSpec
    Just m -> m

typesetCommands :: (Elem TypesetCommand vset, Elem BoxCommand vset) => Map.Map Text (Union vset)
typesetCommands = (fmap liftUnion $ Map.fromList
  [("special",      Tspecial)
  ,("penalty",      Tpenalty)
  ,("kern",         Tkern)
  ,("unpenalty",    Tunpenalty)
  ,("unkern",       Tunkern)
  ,("unskip",       Tunskip)
  ,("mark",         Tmark)
  ,("insert",       Tinsert)
  ,("vadjust",      Tvadjust)
  ,("halign",       Thalign)
  ,("indent",       Tindent)
  ,("noindent",     Tnoindent)
  ,("vrule",        Tvrule)
  ,("char",         Tchar)
  ,("hskip",        Thskip)
  ,("hfil",         Thfil)
  ,("hfill",        Thfill)
  ,("hss",          Thss)
  ,("hfilneg",      Thfilneg)
  ,(" ",            TControlSpace)
  ,("raise",        Traise)
  ,("lower",        Tlower)
  ,("/",            TItalicCorrection)
  ,("discretionary",Tdiscretionary)
  ,("-",            TDiscretionaryHyphen)
  ,("Ustartmath",   TUstartmath)
  ,("Ustartdisplaymath",TUstartdisplaymath)
  ]) <> (fmap liftUnion $ Map.fromList
  [("box", Bbox)
  ,("copy", Bcopy)
  ,("lastbox", Blastbox)
  ,("hbox", Bhbox)
  ,("vbox", Bvbox)
  ,("vtop", Bvtop)
  ])
