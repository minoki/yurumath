{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Text.YuruMath.TeX.Typeset where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Meaning
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
  deriving (Eq,Show)

instance Meaning TypesetCommand where
  meaningString Tspecial = controlSequence "special"
  meaningString Tpenalty = controlSequence "penalty"
  meaningString Tkern = controlSequence "kern"
  meaningString Tunpenalty = controlSequence "unpenalty"
  meaningString Tunkern = controlSequence "unkern"
  meaningString Tunskip = controlSequence "unskip"
  meaningString Tmark = controlSequence "mark"
  meaningString Tinsert = controlSequence "insert"
  meaningString Tvadjust = controlSequence "vadjust"
  meaningString Thalign = controlSequence "halign"
  meaningString Tindent = controlSequence "indent"
  meaningString Tnoindent = controlSequence "noindent"
  meaningString Tvrule = controlSequence "vrule"
  meaningString Tchar = controlSequence "char"
  meaningString Thskip = controlSequence "hskip"
  meaningString Thfil = controlSequence "hfil"
  meaningString Thfill = controlSequence "hfill"
  meaningString Thss = controlSequence "hfss"
  meaningString Thfilneg = controlSequence "hfilneg"
  meaningString TControlSpace = controlSequence " "
  meaningString Traise = controlSequence "raise"
  meaningString Tlower = controlSequence "lower"
  meaningString TItalicCorrection = controlSequence "/"
  meaningString Tdiscretionary = controlSequence "discretionary"
  meaningString TDiscretionaryHyphen = controlSequence "-"
  meaningString TUstartmath = controlSequence "Ustartmath"
  meaningString TUstartdisplaymath = controlSequence "Ustartdisplaymath"

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
                deriving (Eq,Show)

instance Meaning BoxCommand where
  meaningString Bbox = controlSequence "box"
  meaningString Bcopy = controlSequence "copy"
  meaningString Blastbox = controlSequence "lastbox"
  meaningString Bvsplit = controlSequence "vsplit"
  meaningString Bhbox = controlSequence "hbox"
  meaningString Bvbox = controlSequence "vbox"
  meaningString Bvtop = controlSequence "vtop"

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute BoxCommand m where
  doExecute _ = throwError "box commands are not implemented yet"
  doGlobal _ = Nothing
  getQuantity _ = NotQuantity

class BoxReader box m where
  readBox :: BoxCommand -> m box

instance (MonadError String m) => BoxReader Void m where
  readBox _ = throwError "box commands are not supported"

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
