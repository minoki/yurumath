{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Text.YuruMath.TeX.Typeset where
import Text.YuruMath.TeX.Types
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.OpenUnion
import TypeFun.Data.List (Elem)
import Control.Monad.Except

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
  | Thfillneg
  | TControlSpace -- "\ "
  | Traise
  | Tlower
  | TItalicCorrection -- \/
  | Tdiscretionary
  | TDiscretionaryHyphen -- \-
  deriving (Eq,Show)

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute TypesetCommand m where
  doExecute Tspecial      = throwError $ "You can't use \\special in this mode"
  doExecute Tpenalty      = throwError $ "You can't use \\penalty in this mode"
  doExecute Tkern         = throwError $ "You can't use \\kern in this mode"
  doExecute Tunpenalty    = throwError $ "You can't use \\unpenalty in this mode"
  doExecute Tunkern       = throwError $ "You can't use \\unkern in this mode"
  doExecute Tunskip       = throwError $ "You can't use \\unskip in this mode"
  doExecute Tmark         = throwError $ "You can't use \\mark in this mode"
  doExecute Tinsert       = throwError $ "You can't use \\insert in this mode"
  doExecute Tvadjust      = throwError $ "You can't use \\vadjust in this mode"
  doExecute Thalign       = throwError $ "You can't use \\halign in this mode"
  doExecute Tindent       = throwError $ "You can't use \\indent in this mode"
  doExecute Tnoindent     = throwError $ "You can't use \\noindent in this mode"
  doExecute Tvrule        = throwError $ "You can't use \\vrule in this mode"
  doExecute Tchar         = throwError $ "You can't use \\char in this mode"
  doExecute Thskip        = throwError $ "You can't use \\hskip in this mode"
  doExecute Thfil         = throwError $ "You can't use \\hfil in this mode"
  doExecute Thfill        = throwError $ "You can't use \\hfill in this mode"
  doExecute Thss          = throwError $ "You can't use \\hss in this mode"
  doExecute Thfillneg     = throwError $ "You can't use \\hfillneg in this mode"
  doExecute TControlSpace = throwError $ "You can't use <control sequence> in this mode"
  doExecute Traise        = throwError $ "You can't use \\raise in this mode"
  doExecute Tlower        = throwError $ "You can't use \\lower in this mode"
  doExecute TItalicCorrection    = throwError $ "You can't use \\/ in this mode"
  doExecute Tdiscretionary       = throwError $ "You can't use \\discretionary in this mode"
  doExecute TDiscretionaryHyphen = throwError $ "You can't use \\- in this mode"
  doGlobal _ = Nothing
  getQuantity _ = NotQuantity

typesetCommands :: (Elem TypesetCommand vset) => Map.Map Text (Union vset)
typesetCommands = fmap liftUnion $ Map.fromList
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
  ,("hfillneg",     Thfillneg)
  ,(" ",            TControlSpace)
  ,("raise",        Traise)
  ,("lower",        Tlower)
  ,("/",            TItalicCorrection)
  ,("discretionary",Tdiscretionary)
  ,("-",            TDiscretionaryHyphen)
  ]
