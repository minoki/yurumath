{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.YuruMath.TeX.Primitive.Register
  (RegisterCommand
  ,CountReg
  ,DimenReg
  ,SkipReg
  ,MuskipReg
  ,ToksReg
  ,registerCommands
  ) where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Meaning
import Text.YuruMath.TeX.Quantity
import Text.YuruMath.TeX.Expansion
import Text.YuruMath.TeX.Execution
import Data.String
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Map.Strict as Map
import Control.Monad.Error.Class
import Control.Lens.Lens (Lens')
import Control.Lens.At (at)
import Control.Lens.Iso (non)
import Control.Lens.Getter (use)
import Data.OpenUnion
import TypeFun.Data.List (SubList,Elem)

--
-- \count register
--

-- \countdef-ed value
newtype CountReg = CountReg Int
  deriving (Eq,Show)

countRegAt :: (IsLocalState localstate) => Int -> Lens' localstate Integer
countRegAt !index = countReg . at index . non 0

-- \countdef-ed token
setCountReg :: (MonadTeXState s m, MonadError String m) => Int -> m (Assignment s)
setCountReg !index = do
  readEquals
  value <- readNumber
  texAssign (countRegAt index) value

-- \count command
countSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
countSet = readRegIndex >>= setCountReg

countGet :: (MonadTeXState s m, MonadError String m) => m Integer
countGet = do
  index <- readRegIndex
  use (localState . countRegAt index)

countdefCommand :: (MonadTeXState s m, MonadError String m, Elem CountReg (NValueSet s)) => m (Assignment s)
countdefCommand = do
  name <- readCommandName
  readEquals
  index <- readRegIndex
  texAssign (definitionAt name) (nonexpandableToValue $ CountReg index)

instance Meaning CountReg where
  meaningString (CountReg i) = controlSequence "count" <> fromString (show i)

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute CountReg m where
  doExecute (CountReg i)    = runLocal $ setCountReg i
  doGlobal (CountReg i)     = Just $ runGlobal $ setCountReg i
  doArithmetic (CountReg i) = Just $ arithmeticInteger (countRegAt i)
  getQuantity (CountReg i)  = QInteger $ use (localState . countRegAt i)

--
-- \dimen register
--

-- \dimendef-ed value
newtype DimenReg = DimenReg Int
  deriving (Eq,Show)

dimenRegAt :: (IsLocalState localstate) => Int -> Lens' localstate Dimen
dimenRegAt !index = dimenReg . at index . non zeroQ

-- \dimendef-ed token
setDimenReg :: (MonadTeXState s m, MonadError String m) => Int -> m (Assignment s)
setDimenReg !index = do
  readEquals
  value <- readDimension
  texAssign (dimenRegAt index) value

-- \dimen command
dimenSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
dimenSet = readRegIndex >>= setDimenReg

dimenGet :: (MonadTeXState s m, MonadError String m) => m Dimen
dimenGet = do
  index <- readRegIndex
  use (localState . dimenRegAt index)

dimendefCommand :: (MonadTeXState s m, MonadError String m, Elem DimenReg (NValueSet s)) => m (Assignment s)
dimendefCommand = do
  name <- readCommandName
  readEquals
  index <- readRegIndex
  texAssign (definitionAt name) (nonexpandableToValue $ DimenReg index)

instance Meaning DimenReg where
  meaningString (DimenReg i) = controlSequence "dimen" <> fromString (show i)

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute DimenReg m where
  doExecute (DimenReg i)    = runLocal $ setDimenReg i
  doGlobal (DimenReg i)     = Just $ runGlobal $ setDimenReg i
  doArithmetic (DimenReg i) = Just $ arithmeticQuantity (dimenRegAt i)
  getQuantity (DimenReg i)  = QDimension $ use (localState . dimenRegAt i)

--
-- \skip register
--

-- \skipdef-ed value
newtype SkipReg = SkipReg Int
  deriving (Eq,Show)

skipRegAt :: (IsLocalState localstate) => Int -> Lens' localstate (Glue Dimen)
skipRegAt !index = skipReg . at index . non zeroQ

-- \skipdef-ed token
setSkipReg :: (MonadTeXState s m, MonadError String m) => Int -> m (Assignment s)
setSkipReg !index = do
  readEquals
  value <- readGlue
  texAssign (skipRegAt index) value

-- \skip command
skipSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
skipSet = readRegIndex >>= setSkipReg

skipGet :: (MonadTeXState s m, MonadError String m) => m (Glue Dimen)
skipGet = do
  index <- readRegIndex
  use (localState . skipRegAt index)

skipdefCommand :: (MonadTeXState s m, MonadError String m, Elem SkipReg (NValueSet s)) => m (Assignment s)
skipdefCommand = do
  name <- readCommandName
  readEquals
  index <- readRegIndex
  texAssign (definitionAt name) (nonexpandableToValue $ SkipReg index)

instance Meaning SkipReg where
  meaningString (SkipReg i) = controlSequence "skip" <> fromString (show i)

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute SkipReg m where
  doExecute (SkipReg i)    = runLocal $ setSkipReg i
  doGlobal (SkipReg i)     = Just $ runGlobal $ setSkipReg i
  doArithmetic (SkipReg i) = Just $ arithmeticQuantity (skipRegAt i)
  getQuantity (SkipReg i)  = QGlue $ use (localState . skipRegAt i)

--
-- \muskip register
--

-- \muskipdef-ed value
newtype MuskipReg = MuskipReg Int
  deriving (Eq,Show)

muskipRegAt :: (IsLocalState localstate) => Int -> Lens' localstate (Glue MuDimen)
muskipRegAt !index = muskipReg . at index . non zeroQ

-- \muskipdef-ed token
setMuskipReg :: (MonadTeXState s m, MonadError String m) => Int -> m (Assignment s)
setMuskipReg !index = do
  readEquals
  value <- readMuGlue
  texAssign (muskipRegAt index) value

-- \muskip command
muskipSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
muskipSet = readRegIndex >>= setMuskipReg

muskipGet :: (MonadTeXState s m, MonadError String m) => m (Glue MuDimen)
muskipGet = do
  index <- readRegIndex
  use (localState . muskipRegAt index)

muskipdefCommand :: (MonadTeXState s m, MonadError String m, Elem MuskipReg (NValueSet s)) => m (Assignment s)
muskipdefCommand = do
  name <- readCommandName
  readEquals
  index <- readRegIndex
  texAssign (definitionAt name) (nonexpandableToValue $ MuskipReg index)

instance Meaning MuskipReg where
  meaningString (MuskipReg i) = controlSequence "muskip" <> fromString (show i)

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute MuskipReg m where
  doExecute (MuskipReg i)    = runLocal $ setMuskipReg i
  doGlobal (MuskipReg i)     = Just $ runGlobal $ setMuskipReg i
  doArithmetic (MuskipReg i) = Just $ arithmeticQuantity (muskipRegAt i)
  getQuantity (MuskipReg i)  = QMuGlue $ use (localState . muskipRegAt i)

--
-- \toks register
--

-- \toksdef-ed value
newtype ToksReg = ToksReg Int
  deriving (Eq,Show)

toksRegAt :: (IsLocalState localstate) => Int -> Lens' localstate [TeXToken]
toksRegAt !index = toksReg . at index . non []

-- \toksdef-ed token
-- <token variable><equals><general text>
-- or <token variable><equals><filler><token variable>
setToksReg :: forall s m. (MonadTeXState s m, MonadError String m) => Int -> m (Assignment s)
setToksReg !index = do
  readEquals
  value <- doReadTokenList
  texAssign (toksRegAt index) value
  where
    doReadTokenList :: m [TeXToken]
    doReadTokenList = do
      (_,v) <- required nextExpandedToken
      case toCommonValue v of
        Just (Character _ CCBeginGroup)
          -> map fromEToken <$> readUntilEndGroupE LongParam -- <general text>
        Just (Character _ CCSpace) -> doReadTokenList -- optional spaces: ignoed
        Just Relax -> doReadTokenList -- \relax: ignored
        _ | QToks getTokenList <- getQuantity v -> do
              getTokenList
          | otherwise -> throwError "Unexpected token while reading token list" -- Missing { inserted.

-- \toks command
toksSet :: (MonadTeXState s m, MonadError String m) => m (Assignment s)
toksSet = readRegIndex >>= setToksReg

toksGet :: (MonadTeXState s m, MonadError String m) => m [TeXToken]
toksGet = do
  index <- readRegIndex
  use (localState . toksRegAt index)

toksdefCommand :: (MonadTeXState s m, MonadError String m, Elem ToksReg (NValueSet s)) => m (Assignment s)
toksdefCommand = do
  name <- readCommandName
  readEquals
  index <- readRegIndex
  texAssign (definitionAt name) (nonexpandableToValue $ ToksReg index)

instance Meaning ToksReg where
  meaningString (ToksReg i) = controlSequence "toks" <> fromString (show i)

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute ToksReg m where
  doExecute (ToksReg i)   = runLocal $ setToksReg i
  doGlobal (ToksReg i)    = Just $ runGlobal $ setToksReg i
  getQuantity (ToksReg i) = QToks $ use (localState . toksRegAt i)

--
-- Register commands
--

data RegisterCommand = Ecount
                     | Ecountdef
                     | Edimen
                     | Edimendef
                     | Eskip
                     | Eskipdef
                     | Emuskip
                     | Emuskipdef
                     | Etoks
                     | Etoksdef
                     deriving (Eq,Show,Enum,Bounded)

instance IsPrimitive RegisterCommand where
  primitiveName Ecount = "count"
  primitiveName Ecountdef = "countdef"
  primitiveName Edimen = "dimen"
  primitiveName Edimendef = "dimendef"
  primitiveName Eskip = "skip"
  primitiveName Eskipdef = "skipdef"
  primitiveName Emuskip = "muskip"
  primitiveName Emuskipdef = "muskipdef"
  primitiveName Etoks = "toks"
  primitiveName Etoksdef = "toksdef"

instance Meaning RegisterCommand

instance (Monad m, MonadTeXState s m, MonadError String m, SubList '[CountReg,DimenReg,SkipReg,MuskipReg,ToksReg] (NValueSet s), Meaning (NValue s)) => DoExecute RegisterCommand m where
  doExecute Ecount     = runLocal countSet
  doExecute Ecountdef  = runLocal countdefCommand
  doExecute Edimen     = runLocal dimenSet
  doExecute Edimendef  = runLocal dimendefCommand
  doExecute Eskip      = runLocal skipSet
  doExecute Eskipdef   = runLocal skipdefCommand
  doExecute Emuskip    = runLocal muskipSet
  doExecute Emuskipdef = runLocal muskipdefCommand
  doExecute Etoks      = runLocal toksSet
  doExecute Etoksdef   = runLocal toksdefCommand
  doGlobal Ecount      = Just $ runGlobal countSet
  doGlobal Ecountdef   = Just $ runGlobal countdefCommand
  doGlobal Edimen      = Just $ runGlobal dimenSet
  doGlobal Edimendef   = Just $ runGlobal dimendefCommand
  doGlobal Eskip       = Just $ runGlobal skipSet
  doGlobal Eskipdef    = Just $ runGlobal skipdefCommand
  doGlobal Emuskip     = Just $ runGlobal muskipSet
  doGlobal Emuskipdef  = Just $ runGlobal muskipdefCommand
  doGlobal Etoks       = Just $ runGlobal toksSet
  doGlobal Etoksdef    = Just $ runGlobal toksdefCommand
  doArithmetic Ecount  = Just $ do index <- readRegIndex
                                   arithmeticInteger (countRegAt index)
  doArithmetic Edimen  = Just $ do index <- readRegIndex
                                   arithmeticQuantity (dimenRegAt index)
  doArithmetic Eskip   = Just $ do index <- readRegIndex
                                   arithmeticQuantity (skipRegAt index)
  doArithmetic Emuskip = Just $ do index <- readRegIndex
                                   arithmeticQuantity (muskipRegAt index)
  doArithmetic _       = Nothing
  getQuantity Ecount   = QInteger countGet
  getQuantity Edimen   = QDimension dimenGet
  getQuantity Eskip    = QGlue skipGet
  getQuantity Emuskip  = QMuGlue muskipGet
  getQuantity Etoks    = QToks toksGet
  getQuantity _        = NotQuantity

registerCommands :: (SubList '[RegisterCommand,CountReg,DimenReg,SkipReg,MuskipReg,ToksReg] set) => Map.Map Text (Union set)
registerCommands = Map.fromList
  [("count",    liftUnion Ecount)
  ,("countdef", liftUnion Ecountdef)
  ,("dimen",    liftUnion Edimen)
  ,("dimendef", liftUnion Edimendef)
  ,("skip",     liftUnion Eskip)
  ,("skipdef",  liftUnion Eskipdef)
  ,("muskip",   liftUnion Emuskip)
  ,("muskipdef",liftUnion Emuskipdef)
  ,("toks",     liftUnion Etoks)
  ,("toksdef",  liftUnion Etoksdef)
  ]
