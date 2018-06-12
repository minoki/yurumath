{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Text.YuruMath.TeX.Expr (ExprCommand,exprCommands) where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Quantity
import Text.YuruMath.TeX.Expansion
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.OpenUnion
import TypeFun.Data.List (Elem)

data ExprCommand = Enumexpr
                 | Edimexpr
                 | Eglueexpr
                 | Emuexpr
                 | Egluestretch
                 | Eglueshrink
                 | Egluestretchorder
                 | Eglueshrinkorder
                 | Emutoglue
                 | Egluetomu
                 deriving (Eq,Show)

-- \numexpr's rounded division:
-- * If x * y > 0, return (floor (x % y + 1 / 2)).
-- * If x * y < 0, return (- floor (- x % y + 1 / 2)).
-- * If y == 0, raise an error.
-- * If x == 0, return 0.
etexDiv :: Integer -> Integer -> Integer
etexDiv !x !y
  | x >= 0, y > 0 =   divPositive   x    y
  | x <  0, y > 0 = - divPositive (-x)   y
  | x >= 0, y < 0 = - divPositive   x  (-y)
  | x <  0, y < 0 =   divPositive (-x) (-y)
  | otherwise     = error "divide by zero"
  where divPositive !x !y = (2 * x + y) `quot` (2 * y)

-- \numexpr<integer expr><optional spaces and \relax>
-- \dimexpr<dimen expr><optional spaces and \relax>
-- \glueexpr<glue expr><optional spaces and \relax>
-- \muexpr<muglue expr><optional spaces and \relax>
-- In the following, Q is one of 'integer' ('number'), 'dimen', 'glue' or 'muglue'.
-- <Q expr> ::= <Q term> | <Q expr><add or sub><Q term>
-- <Q term> ::= <Q factor> | <Q term><mul or div><integer factor>
-- <Q factor> ::= <Q> | <left paren><Q expr><right paren>
-- <add or sub> ::= <optional spaces>'+'12 | <optional spaces>'-'12
-- <mul or div> ::= <optional spaces>'*'12 | <optional spaces>'/'12
-- <left paren> ::= <optional spaces>'('12
-- <right paren> ::= <optional spaces>')'12
parseExpression :: (MonadTeXState s m, MonadError String m, QuantityRead q) => String -> Int -> m q
parseExpression name !level = parseTerm >>= readAddOp
  where
    readAddOp !acc = do
      et <- maybeEvalToken
      case et of
        Just (t,v) ->
          case toCommonValue v of
            Just (Character _ CCSpace) -> readAddOp acc
            Just (Character '+' CCOther) -> do y <- parseTerm
                                               readAddOp (acc <+> y)
            Just (Character '-' CCOther) -> do y <- parseTerm
                                               readAddOp (acc <-> y)
            Just Relax | level == 0 -> return acc -- end of input
                       | otherwise -> throwError $ name ++ ": Unexpected \\relax"
            Just (Character ')' CCOther) | level > 0 -> return acc -- end of input
            _ -> unreadETokens 0 [t] >> return acc -- end of factor
        Nothing -> return acc

    parseTerm = parseFactor >>= readMulOp

    readMulOp !acc = do
      et <- maybeEvalToken
      case et of
        Just (t,v) ->
          case toCommonValue v of
            Just (Character _ CCSpace) -> readMulOp acc
            Just (Character '*' CCOther) -> do y <- parseIntegerFactor name level
                                               readMulOp (scaleAsInteger (* y) acc)
            Just (Character '/' CCOther) -> do y <- parseIntegerFactor name level
                                               if y == 0
                                                 then throwError $ name ++ ": Divide by zero"
                                                 else readMulOp (scaleAsInteger (`etexDiv` y) acc)
            _ -> unreadETokens 0 [t] >> return acc -- end of factor
        Nothing -> return acc

    parseFactor = do
      (t,v) <- evalToken
      case toCommonValue v of
        Just (Character '(' CCOther) -> parseExpression name (level + 1)
        Just (Character _ CCSpace) -> parseFactor
        _ -> unreadETokens 0 [t] >> readQuantity

parseIntegerFactor :: (MonadTeXState s m, MonadError String m) => String -> Int -> m Integer
parseIntegerFactor name !level = do
  (t,v) <- evalToken
  case toCommonValue v of
    Just (Character '(' CCOther) -> parseExpression name (level + 1)
    Just (Character _ CCSpace) -> parseIntegerFactor name level
    _ -> unreadETokens 0 [t] >> readNumber

ssToDimen :: StretchShrink Dimen -> Dimen
ssToDimen (FixedSS dimen) = dimen
ssToDimen (InfiniteSS i _) = DimenWithSp i

ssToOrder :: StretchShrink Dimen -> Integer
ssToOrder (FixedSS _) = 0
ssToOrder (InfiniteSS _ l) = fromIntegral (l + 1)

mutoglue :: Glue MuDimen -> Glue Dimen
mutoglue = fmap (DimenWithSp . asScaledMu)

gluetomu :: Glue Dimen -> Glue MuDimen
gluetomu = fmap (DimenWithScaledMu . asScaledPoints)

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute ExprCommand m where
  doExecute Enumexpr  = throwError "You can't use `\\numexpr' in this mode."
  doExecute Edimexpr  = throwError "You can't use `\\dimexpr' in this mode."
  doExecute Eglueexpr = throwError "You can't use `\\glueexpr' in this mode."
  doExecute Emuexpr   = throwError "You can't use `\\muexpr' in this mode."
  doExecute Egluestretch      = throwError "You can't use `\\gluestretch' in this mode."
  doExecute Eglueshrink       = throwError "You can't use `\\glueshrink' in this mode."
  doExecute Egluestretchorder = throwError "You can't use `\\gluestretchorder' in this mode."
  doExecute Eglueshrinkorder  = throwError "You can't use `\\glueshrinkorder' in this mode."
  doExecute Emutoglue         = throwError "You can't use `\\mutoglue' in this mode."
  doExecute Egluetomu         = throwError "You can't use `\\gluetomu' in this mode."
  getQuantity Enumexpr  = QInteger   (parseExpression "\\numexpr" 0)
  getQuantity Edimexpr  = QDimension (parseExpression "\\dimexpr" 0)
  getQuantity Eglueexpr = QGlue      (parseExpression "\\glueexpr" 0)
  getQuantity Emuexpr   = QMuGlue    (parseExpression "\\muexpr" 0)
  getQuantity Egluestretch      = QDimension ((ssToDimen . glueStretch) <$> readGlue)
  getQuantity Eglueshrink       = QDimension ((ssToDimen . glueShrink) <$> readGlue)
  getQuantity Egluestretchorder = QInteger ((ssToOrder . glueStretch) <$> readGlue)
  getQuantity Eglueshrinkorder  = QInteger ((ssToOrder . glueShrink) <$> readGlue)
  getQuantity Emutoglue         = QGlue (mutoglue <$> readMuGlue)
  getQuantity Egluetomu         = QMuGlue (gluetomu <$> readGlue)

exprCommands :: (Elem ExprCommand set) => Map.Map Text (Union set)
exprCommands = Map.fromList
  [("numexpr", liftUnion Enumexpr)
  ,("dimexpr", liftUnion Edimexpr)
  ,("glueexpr",liftUnion Eglueexpr)
  ,("muexpr",  liftUnion Emuexpr)
  ,("gluestretch",     liftUnion Egluestretch)
  ,("glueshrink",      liftUnion Eglueshrink)
  ,("gluestretchorder",liftUnion Egluestretchorder)
  ,("glueshrinkorder", liftUnion Eglueshrinkorder)
  ,("mutoglue",        liftUnion Emutoglue)
  ,("gluetomu",        liftUnion Egluetomu)
  ]
