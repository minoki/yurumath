{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Text.YuruMath.TeX.Expr (ExprCommand,exprCommands) where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Quantity
import Text.YuruMath.TeX.Meaning
import Text.YuruMath.TeX.Expansion
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

instance Meaning ExprCommand where
  meaningString Enumexpr = controlSequence "numexpr"
  meaningString Edimexpr = controlSequence "dimexpr"
  meaningString Eglueexpr = controlSequence "glueexpr"
  meaningString Emuexpr = controlSequence "muexpr"
  meaningString Egluestretch = controlSequence "gluestretch"
  meaningString Eglueshrink = controlSequence "glueshrink"
  meaningString Egluestretchorder = controlSequence "gluestretchorder"
  meaningString Eglueshrinkorder = controlSequence "glueshrinkorder"
  meaningString Emutoglue = controlSequence "mutoglue"
  meaningString Egluetomu = controlSequence "gluetomu"

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
parseExpression :: forall s q m. (MonadTeXState s m, MonadError String m, QuantityRead q) => String -> Int -> m q
parseExpression name !level = parseTerm >>= readAddOp
  where
    readAddOp :: q -> m q
    readAddOp !acc = do
      et <- maybeEvalToken
      case et of
        Just (t,v) ->
          case t of
            ETCharacter { etChar = '+', etCatCode = CCOther } -> do
              y <- parseTerm
              readAddOp (acc <+> y)
            ETCharacter { etChar = '-', etCatCode = CCOther } -> do
              y <- parseTerm
              readAddOp (acc <-> y)
            ETCharacter { etChar = ')', etCatCode = CCOther}
              | level > 0 -> return acc -- end of input
            _ -> case toCommonValue v of
              Just (Character _ CCSpace) -> readAddOp acc
              Just Relax | level == 0 -> return acc -- end of input
                         | otherwise -> throwError $ name ++ ": Unexpected \\relax"
              _ -> unreadETokens 0 [t] >> return acc -- end of factor
        Nothing -> return acc

    parseTerm = parseFactor >>= readMulOp

    readMulOp :: q -> m q
    readMulOp !acc = do
      et <- maybeEvalToken
      case et of
        Just (t,v) ->
          case t of
            ETCharacter { etChar = '*', etCatCode = CCOther } -> do
              y <- parseIntegerFactor name level
              readMulOp (scaleAsInteger (* y) acc)
            ETCharacter { etChar = '/', etCatCode = CCOther } -> do
              y <- parseIntegerFactor name level
              if y == 0
                then throwError $ name ++ ": Divide by zero"
                else readMulOp (scaleAsInteger (`etexDiv` y) acc)
            _ | isImplicitSpace v -> readMulOp acc
              | otherwise -> unreadETokens 0 [t] >> return acc -- end of factor
        Nothing -> return acc

    parseFactor :: m q
    parseFactor = do
      (t,v) <- evalToken
      case t of
        ETCharacter { etChar = '(', etCatCode = CCOther } ->
          parseExpression name (level + 1)
        _ | isImplicitSpace v -> parseFactor
          | otherwise -> unreadETokens 0 [t] >> readQuantity

parseIntegerFactor :: (MonadTeXState s m, MonadError String m) => String -> Int -> m Integer
parseIntegerFactor name !level = do
  (t,v) <- evalToken
  case t of
    ETCharacter { etChar = '(', etCatCode = CCOther } ->
      parseExpression name (level + 1)
    _ | isImplicitSpace v -> parseIntegerFactor name level
      | otherwise -> unreadETokens 0 [t] >> readNumber

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
  doExecute = can'tUseThisCommandInCurrentMode
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
