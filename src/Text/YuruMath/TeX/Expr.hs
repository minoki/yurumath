{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Text.YuruMath.TeX.Expr (ExprCommand,exprCommands) where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Expansion
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.OpenUnion
import TypeFun.Data.List (Elem)

data ExprCommand = Enumexpr
                 | Edimexpr  -- not implemented yet
                 | Eglueexpr -- not implemented yet
                 | Emuexpr   -- not implemented yet
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
-- <integer expr> ::= <integer term> | <integer expr><add or sub><integer term>
-- <integer term> ::= <integer factor> | <integer term><mul or div><integer factor>
-- <integer factor> ::= <number> | <left paren><integer expr><right paren>
-- <add or sub> ::= <optional spaces>'+'12 | <optional spaces>'-'12
-- <mul or div> ::= <optional spaces>'*'12 | <optional spaces>'/'12
-- <left paren> ::= <optional spaces>'('12
-- <right paren> ::= <optional spaces>')'12
parseExpression :: (MonadTeXState s m, MonadError String m) => Int -> m Integer
parseExpression !level = parseTerm >>= readAddOp
  where
    readAddOp !acc = do
      et <- maybeEvalToken
      case et of
        Just (t,v) ->
          case toCommonValue v of
            Just (Character _ CCSpace) -> readAddOp acc
            Just (Character '+' CCOther) -> do y <- parseTerm
                                               readAddOp (acc + y)
            Just (Character '-' CCOther) -> do y <- parseTerm
                                               readAddOp (acc - y)
            Just Relax | level == 0 -> return acc -- end of input
                       | otherwise -> throwError "\\numexpr: Unexpected \\relax"
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
            Just (Character '*' CCOther) -> do y <- parseFactor
                                               readMulOp (acc * y)
            Just (Character '/' CCOther) -> do y <- parseFactor
                                               if y == 0
                                                 then throwError "\\numexpr: Divide by zero"
                                                 else readMulOp (acc `etexDiv` y)
            _ -> unreadETokens 0 [t] >> return acc -- end of factor
        Nothing -> return acc

    parseFactor = do
      (t,v) <- evalToken
      case toCommonValue v of
        Just (Character '(' CCOther) -> parseExpression (level + 1)
        Just (Character _ CCSpace) -> parseFactor
        _ -> unreadETokens 0 [t] >> readNumber

instance (Monad m, MonadTeXState s m, MonadError String m) => DoExecute ExprCommand m where
  doExecute Enumexpr  = throwError "You can't use `\\numexpr' in this mode."
  doExecute Edimexpr  = throwError "\\dimexpr: Not implemented yet"
  doExecute Eglueexpr = throwError "\\glueexpr: Not implemented yet"
  doExecute Emuexpr   = throwError "\\muexpr: Not implemented yet"
  getIntegerValue Enumexpr = Just (parseExpression 0)
  getIntegerValue _ = Nothing

exprCommands :: (Elem ExprCommand set) => Map.Map Text (Union set)
exprCommands = Map.fromList
  [("numexpr", liftUnion Enumexpr)
  ,("dimexpr", liftUnion Edimexpr)
  ,("glueexpr",liftUnion Eglueexpr)
  ,("muexpr",  liftUnion Emuexpr)
  ]
