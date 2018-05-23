{-# LANGUAGE FlexibleContexts #-}
module Text.YuruMath.TeX.MathData where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Math
import Data.Text
import qualified Data.Map.Strict as Map

mathCommands :: Map.Map Text MathCode
mathCommands = Map.fromList
  -- Ordinary Symbols
  [("infty",   mkUMathCode MathOrd 0 '\x221E')
  ,("forall",  mkUMathCode MathOrd 0 '\x2200')
  ,("exists",  mkUMathCode MathOrd 0 '\x2203')
  ,("emptyset",mkUMathCode MathOrd 0 '\x2205')

  -- Large Operators
  ,("sum",     mkUMathCode MathOp 0 '\x2211')
  ,("int",     mkUMathCode MathOp 0 '\x222B') -- nolimits
  ,("prod",    mkUMathCode MathOp 0 '\x220F')

  -- Binary Symbols
  ,("cap",     mkUMathCode MathBin 0 '\x2229')
  ,("cup",     mkUMathCode MathBin 0 '\x222A')
  ,("pm",      mkUMathCode MathBin 0 '\x00B1')
  ,("mp",      mkUMathCode MathBin 0 '\x2213')
  ,("times",   mkUMathCode MathBin 0 '\x00D7')

  -- Relations
  ,("mid",       mkUMathCode MathRel 0 '|') -- ?
  ,("leftarrow", mkUMathCode MathRel 0 '\x2190')
  ,("rightarrow",mkUMathCode MathRel 0 '\x2192')
  ,("in",        mkUMathCode MathRel 0 '\x2208')
  ,("ni",        mkUMathCode MathRel 0 '\x220B')
  ,("leq",       mkUMathCode MathRel 0 '\x2264')
  ,("le",        mkUMathCode MathRel 0 '\x2264')
  ,("geq",       mkUMathCode MathRel 0 '\x2265')
  ,("ge",        mkUMathCode MathRel 0 '\x2265')
  ,("subset",    mkUMathCode MathRel 0 '\x2282')
  ,("supset",    mkUMathCode MathRel 0 '\x2283')

  -- Punctuation symbols
  ,("colon",mkUMathCode MathPunct 0 ':')
  ,("cdots",mkUMathCode MathPunct 0 '\x22EF')
  ,("vdots",mkUMathCode MathPunct 0 '\x22EE')
  ,("ddots",mkUMathCode MathPunct 0 '\x22F1')

  -- TODO: Accents
  -- TODO: Radicals
  ]

mathFunctions :: [(Text,LimitsSpec)]
mathFunctions = [("log",   NoLimits)
                ,("lg",    NoLimits)
                ,("ln",    NoLimits)
                ,("lim",   Limits)
                ,("sin",   NoLimits)
                ,("arcsin",NoLimits)
                ,("sinh",  NoLimits)
                ,("cos",   NoLimits)
                ,("arccos",NoLimits)
                ,("cosh",  NoLimits)
                ,("tan",   NoLimits)
                ,("arctan",NoLimits)
                ,("tanh",  NoLimits)
                ,("cot",   NoLimits)
                ,("coth",  NoLimits)
                ,("sec",   NoLimits)
                ,("csc",   NoLimits)
                ,("max",   Limits)
                ,("min",   Limits)
                ,("sup",   Limits)
                ,("inf",   Limits)
                ,("arg",   NoLimits)
                ,("ker",   NoLimits)
                ,("dim",   NoLimits)
                ,("hom",   NoLimits)
                ,("det",   Limits)
                ,("exp",   NoLimits)
                ,("Pr",    Limits)
                ,("gcd",   Limits)
                ,("deg",   NoLimits)
                -- amsmath:
                ]
-- \limsup: lim\,sup
-- \liminf: lim\,inf
-- \bmod, \pmod
-- amsmath:
-- \injlim, \projlim, \varlimsup, \varliminf, \varinjlim, \varprojlim
-- \mod, \pod
