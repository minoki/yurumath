{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.YuruMath.TeX.MathData where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.State
import Text.YuruMath.TeX.Math
import Data.Text
import qualified Data.Map.Strict as Map

mathCommands :: Map.Map Text MathCode
mathCommands = Map.fromList
  -- Ordinary Symbols
  [("infty",   mkMathCode MathOrd 0 '\x221E')
  ,("forall",  mkMathCode MathOrd 0 '\x2200')
  ,("exists",  mkMathCode MathOrd 0 '\x2203')
  ,("emptyset",mkMathCode MathOrd 0 '\x2205')

  -- Large Operators
  ,("sum",     mkMathCode MathOp 0 '\x2211')
  ,("int",     mkMathCode MathOp 0 '\x222B') -- nolimits
  ,("prod",    mkMathCode MathOp 0 '\x220F')

  -- Binary Symbols
  ,("cap",     mkMathCode MathBin 0 '\x2229')
  ,("cup",     mkMathCode MathBin 0 '\x222A')
  ,("pm",      mkMathCode MathBin 0 '\x00B1')
  ,("mp",      mkMathCode MathBin 0 '\x2213')
  ,("times",   mkMathCode MathBin 0 '\x00D7')

  -- Relations
  ,("mid",       mkMathCode MathRel 0 '|') -- ?
  ,("leftarrow", mkMathCode MathRel 0 '\x2190')
  ,("rightarrow",mkMathCode MathRel 0 '\x2192')
  ,("in",        mkMathCode MathRel 0 '\x2208')
  ,("ni",        mkMathCode MathRel 0 '\x220B')
  ,("leq",       mkMathCode MathRel 0 '\x2264')
  ,("le",        mkMathCode MathRel 0 '\x2264')
  ,("geq",       mkMathCode MathRel 0 '\x2265')
  ,("ge",        mkMathCode MathRel 0 '\x2265')
  ,("subset",    mkMathCode MathRel 0 '\x2282')
  ,("supset",    mkMathCode MathRel 0 '\x2283')

  -- Punctuation symbols
  ,("colon",mkMathCode MathPunct 0 ':')
  ,("cdots",mkMathCode MathPunct 0 '\x22EF')
  ,("vdots",mkMathCode MathPunct 0 '\x22EE')
  ,("ddots",mkMathCode MathPunct 0 '\x22F1')

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
