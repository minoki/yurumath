{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Text.YuruMath.TeX.LaTeX (latexDefinitions) where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Tokenizer
import Text.YuruMath.TeX.Macro
import Text.YuruMath.TeX.Execution
import Data.Text (Text)
import TypeFun.Data.List (Elem,SubList)
import Data.OpenUnion
import qualified Data.Map as Map

tokenizeStaticString :: String -> [TeXToken]
tokenizeStaticString s = case tokenizeString latexInternalTokenizerContext s of
  Left err -> error $ "Token error in static string: " ++ err
  Right x -> x

mkSimpleMacroWithString :: [MacroParamSpec] -> String -> Macro
mkSimpleMacroWithString p s = mkSimpleMacro p $ tokenizeStaticString s

shortParam, longParam :: MacroParamSpec
shortParam = StandardMandatory ShortParam
longParam = StandardMandatory LongParam

latexDefinitions :: (SubList '[Macro] eset,SubList '[CommonValue,CommonExecutable,MacroCommand] vset) => Map.Map Text (Either (Union eset) (Union vset))
latexDefinitions = Map.fromList
  [
  -- ltplain
   ("dospecials",Left $ liftUnion $ mkSimpleMacroWithString [] "\\do\\ \\do\\\\\\do\\{\\do\\}\\do\\$\\do\\&\\do\\#\\do\\^\\do\\_\\do\\%\\do\\~")
  ,("z@",        Right $ liftUnion $ IntegerConstant 0)
  ,("@ne",       Right $ liftUnion $ IntegerConstant 1)
  ,("m@ne",      Right $ liftUnion $ IntegerConstant (-1))
  ,("tw@",       Right $ liftUnion $ IntegerConstant 2)
  ,("sixt@@n",   Right $ liftUnion $ IntegerConstant 16)
  ,("@cclv",     Right $ liftUnion $ IntegerConstant 255)
  ,("@cclvi",    Right $ liftUnion $ IntegerConstant 256)
  ,("@m",        Right $ liftUnion $ IntegerConstant 1000)
  ,("@M",        Right $ liftUnion $ IntegerConstant 10000)
  ,("@MM",       Right $ liftUnion $ IntegerConstant 20000)
  ,("active",    Right $ liftUnion $ IntegerConstant 13)
  ,("lq",        Left $ liftUnion $ mkSimpleMacroWithString [] "`")
  ,("rq",        Left $ liftUnion $ mkSimpleMacroWithString [] "'")
  ,("lbrack",    Left $ liftUnion $ mkSimpleMacroWithString [] "[")
  ,("rbrack",    Left $ liftUnion $ mkSimpleMacroWithString [] "]")
  ,("space",     Left $ liftUnion $ mkSimpleMacroWithString [] " ")
  ,("bgroup",    Right $ liftUnion $ Character '{' CCBeginGroup)
  ,("egroup",    Right $ liftUnion $ Character '}' CCEndGroup)

  -- ltdefns
  ,("@namedef",      Left $ liftUnion $ mkSimpleMacroWithString [shortParam] "\\expandafter\\def\\csname #1\\endcsname")
  ,("@nameuse",      Left $ liftUnion $ mkSimpleMacroWithString [shortParam] "\\csname #1\\endcsname")
  -- \@cons
  ,("@car",          Left $ liftUnion $ mkSimpleMacroWithString [shortParam, Until ShortParam [TTCommandName (NControlSeq "@nil")]] "#1")
  ,("@cdr",          Left $ liftUnion $ mkSimpleMacroWithString [shortParam, Until ShortParam [TTCommandName (NControlSeq "@nil")]] "#2")
  ,("@carcube",      Left $ liftUnion $ mkSimpleMacroWithString [shortParam, shortParam, shortParam, Until ShortParam [TTCommandName (NControlSeq "@nil")]] "#1#2#3")
  ,("@gobble",       Left $ liftUnion $ mkSimpleMacroWithString [longParam] "")
  ,("@gobbletwo",    Left $ liftUnion $ mkSimpleMacroWithString [longParam, longParam] "")
  ,("@gobblefour",   Left $ liftUnion $ mkSimpleMacroWithString [longParam, longParam, longParam, longParam] "")
  ,("@firstofone",   Left $ liftUnion $ mkSimpleMacroWithString [longParam] "#1")
  ,("@firstoftwo",   Left $ liftUnion $ mkSimpleMacroWithString [longParam, longParam] "#1")
  ,("@secondoftwo",  Left $ liftUnion $ mkSimpleMacroWithString [longParam, longParam] "#2")
  ,("@iden",         Left $ liftUnion $ mkSimpleMacroWithString [longParam] "#1") -- alias for \@firstofone
  ,("@thirdofthree", Left $ liftUnion $ mkSimpleMacroWithString [longParam, longParam, longParam] "#3")
  ,("@backslashchar",Left $ liftUnion $ mkSimpleMacro [] [TTCharacter '\\' CCOther]) -- A category code 12 backslash.
  -- \protect and \robust stuff
  -- ,("@ifundefined")
  ,("@sptoken",      Right $ liftUnion $ Character ' ' CCSpace)
  ,("makeatletter",  Left $ liftUnion $ mkSimpleMacroWithString [] "\\catcode`\\@=11 ") -- catcode 11 = letter
  ,("makeatother",   Left $ liftUnion $ mkSimpleMacroWithString [] "\\catcode`\\@=12 ") -- catcode 12 = other

  -- ltalloc
  ,("@xxxii",Right $ liftUnion $ IntegerConstant 32)
  ,("@Mi",   Right $ liftUnion $ IntegerConstant 10001)
  ,("@Mii",  Right $ liftUnion $ IntegerConstant 10002)
  ,("@Miii", Right $ liftUnion $ IntegerConstant 10003)
  ,("@Miv",  Right $ liftUnion $ IntegerConstant 10004)

  -- ltspace
  -- \nopagebreak, \pagebreak, \linebreak, \nolinebreak, \samepage, \\
  -- \newline, \addvspace, \addpenalty, \vspace
  -- \smallskip, \medskip, \bigskip
  -- \nobreakdashes, \nobreakspace, ~
  -- \, \@
  -- \hspace, \fill, \stretch
  -- \thinspace, \negthinspace, \enspace
  -- \enskip, \quad, \qquad

  -- ltlogos
  -- \TeX, \LaTeX, \LaTeXe

  -- fontdef
  -- fontmath
  -- \DeclareSymbolFont: operators, letters, symbols, largesymbols
  -- \SetSymbolFont: operators, letters, symbols
  -- \DeclareSymbolFontAlphabet: \mathrm: operators, \mathnormal: letters, \mathcal: symbols
  -- \DeclareMathAlphabet: \mathbf, \mathsf, \mathit, \mathtt
  -- greek letters by DeclareMathSymbol

  -- ltmath
  -- \log-like, \bmod, \pmod
  -- \big, \Big, \bigg, \Bigg are defined in fontdef
  ,("bigl",  Left $ liftUnion $ mkSimpleMacroWithString [] "\\mathopen\\big")
  ,("bigm",  Left $ liftUnion $ mkSimpleMacroWithString [] "\\mathrel\\big")
  ,("bigr",  Left $ liftUnion $ mkSimpleMacroWithString [] "\\mathclose\\big")
  ,("Bigl",  Left $ liftUnion $ mkSimpleMacroWithString [] "\\mathopen\\Big")
  ,("Bigm",  Left $ liftUnion $ mkSimpleMacroWithString [] "\\mathrel\\Big")
  ,("Bigr",  Left $ liftUnion $ mkSimpleMacroWithString [] "\\mathclose\\Big")
  ,("biggl", Left $ liftUnion $ mkSimpleMacroWithString [] "\\mathopen\\bigg")
  ,("biggm", Left $ liftUnion $ mkSimpleMacroWithString [] "\\mathrel\\bigg")
  ,("biggr", Left $ liftUnion $ mkSimpleMacroWithString [] "\\mathclose\\bigg")
  ,("Biggl", Left $ liftUnion $ mkSimpleMacroWithString [] "\\mathopen\\Bigg")
  ,("Biggm", Left $ liftUnion $ mkSimpleMacroWithString [] "\\mathrel\\Bigg")
  ,("Biggr", Left $ liftUnion $ mkSimpleMacroWithString [] "\\mathclose\\Bigg")
  -- \jot, \interdisplaylinepenalty
  -- from plain TeX:
  -- Discouraged: \choose, \brack, \brace
  ,("mathpalette", Left $ liftUnion $ mkSimpleMacroWithString [shortParam, shortParam] "\\mathchoice{#1\\displaystyle{#2}}{#1\\textstyle{#2}}{#1\\scriptstyle{#2}}{#1\\scriptscriptstyle{#2}}") -- TODO: cramped style?
  -- \root
  -- \phantom, \mathstrut, \smash
  -- \buildrel#1\over#2
  -- \cases, \matrix, \pmatrix, \bordermatrix
  -- \openup, \displaylines
  -- \> (in ltspace), \;, \!, \*, \:
  ,("sp", Right $ liftUnion $ Character '^' CCSup) -- \let\sp=^
  ,("sb", Right $ liftUnion $ Character '_' CCSub) -- \let\sb=_
  -- \active@math@prime
  -- mode change: \( \), \[ \]
  -- \math, \endmath, \displaymath, \enddisplaymath
  -- equation, \@eqnum
  -- \stackrel
  ,("frac", Left $ liftUnion $ mkSimpleMacroWithString [shortParam, shortParam] "\\Ustack{\\begingroup#1\\endgroup\\over#2}")
  -- \sqrt
  -- eqnarray, eqnarray*
  -- \ensuremath
  ,("ensuremath",   Left $ liftUnion $ mkSimpleMacroWithString [] "\\ifmmode\\expandafter\\@firstofone\\else\\expandafter\\@ensuredmath\\fi") -- TODO: Make robust
  ,("@ensuredmath", Left $ liftUnion $ mkSimpleMacroWithString [longParam] "$\\relax#1$")
  ]
