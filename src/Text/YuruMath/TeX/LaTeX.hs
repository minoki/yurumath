{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Text.YuruMath.TeX.LaTeX where
import Text.YuruMath.TeX.Types
import Text.YuruMath.TeX.Tokenizer
import Text.YuruMath.TeX.Macro
import Text.YuruMath.TeX.State
import Data.Text (Text)
import TypeFun.Data.List (Elem,SubList)
import Data.OpenUnion
import qualified Data.Map.Strict as Map

tokenizeStaticString :: String -> [TeXToken]
tokenizeStaticString s = case tokenizeString latexInternalTokenizerContext s of
  Left err -> error $ "Token error in static string: " ++ err
  Right x -> x

mkSimpleMacroWithString :: [MacroParamSpec] -> String -> Macro
mkSimpleMacroWithString p s = mkSimpleMacro p $ tokenizeStaticString s

shortParam, longParam :: MacroParamSpec
shortParam = StandardMandatory ShortParam
longParam = StandardMandatory LongParam

optionalStar :: [TeXToken] -> [TeXToken] -> MacroParamSpec
optionalStar ifTrue ifFalse = OptionalChar
  { paramSpecConsumeSpace = ConsumeSpaces
  , paramSpecToken = TTCharacter '*' CCOther
  , paramSpecIfTrue = ifTrue
  , paramSpecIfFalse = ifFalse
  }

protected :: Macro -> Macro
protected m = m { macroIsProtected = True }

operatornameM :: String -> LimitsSpec -> Macro
operatornameM name DisplayLimits = mkSimpleMacroWithString [] $ "\\mathop{\\YuruMathSetText\\YuruMathSetFunctionName " ++ name ++ "}"
operatornameM name NoLimits = mkSimpleMacroWithString [] $ "\\mathop{\\YuruMathSetText\\YuruMathSetFunctionName " ++ name ++ "}\\nolimits"
operatornameM name Limits = mkSimpleMacroWithString [] $ "\\mathop{\\YuruMathSetText\\YuruMathSetFunctionName " ++ name ++ "}\\limits"
operatorname :: (Elem Macro eset) => String -> LimitsSpec -> Either (Union eset) v
operatorname !name !limits = Left $ liftUnion $ operatornameM name limits

latexDefinitions :: (SubList '[Macro] eset,SubList '[CommonValue,MacroCommand] vset) => Map.Map Text (Either (Union eset) (Union vset))
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
  ,("\r",        Left $ liftUnion $ mkSimpleMacroWithString [] "\\ ") -- \^^M
  ,("\t",        Left $ liftUnion $ mkSimpleMacroWithString [] "\\ ") -- \let\^^I=\^^M
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
  ,("@ifundefined",   Left $ liftUnion $ mkSimpleMacroWithString [shortParam]
                      "\\expandafter\\ifx\\csname#1\\endcsname\\relax\\expandafter\\@firstoftwo\\else\\expandafter\\@secondoftwo\\fi")
  ,("@qend",         Left $ liftUnion $ mkSimpleMacro [] $ map (flip TTCharacter CCOther) "end") -- "end" \catcode-d 12
  ,("@qrelax",       Left $ liftUnion $ mkSimpleMacro [] $ map (flip TTCharacter CCOther) "relax") -- "relax" \catcode-d 12
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
  ,(",", Left $ liftUnion $ protected $ mkSimpleMacroWithString [] "\\relax\\ifmmode\\mskip\\thinmuskip\\else\\thinspace\\fi")
  ,("@", Left $ liftUnion $ mkSimpleMacroWithString [] "\\spacefactor\\@m{}")
  -- \hspace, \fill, \stretch
  ,("thinspace", Left $ liftUnion $ mkSimpleMacroWithString [] "\\kern .16667em ")
  ,("negthinspace", Left $ liftUnion $ mkSimpleMacroWithString [] "\\kern-.16667em ")
  ,("enspace", Left $ liftUnion $ mkSimpleMacroWithString [] "\\kern.5em ")
  ,("enskip", Left $ liftUnion $ mkSimpleMacroWithString [] "\\hskip.5em\\relax")
  ,("quad", Left $ liftUnion $ mkSimpleMacroWithString [] "\\hskip1em\\relax")
  ,("qquad", Left $ liftUnion $ mkSimpleMacroWithString [] "\\hskip2em\\relax")

  -- ltlogos
  -- \TeX, \LaTeX, \LaTeXe

  -- fontdef
  -- fontmath
  -- \DeclareSymbolFont: operators, letters, symbols, largesymbols
  -- \SetSymbolFont: operators, letters, symbols
  -- \DeclareSymbolFontAlphabet: \mathrm: operators, \mathnormal: letters, \mathcal: symbols
  -- \DeclareMathAlphabet: \mathbf, \mathsf, \mathit, \mathtt
  -- greek letters by DeclareMathSymbol

  -- Ordinary Symbols
  ,("partial", Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathOrd 0 '\x2202')
  ,("infty",   Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathOrd 0 '\x221E')
  ,("prime",   Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathOrd 0 '\x2032')
  ,("emptyset",Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathOrd 0 '\x2205')
  ,("top",     Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathOrd 0 '\x22A4')
  ,("bot",     Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathOrd 0 '\x22A5')
  ,("forall",  Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathOrd 0 '\x2200')
  ,("exists",  Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathOrd 0 '\x2203')
  ,("neg",     Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathOrd 0 '\x00AC')
  ,("lnot",    Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathOrd 0 '\x00AC')

  -- Large Operators
  ,("intop",   Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathOp 0 '\x222B') -- nolimits
  ,("int",     Left $ liftUnion $ mkSimpleMacroWithString [] "\\intop\\nolimits")
  ,("prod",    Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathOp 0 '\x220F')
  ,("sum",     Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathOp 0 '\x2211')

  -- Binary Symbols
  ,("cap",     Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathBin 0 '\x2229')
  ,("cup",     Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathBin 0 '\x222A')
  ,("div",     Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathBin 0 '\x00F7')
  ,("otimes",  Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathBin 0 '\x2297')
  ,("oplus",   Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathBin 0 '\x2295')
  ,("mp",      Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathBin 0 '\x2213')
  ,("pm",      Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathBin 0 '\x00B1')
  ,("times",   Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathBin 0 '\x00D7')

  -- Relations
  ,("mid",       Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathRel 0 '|') -- ?
  ,("leftarrow", Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathRel 0 '\x2190')
  ,("rightarrow",Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathRel 0 '\x2192')
  ,("in",        Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathRel 0 '\x2208')
  ,("ni",        Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathRel 0 '\x220B')
  ,("leq",       Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathRel 0 '\x2264')
  ,("le",        Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathRel 0 '\x2264')
  ,("geq",       Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathRel 0 '\x2265')
  ,("ge",        Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathRel 0 '\x2265')
  ,("subset",    Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathRel 0 '\x2282')
  ,("supset",    Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathRel 0 '\x2283')

  -- Punctuation symbols
  ,("colon",Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathPunct 0 ':')
  ,("cdots",Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathPunct 0 '\x22EF')
  ,("vdots",Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathPunct 0 '\x22EE')
  ,("ddots",Right $ liftUnion $ DefinedMathCharacter $ mkUMathCode MathPunct 0 '\x22F1')

  -- TODO: Accents
  -- TODO: Radicals
  ,("sqrtsign",Left $ liftUnion $ mkSimpleMacroWithString [] "\\Uradical \"0 \"221A ")

  ,("mathnormal", Left $ liftUnion $ protected $ mkSimpleMacroWithString [shortParam] "{\\YuruMathSetSymbol\\YuruMathSetItalic#1}") -- robust
  ,("mathrm", Left $ liftUnion $ protected $ mkSimpleMacroWithString [shortParam] "{\\YuruMathSetText\\YuruMathSetNormal#1}") -- robust
  ,("mathit", Left $ liftUnion $ protected $ mkSimpleMacroWithString [shortParam] "{\\YuruMathSetText\\YuruMathSetItalic#1}") -- robust
  ,("mathbf", Left $ liftUnion $ protected $ mkSimpleMacroWithString [shortParam] "{\\YuruMathSetText\\YuruMathSetBold#1}") -- robust
  ,("mathsf", Left $ liftUnion $ protected $ mkSimpleMacroWithString [shortParam] "{\\YuruMathSetText\\YuruMathSetSansSerif#1}") -- robust
  ,("mathtt", Left $ liftUnion $ protected $ mkSimpleMacroWithString [shortParam] "{\\YuruMathSetText\\YuruMathSetMonospace#1}") -- robust
  ,("mathcal", Left $ liftUnion $ protected $ mkSimpleMacroWithString [shortParam] "{\\YuruMathSetText\\YuruMathSetScript#1}") -- robust
  -- \mathbb, \mathfrak, \mathscr are defined by amsfonts
  ,("mathbb", Left $ liftUnion $ protected $ mkSimpleMacroWithString [shortParam] "{\\YuruMathSetText\\YuruMathSetDoubleStruck#1}") -- robust
  ,("mathfrak", Left $ liftUnion $ protected $ mkSimpleMacroWithString [shortParam] "{\\YuruMathSetText\\YuruMathSetFraktur#1}") -- robust
  ,("mathscr", Left $ liftUnion $ protected $ mkSimpleMacroWithString [shortParam] "{\\YuruMathSetText\\YuruMathSetScript#1}") -- robust
  ,("operatorname", Left $ liftUnion $ protected $ mkSimpleMacroWithString [optionalStar (tokenizeStaticString "") (tokenizeStaticString "\\nolimits"), shortParam] "\\mathop{\\YuruMathSetText\\YuruMathSetFunctionName#2}#1") -- robust

  -- \def\big#1{{\hbox{$\left#1\vbox to8.5\p@{}\right.\n@space$}}} -- \p@ is a \dimen set to 1pt
  -- \def\Big#1{{\hbox{$\left#1\vbox to11.5\p@{}\right.\n@space$}}}
  -- \def\bigg#1{{\hbox{$\left#1\vbox to14.5\p@{}\right.\n@space$}}}
  -- \def\Bigg#1{{\hbox{$\left#1\vbox to17.5\p@{}\right.\n@space$}}}
  -- \def\n@space{\nulldelimiterspace\z@ \m@th} -- \m@th is defined in ltplain.dtx as \mathsurrond\z@
  ,("big", Left $ liftUnion $ mkSimpleMacroWithString [shortParam] "{\\YuruMathSizedDelimiter 10pt #1}")
  ,("Big", Left $ liftUnion $ mkSimpleMacroWithString [shortParam] "{\\YuruMathSizedDelimiter 16pt #1}")
  ,("bigg", Left $ liftUnion $ mkSimpleMacroWithString [shortParam] "{\\YuruMathSizedDelimiter 22pt #1}")
  ,("Bigg", Left $ liftUnion $ mkSimpleMacroWithString [shortParam] "{\\YuruMathSizedDelimiter 28pt #1}")

  -- ltmath
  ,("log",   operatorname "log"    NoLimits)
  ,("lg",    operatorname "lg"     NoLimits)
  ,("ln",    operatorname "ln"     NoLimits)
  ,("lim",   operatorname "lim"    DisplayLimits)
  ,("limsup",operatorname "lim\\,sup" DisplayLimits) -- ?
  ,("liminf",operatorname "lim\\,inf" DisplayLimits) -- ?
  ,("sin",   operatorname "sin"    NoLimits)
  ,("arcsin",operatorname "arcsin" NoLimits)
  ,("sinh",  operatorname "sinh"   NoLimits)
  ,("cos",   operatorname "cos"    NoLimits)
  ,("arccos",operatorname "arccos" NoLimits)
  ,("cosh",  operatorname "cosh"   NoLimits)
  ,("tan",   operatorname "tan"    NoLimits)
  ,("arctan",operatorname "arctan" NoLimits)
  ,("tanh",  operatorname "tanh"   NoLimits)
  ,("cot",   operatorname "cot"    NoLimits)
  ,("coth",  operatorname "coth"   NoLimits)
  ,("sec",   operatorname "sec"    NoLimits)
  ,("csc",   operatorname "csc"    NoLimits)
  ,("max",   operatorname "max"    DisplayLimits)
  ,("min",   operatorname "min"    DisplayLimits)
  ,("sup",   operatorname "sup"    DisplayLimits)
  ,("inf",   operatorname "inf"    DisplayLimits)
  ,("arg",   operatorname "arg"    NoLimits)
  ,("ker",   operatorname "ker"    NoLimits)
  ,("dim",   operatorname "dim"    NoLimits)
  ,("hom",   operatorname "hom"    NoLimits)
  ,("det",   operatorname "det"    DisplayLimits)
  ,("exp",   operatorname "exp"    NoLimits)
  ,("Pr",    operatorname "Pr"     DisplayLimits)
  ,("gcd",   operatorname "gcd"    DisplayLimits)
  ,("deg",   operatorname "deg"    NoLimits)
  -- ("bmod", Left $ liftUnion $ mkSimpleMacroWithString [] "\\nonscript\\mskip-\\medmuskip\\mkern5mu\\mathbin{\\operator@font mod}\\penalty900\\mkern5mu\\nonscript\\mskip-\\medmuskip")
  -- ("pmod", Left $ liftUnion $ mkSimpleMacroWithString [shortParam] "\\allowbreak\\mkern18mu({\\operator@font mod}\,\,#1)")
  -- \bmod, \pmod
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
  -- \,: already defined in ltspace
  ,(">", Left $ liftUnion $ mkSimpleMacroWithString [] "\\mskip\\medmuskip")
  ,(";", Left $ liftUnion $ mkSimpleMacroWithString [] "\\mskip\\thickmuskip")
  ,("!", Left $ liftUnion $ mkSimpleMacroWithString [] "\\mskip-\\thinmuskip")
  ,(":", Left $ liftUnion $ mkSimpleMacroWithString [] "\\mskip\\medmuskip") -- \let\:=\>
  -- ,("*", Left $ liftUnion $ mkSimpleMacroWithString [] "\\discretionary{...}{}{}") -- invisible times
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
  ,("ensuremath",   Left $ liftUnion $ protected $ mkSimpleMacroWithString [] "\\ifmmode\\expandafter\\@firstofone\\else\\expandafter\\@ensuredmath\\fi")
  ,("@ensuredmath", Left $ liftUnion $ mkSimpleMacroWithString [longParam] "$\\relax#1$")
  ]
