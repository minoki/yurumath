# YuruMath

YuruMath is an implementation of the TeX language (without a complete visual processor) in Haskell.
This is developed with the intention to emit MathML from (La)TeX formula.

## Play with REPL

```sh
$ git clone https://github.com/minoki/yurumath.git
$ cd yurumath/
$ stack build --fast
$ stack exec yurumath-repl
*
```

`*` is the prompt (as in TeX).

Now let's type something:

```tex
*\message{Hello world!}
Hello world!
*
```

Some equation:

```tex
*\frac{a}{b}
MathML: <math>
    <mfrac>
        <mi>
            a
        </mi>
        <mi>
            b
        </mi>
    </mfrac>
</math>

*
```

Integers beyond 2<sup>32</sup> are supported:

```tex
*\def\fact#1{\ifnum#1=0 1 \else\numexpr#1*\expandafter\fact\expandafter{\number\numexpr#1-1\relax}\relax\fi}
*\message{\number\fact{20}}
2432902008176640000
*\message{\number"100000000 }
4294967296
```

The interpreter tries to prevent infinite loop:

```tex
*\def\foo{\foo} \foo
error: recursion too deep
*\def\foo{\foo\foo\foo} \foo
error: token list too long
*\def\foo{\uppercase{\foo}} \foo
error: recursion too deep
*\def\foo{'} {\catcode`\'=\active \global\let'=\foo} '
error: recursion too deep
```

## Supported TeX primitives

Some commands are from e-TeX or XeTeX/LuaTeX.

### Expandable commands

- `\expandafter`, `\noexpand`, `\csname`, `\string`, `\number`, `\romannumeral`, `\the`, `\meaning`
- `\if`, `\ifcat`, `\ifx`, `\iftrue`, `\iffalse`, `\ifnum`, `\ifdim`, `\ifodd`, `\ifcase`
- `\ifhmode`, `\ifvmode`, `\ifmmode`, `\ifinner`, `\ifdefined`, `\ifcsname`
- `\unless`, `\else`, `\fi`, `\or`
- `\begincsname`, `\csstring`, `\Uchar`, `\mathstyle`
- `\unexpanded`, `\detokenize`, `\strcmp` (or `\pdfstrcmp`), `\expanded`

### Non-expandable commands

- `\relax`, `\endcsname`, `\global`
- `\uppercase`, `\lowercase`
- `\let`, `\futurelet`
- `\def`, `\edef`, `\gdef`, `\xdef`, `\outer`, `\long`, `\protected`
- `\chardef`, `\mathchardef`, `\Umathchardef`, `\Umathcharnumdef`
- `\catcode`, `\lccode`, `\uccode`, `\mathcode`, `\Umathcode`, `\Umathcodenum`, `\delcode`, `\Udelcode`, `\Udelcodenum`
- `\begingroup`, `\endgroup`
- `\endlinechar`, `\escapechar`
- `\count`, `\countdef`, `\dimen`, `\dimendef`, `\skip`, `\skipdef`, `\muskip`, `\muskipdef`
- `\advance`, `\multiply`, `\divide`
- `\numexpr`, `\dimexpr`, `\glueexpr`, `\muexpr`
- `\gluestretch`, `\glueshrink`, `\gluestretchorder`, `\glueshrinkorder`, `\mutoglue`, `\gluetomu`
- `\message`, `\show`, `\showthe`, `\showtokens`
- `\char`, `\kern`, `\unkern`, `\unskip`, `\hskip`, `\hfil`, `\hfill`, `\hss`, `\hfilneg`, `\noindent`, `\ `, `\/`
- `\newcommand`, `\renewcommand`, `\providecommand` (from LaTeX)

Math mode commands:

- `\mathchar`, `\Umathchar`, `\Umathcharnum`, `\delimiter`, `\Udelimiter`, `\mathaccent`, `\Umathaccent`, `\radical`, `\Uradical`, `\Uroot`
- `\mathord`, `\mathop`, `\mathbin`, `\mathrel`, `\mathopen`, `\mathclose`, `\mathpunct`, `\mathinner`
- `\underline`, `\overline`, `\vcenter`
- `\displaylimits`, `\limits`, `\nolimits`
- `\left`, `\middle`, `\right`, `\Uleft`, `\Umiddle`, `\Uright`
- `\over`, `\atop`, `\above`, `\Uskewed`, `\overwithdelims`, `\atopwithdelims`, `\abovewithdelims`, `\Uskewedwithdelims`
- `\mathchoice`, `\Ustack`
- `\fam`, `\thinmuskip`, `\medmuskip`, `\thickmuskip`
- `\mkern`, `\mskip`, `\nonscript`
- `\Usuperscript`, `\Usubscript`, `\Ustopmath`, `\Ustopdisplaymath`
- `\displaystyle`, `\textstyle`, `\scriptstyle`, `\scriptscriptstyle`, `\crampeddisplaystyle`, `\crampedtextstyle`, `\crampedscriptstyle`, `\crampedscriptscriptstyle`

## Incompatiblities with TeX

* `\over`-like commands must be prefixed by `\Ustack`, like `\Ustack{1\over2}`.
* `^^` notation in the control word (e.g. `\f^^6fo` = `\foo`) is not supported.
