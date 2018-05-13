import Control.Arrow ((&&&))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>),(<.>))
import Data.Char
import Data.List
import Data.Ord
import qualified Data.Set as Set

data MMLVariant
  = MMLVariant
    { version :: [String]
    , parents :: [String]
    , unaryParents :: [String]
    , binaryParents :: [String]
    , ternaryParents :: [String]
    , leafs :: [String]
    , attributes :: [String]
    }

writeMMLVariant :: MMLVariant -> IO ()
writeMMLVariant mmlVariant = do
  createDirectoryIfMissing True basePath

  let tags = map (id &&& makeParent) (parents mmlVariant ++ unaryParents mmlVariant)
        ++ map (id &&& makeBinary) (binaryParents mmlVariant)
        ++ map (id &&& makeTernary) (ternaryParents mmlVariant)
        ++ map (id &&& makeLeaf) (leafs mmlVariant)
      sortedTags = sortBy (comparing fst) tags

  writeFile (basePath <.> "hs") $ trimEmptyLinesAtEnd $ unlines
    [ "-- DO NOT EDIT THIS FILE!  This file was generated by tools/GenerateMMLCombinators.hs."
    , ""
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "module " ++ moduleName
    , generateExports ("module Text.Blaze" : map (sanitize . fst) sortedTags) ++ " where"
    , "import Text.Blaze"
    , "import Text.Blaze.Internal"
    , "import Data.Semigroup ((<>))"
    , ""
    , "type MathML = Markup"
    , ""
    , "toMathML :: ToMarkup a => a -> MathML"
    , "toMathML = toMarkup"
    , ""
    , unlines $ map snd sortedTags
    ]

  let sortedAttributes = Set.toAscList $ Set.fromList $ attributes mmlVariant

  writeFile (basePath </> "Attributes.hs") $ trimEmptyLinesAtEnd $ unlines
    [ "-- DO NOT EDIT THIS FILE!  This file was generated by tools/GenerateMMLCombinators.hs."
    , ""
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "module " ++ attributeModuleName ++ " where"
    , "import Prelude ()"
    , "import Text.Blaze.Internal (Attribute, AttributeValue, attribute)"
    , ""
    , unlines $ map makeAttribute sortedAttributes
    ]
  where
    basePath = "src" </> "Text" </> "YuruMath" </> "Builder" </> foldr1 (</>) (version mmlVariant)
    moduleName = "Text.YuruMath.Builder." ++ intercalate "." (version mmlVariant)
    attributeModuleName = "Text.YuruMath.Builder." ++ intercalate "." (version mmlVariant) ++ ".Attributes"
    trimEmptyLinesAtEnd "" = ""
    trimEmptyLinesAtEnd ('\n':'\n':rest) | all isSpace rest = "\n"
    trimEmptyLinesAtEnd (x:xs) = x : trimEmptyLinesAtEnd xs

generateExports :: [String] -> String
generateExports list = "  (" ++ intercalate "\n  ," list ++ "\n  )"

makeParent :: String -> String
makeParent name = unlines
  [ fname ++ " :: MathML -> MathML"
  , fname ++ " = Parent " ++ show name ++ " " ++ show ("<" ++ name) ++ " " ++ show ("</" ++ name ++ ">")
  ]
  where fname = sanitize name

makeBinary :: String -> String
makeBinary name = unlines
  [ fname ++ " :: MathML -> MathML -> MathML"
  , fname ++ " a b = Parent " ++ show name ++ " " ++ show ("<" ++ name) ++ " " ++ show ("</" ++ name ++ ">") ++ " (a <> b)"
  ]
  where fname = sanitize name

makeTernary :: String -> String
makeTernary name = unlines
  [ fname ++ " :: MathML -> MathML -> MathML -> MathML"
  , fname ++ " a b c = Parent " ++ show name ++ " " ++ show ("<" ++ name) ++ " " ++ show ("</" ++ name ++ ">") ++ " (a <> b <> c)"
  ]
  where fname = sanitize name

makeLeaf :: String -> String
makeLeaf name = unlines
  [ fname ++ " :: MathML"
  , fname ++ " = Leaf " ++ show name ++ " " ++ show ("<" ++ name) ++ " " ++ show (" />") ++ " ()"
  ]
  where fname = sanitize name

makeAttribute :: String -> String
makeAttribute name = unlines
  [ fname ++ " :: AttributeValue -> Attribute"
  , fname ++ " = attribute " ++ show name ++ " " ++ show (" " ++ name ++ "=\"")
  ]
  where fname = sanitize name

mathml3 :: MMLVariant
mathml3 = MMLVariant
  { version = ["MathML3"]
  , parents =
      [ "math", "semantics", "annotation", "annotation-xml"
      , "mi", "mn", "mo", "mtext", "ms"
      , "mrow", "mfenced", "mmultiscripts"
      , "mtable", "mtr", "mlabeledtr"
      , "mstack", "mlongdiv", "msgroup", "msrow", "mscarries", "mscarry"
      , "maction"
      ]
  , unaryParents = -- possibly inferred mrow
      [ "msqrt", "mstyle", "merror", "mpadded", "mphantom", "menclose", "mtd"
      ]
  , binaryParents =
      [ "mfrac", "mroot", "msub", "msup", "munder", "mover"
      ]
  , ternaryParents =
      [ "msubsup", "munderover"
      ]
  , leafs =
      [ "mglyph", "mspace"
      , "mprescripts", "none"
      , "maligngroup", "malignmark"
      , "msline"
      ]
  , attributes =
      [ "id", "xref", "class", "style", "href"

      -- math element
      , "display", "maxwidth", "overflow"
      , "altimg", "altimg-width", "altimg-height", "altimg-valign"
      , "alttext", "cdgroup"

      -- common to presentation elements
      , "mathcolor", "mathbackground"

      -- mglyph
      , "src", "width", "height", "valign", "alt"

      -- common to token elements
      , "mathvariant", "mathsize", "dir"

      -- mo
      , "form", "fence", "separator", "lspace", "rspace", "stretchy"
      , "symmetric", "maxsize", "minsize", "largeop", "movablelimits", "accent"
      , "linebreak", "lineleading", "linebreakstyle", "linebreakmultchar"
      , "indentalign", "indentshift", "indenttarget", "indentalignfirst"
      , "indentshiftfirst", "indentalignlast", "indentshiftlast"

      -- mspace
      , "width", "height", "depth", "linebreak"

      -- ms
      , "lquote", "rquote"

      -- mfrac
      , "linethickness", "numalign", "denomalign", "bevelled"

      -- mstyle
      , "scriptlevel", "displaystyle", "scriptsizemultiplier", "scriptminsize"
      , "infixlinebreakstyle", "decimalpoint"

      -- mpadded
      , "height", "depth", "width", "lspace", "voffset"

      -- mfenced
      , "open", "close", "separators"

      -- menclose
      , "notation"

      -- msub, msup, msubsup
      , "subscriptshift", "superscriptshift"

      -- munder, mover, munderover
      , "accentunder", "accent", "align"

      -- mtable
      , "align", "rowalign", "columnalign", "groupalign", "alignmentscope", "columnwidth"
      , "width", "rowspacing", "columnspacing", "rowlines", "columnlines", "frame"
      , "framespacing", "equalrows", "equalcolumns", "displaystyle", "side"
      , "minlabelspacing"

      -- mtr
      , "rowalign", "columnalign", "groupalign"

      -- mtd
      , "rowspan", "columnspan", "rowalign", "columnalign", "groupalign"

      -- malignmark
      , "edge"

      -- maligngroup
      , "groupalign"

      -- mstack, mlongdiv
      , "align", "stackalign", "charalign", "charspacing", "longdivstyle"

      -- msgroup
      , "position", "shift"

      -- msrow
      , "position"

      -- mscarries
      , "position", "location", "crossout", "scriptsizemultiplier"

      -- mscarry
      , "location", "crossout"

      -- msline
      , "position", "length", "leftoverhang", "rightoverhang", "mslinethickness"

      -- maction
      , "actiontype", "selection"

      -- semantics
      , "definitionURL", "encoding"

      -- annotation, annotation-xml
      , "definitionURL", "encoding", "cd", "name", "src"
      ]
  }

sanitize :: String -> String
sanitize name | name `Set.member` keywords = name ++ "_"
              | otherwise = escapeHyphens name
  where
    keywords = Set.fromList ["case","class","data","default","deriving","do","else"
                            ,"forall","foreign","if","import","in","infix","infixl"
                            ,"infixr","instance","let","mdo","module","newtype","of"
                            ,"pattern","proc","rec","then","type","where","_"
                            ]
    escapeHyphens "" = ""
    escapeHyphens ('-':x:xs) = toUpper x : escapeHyphens xs
    escapeHyphens (x:xs) = x : escapeHyphens xs

main :: IO ()
main = writeMMLVariant mathml3