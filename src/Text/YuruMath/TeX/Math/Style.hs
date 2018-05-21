module Text.YuruMath.TeX.Math.Style where

data MathStyle = DisplayStyle             -- \displaystyle, 0
               | CrampedDisplayStyle      -- \crampeddisplaystyle, 1
               | TextStyle                -- \textstyle, 2
               | CrampedTextStyle         -- \crampedtextstyle, 3
               | ScriptStyle              -- \scriptstyle, 4
               | CrampedScriptStyle       -- \crampedscriptstyle, 5
               | ScriptScriptStyle        -- \scriptscriptstyle, 6
               | CrampedScriptScriptStyle -- \crampedscriptscriptstyle, 7
               deriving (Eq,Show,Enum,Bounded)

isCramped :: MathStyle -> Bool
isCramped CrampedDisplayStyle      = True
isCramped CrampedTextStyle         = True
isCramped CrampedScriptStyle       = True
isCramped CrampedScriptScriptStyle = True
isCramped _ = False

-- sqrt, overline
makeCramped :: MathStyle -> MathStyle
makeCramped DisplayStyle      = CrampedDisplayStyle
makeCramped TextStyle         = CrampedTextStyle
makeCramped ScriptStyle       = CrampedScriptStyle
makeCramped ScriptScriptStyle = CrampedScriptScriptStyle
makeCramped crampedstyle = crampedstyle

makeCrampedIf :: Bool -> MathStyle -> MathStyle
makeCrampedIf True  = makeCramped
makeCrampedIf False = id

-- superscript
superscriptStyle :: MathStyle -> MathStyle
superscriptStyle        DisplayStyle =        ScriptStyle
superscriptStyle CrampedDisplayStyle = CrampedScriptStyle
superscriptStyle        TextStyle    =        ScriptStyle
superscriptStyle CrampedTextStyle    = CrampedScriptStyle
superscriptStyle        ScriptStyle  =        ScriptScriptStyle
superscriptStyle CrampedScriptStyle  = CrampedScriptScriptStyle
superscriptStyle scriptscriptstyle = scriptscriptstyle

-- subscript
subscriptStyle :: MathStyle -> MathStyle
subscriptStyle = makeCramped . superscriptStyle

-- numerator
smallerStyle :: MathStyle -> MathStyle
smallerStyle        DisplayStyle =        TextStyle
smallerStyle CrampedDisplayStyle = CrampedTextStyle
smallerStyle        TextStyle    =        ScriptStyle
smallerStyle CrampedTextStyle    = CrampedScriptStyle
smallerStyle        ScriptStyle  =        ScriptScriptStyle
smallerStyle CrampedScriptStyle  = CrampedScriptScriptStyle
smallerStyle scriptscriptstyle = scriptscriptstyle

-- denominator
denominatorStyle :: MathStyle -> MathStyle
denominatorStyle = makeCramped . smallerStyle
