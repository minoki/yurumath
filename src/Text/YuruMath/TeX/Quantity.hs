{-# LANGUAGE DeriveFunctor #-}
module Text.YuruMath.TeX.Quantity where

infixl 6 <+>, <->

class Quantity a where
  (<+>) :: a -> a -> a
  (<->) :: a -> a -> a
  scaleByRational :: Rational -> a -> a
  scaleAsInteger :: (Integer -> Integer) -> a -> a
  negateQ :: a -> a
  zeroQ :: a

instance Quantity Integer where
  (<+>) = (+)
  (<->) = (-)
  scaleByRational s x = truncate $ s * fromInteger x -- ???
  scaleAsInteger f x = f x
  negateQ = negate
  zeroQ = 0

newtype Dimen = DimenWithSp { asScaledPoints :: Integer } deriving (Eq,Ord,Show)
-- TODO: should keep the original unit?
-- (one of {pt, pc, in, bp, cm, mm, dd, cc, em, ex})

instance Quantity Dimen where
  DimenWithSp x <+> DimenWithSp y = DimenWithSp (x + y)
  DimenWithSp x <-> DimenWithSp y = DimenWithSp (x - y)
  scaleByRational s (DimenWithSp x) = DimenWithSp (truncate $ s * fromInteger x)
  scaleAsInteger f (DimenWithSp x) = DimenWithSp (f x)
  negateQ (DimenWithSp x) = DimenWithSp (negate x)
  zeroQ = DimenWithSp 0

sp, pt, pc, inch, bp, cm, mm, dd, cc :: Rational -> Dimen

-- scaled point (65536sp = 1pt), not available in MathML
sp = DimenWithSp . truncate

-- point, also available in MathML
pt x = sp (65536 * x)

-- pica (1pc = 12pt), also available in MathML
pc x = pt (12 * x)

-- inch (1in = 72.27pt), also available in MathML
inch x = pt (72.27 * x)
-- 1in = 72.27 * 65536sp = 4736286.72sp

-- big point (72bp = 1in), not available in MathML
bp x = inch (x / 72)
-- 1bp = 72.27 * 65536sp / 72 = 65781.76sp

-- centimeter (2.54cm = 1in), also available in MathML
cm x = inch (x / 2.54)
-- 1cm = 72.27 * 65536sp / 2.54 = 1864679.8110236218sp

-- millimeter (10mm = 1cm), also available in MathML
mm x = cm (x / 10)

-- didot point (1157dd = 1238pt), not available in MathML
dd x = pt (1238 / 1157 * x)
-- 1dd = 1238 / 1157 * 65536sp = 70124.08643042351sp

-- cicero (1cc = 12dd), not available in MathML
cc x = dd (12 * x)
-- 1cc = 841489.0371650822sp

-- relative dimension: em, ex

-- only available in MathML: px, %

newtype MuDimen = DimenWithScaledMu { asScaledMu :: Integer } deriving (Eq,Ord,Show)

instance Quantity MuDimen where
  DimenWithScaledMu x <+> DimenWithScaledMu y = DimenWithScaledMu (x + y)
  DimenWithScaledMu x <-> DimenWithScaledMu y = DimenWithScaledMu (x - y)
  scaleByRational s (DimenWithScaledMu x) = DimenWithScaledMu (truncate $ s * fromInteger x)
  scaleAsInteger f (DimenWithScaledMu x) = DimenWithScaledMu (f x)
  negateQ (DimenWithScaledMu x) = DimenWithScaledMu (negate x)
  zeroQ = DimenWithScaledMu 0

mu :: Rational -> MuDimen
mu x = DimenWithScaledMu (truncate (65536 * x))

data StretchShrink dimen = FixedSS !dimen
                         | InfiniteSS !Integer !Int
                         deriving (Eq,Show,Functor)

instance Ord dimen => Ord (StretchShrink dimen) where
  compare (FixedSS x) (FixedSS y) = compare x y
  compare (FixedSS _) (InfiniteSS _ _) = LT
  compare (InfiniteSS _ _) (FixedSS _) = GT
  compare (InfiniteSS x l) (InfiniteSS y m)
    | l < m = LT
    | l > m = GT
    | otherwise = compare x y

data Glue dimen = Glue { glueSpace   :: !dimen
                       , glueStretch :: !(StretchShrink dimen)
                       , glueShrink  :: !(StretchShrink dimen)
                       }
                  deriving (Eq,Show,Functor)

instance (Quantity dimen) => Quantity (StretchShrink dimen) where
  FixedSS x <+> FixedSS y = FixedSS (x <+> y)
  FixedSS _ <+> y@(InfiniteSS _ _) = y
  x@(InfiniteSS _ _) <+> FixedSS _ = x
  x@(InfiniteSS v l) <+> y@(InfiniteSS w m)
    | l < m = y
    | l > m = x
    | otherwise = InfiniteSS (v + w) l
  FixedSS x <-> FixedSS y = FixedSS (x <-> y)
  FixedSS _ <-> y@(InfiniteSS _ _) = negateQ y
  x@(InfiniteSS _ _) <-> FixedSS _ = x
  x@(InfiniteSS v l) <-> y@(InfiniteSS w m)
    | l < m = negateQ y
    | l > m = x
    | otherwise = InfiniteSS (v - w) l
  scaleByRational s (FixedSS x) = FixedSS (scaleByRational s x)
  scaleByRational s (InfiniteSS i l) | i' == 0 = zeroQ
                                     | otherwise = InfiniteSS i' l
    where i' = truncate $ s * fromInteger i
  scaleAsInteger f (FixedSS x) = FixedSS (scaleAsInteger f x)
  scaleAsInteger f (InfiniteSS i l) | i' == 0 = zeroQ
                                    | otherwise = InfiniteSS i' l
    where i' = f i
  negateQ (FixedSS x) = FixedSS (negateQ x)
  negateQ (InfiniteSS i l) = InfiniteSS (negate i) l
  zeroQ = FixedSS zeroQ

instance (Quantity dimen) => Quantity (Glue dimen) where
  g <+> g' = Glue { glueSpace   = glueSpace g   <+> glueSpace g'
                  , glueStretch = glueStretch g <+> glueStretch g'
                  , glueShrink  = glueShrink g  <+> glueShrink g'
                  }
  g <-> g' = Glue { glueSpace   = glueSpace g   <-> glueSpace g'
                  , glueStretch = glueStretch g <-> glueStretch g'
                  , glueShrink  = glueShrink g  <-> glueShrink g'
                  }
  scaleByRational s g = Glue { glueSpace   = scaleByRational s (glueSpace g)
                             , glueStretch = scaleByRational s (glueStretch g)
                             , glueShrink  = scaleByRational s (glueShrink g)
                             }
  scaleAsInteger f g = Glue { glueSpace   = scaleAsInteger f (glueSpace g)
                            , glueStretch = scaleAsInteger f (glueStretch g)
                            , glueShrink  = scaleAsInteger f (glueShrink g)
                            }
  negateQ g = Glue { glueSpace   = negateQ (glueSpace g)
                   , glueStretch = negateQ (glueStretch g)
                   , glueShrink  = negateQ (glueShrink g)
                   }
  zeroQ = Glue { glueSpace   = zeroQ
               , glueStretch = zeroQ
               , glueShrink  = zeroQ
               }
