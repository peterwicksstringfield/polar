{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 786
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
#endif

module Data.Complex.Polar (
#if __GLASGOW_HASKELL__ >= 800
    Polar((:<)),
#elif __GLASGOW_HASKELL >= 786
    Polar,
    pattern (:<),
#else
    Polar,
#endif
    fromPolar,
    fromComplex,
    realPart,
    imagPart,
    conjugate,
    mkPolar,
    cis,
    polar,
    magnitude,
    phase
) where

import           Data.Complex (Complex(..))
import qualified Data.Complex as C

import Data.Typeable
#ifdef __GLASGOW_HASKELL__
import Data.Data (Data)
#endif

#ifdef __HUGS__
import Hugs.Prelude(Num(fromInt), Fractional(fromDouble))
#endif

import Text.Read (parens)
import Text.ParserCombinators.ReadPrec (prec, step)
import Text.Read.Lex (Lexeme (Ident))

infix 6 :<<

-- -----------------------------------------------------------------------------
-- The Polar type

-- | Complex numbers are an algebraic type.
--
-- For a complex number @z@, @'abs' z@ is a number with the magnitude of @z@,
-- but oriented in the positive real direction, whereas @'signum' z@
-- has the phase of @z@, but unit magnitude.
data (RealFloat a) => Polar a = !a :<< !a -- ^ forms a complex number from its magnitude
                                          --   and its phase in radians.
#if __GLASGOW_HASKELL__
    deriving (Eq, Data, Typeable)
#else
    deriving Eq
#endif

#if __GLASGOW_HASKELL__ >= 786
infix 6 :<

-- | Smart constructor that canonicalizes the magnitude and phase.
pattern r :< theta <-
  ( \(r :<< theta) -> Just (r, theta) ->
      Just (r, theta)
    )
  where
    r :< theta = mkPolar r theta
#endif

instance (RealFloat a, Show a) => Show (Polar a) where
    {-# SPECIALISE instance Show (Polar Float) #-}
    {-# SPECIALISE instance Show (Polar Double) #-}
    showsPrec d (r :<< theta) =
      showParen (d >= 11) (showString "mkPolar "
                           . showsPrec 11 r
                           . showString " "
                           . showsPrec 11 theta)

instance (RealFloat a, Read a) => Read (Polar a) where
    {-# SPECIALISE instance Read (Polar Float) #-}
    {-# SPECIALISE instance Read (Polar Double) #-}
    readsPrec d = readParen (d > 10)
                        (\s -> do ("mkPolar", s2) <- lex s
                                  (r, s3) <- readsPrec 11 s2
                                  (theta, s4) <- readsPrec 11 s3
                                  return (mkPolar r theta, s4))

-- | Wrap phase back in interval @(-'pi', 'pi']@.
wrap :: (RealFloat a) => a -> a
{-# SPECIALISE wrap :: Float  -> Float   #-}
{-# SPECIALISE wrap :: Double -> Double  #-}
wrap phi | phi <= (-pi) = wrap (phi+2*pi)
wrap phi | phi > pi     = wrap (phi-2*pi)
wrap phi                = phi
    
-- | Convert to rectangular form.
fromPolar :: (RealFloat a) => Polar a -> Complex a
{-# INLINE fromPolar #-}
fromPolar p = realPart p :+ imagPart p

-- | Convert to polar form.
fromComplex :: (RealFloat a) => Complex a -> Polar a
{-# INLINE fromComplex #-}
fromComplex = uncurry mkPolar_ . C.polar

-- | Extracts the real part of a complex number.
realPart :: (RealFloat a) => Polar a -> a
realPart (r :<< theta) = r * cos theta

-- | Extracts the imaginary part of a complex number.
imagPart :: (RealFloat a) => Polar a -> a
imagPart (r :<< theta) = r * sin theta

-- | The conjugate of a complex number.
conjugate :: (RealFloat a) => Polar a -> Polar a
{-# SPECIALISE conjugate :: Polar Float  -> Polar Float #-}
{-# SPECIALISE conjugate :: Polar Double -> Polar Double #-}
conjugate (r :<< theta) = mkPolar r (negate theta)

-- | Form a complex number from polar components of magnitude and phase.
-- The magnitude and phase are expected to be in canonical form.
mkPolar_ :: RealFloat a => a -> a -> Polar a
{-# SPECIALISE mkPolar_ :: Float  -> Float  -> Polar Float  #-}
{-# SPECIALISE mkPolar_ :: Double -> Double -> Polar Double #-}
mkPolar_ r theta = r :<< theta

-- | Form a complex number from polar components of magnitude and phase.
mkPolar :: RealFloat a => a -> a -> Polar a
{-# SPECIALISE mkPolar :: Float  -> Float  -> Polar Float  #-}
{-# SPECIALISE mkPolar :: Double -> Double -> Polar Double #-}
mkPolar r theta | r == 0 = 0 :<< 0
mkPolar r theta | r < 0 = mkPolar (- r) (theta + pi)
mkPolar r theta = mkPolar_ r (wrap theta)

-- | @'cis' t@ is a complex value with magnitude @1@
-- and phase @t@ (modulo @2*'pi'@).
cis :: (RealFloat a) => a -> Polar a
{-# SPECIALISE cis :: Float  -> Polar Float  #-}
{-# SPECIALISE cis :: Double -> Polar Double #-}
cis theta = mkPolar 1 theta

-- | The function 'polar' takes a complex number and
-- returns a (magnitude, phase) pair in canonical form:
-- the magnitude is nonnegative, and the phase in the range @(-'pi', 'pi']@;
-- if the magnitude is zero, then so is the phase.
polar :: (RealFloat a) => Polar a -> (a,a)
{-# SPECIALISE polar :: Polar Double -> (Double,Double) #-}
{-# SPECIALISE polar :: Polar Float  -> (Float,Float)   #-}
polar (r :<< theta) = (r,theta)

-- | The nonnegative magnitude of a complex number.
magnitude :: (RealFloat a) => Polar a -> a
{-# SPECIALISE magnitude :: Polar Float  -> Float  #-}
{-# SPECIALISE magnitude :: Polar Double -> Double #-}
magnitude (r :<< _) = r

-- | The phase of a complex number, in the range @(-'pi', 'pi']@.
-- If the magnitude is zero, then so is the phase.
phase :: (RealFloat a) => Polar a -> a
{-# SPECIALISE phase :: Polar Float  -> Float  #-}
{-# SPECIALISE phase :: Polar Double -> Double #-}
phase (_ :<< theta) = theta

instance (RealFloat a) => Num (Polar a) where
    {-# SPECIALISE instance Num (Polar Float)  #-}
    {-# SPECIALISE instance Num (Polar Double) #-}
    z + z'           = fromComplex (fromPolar z + fromPolar z')
    z - z'           = fromComplex (fromPolar z - fromPolar z')
    z * z'           = mkPolar  (magnitude z * magnitude z') (phase z + phase z')
    negate z         = mkPolar  (negate (magnitude z)) (phase z)
    abs z            = mkPolar_ (magnitude z) 0
    signum (0 :<< _) = 0
    signum z         = mkPolar_ 1 (phase z)
    fromInteger      = flip mkPolar 0 . fromInteger

instance (RealFloat a) => Fractional (Polar a) where
    {-# SPECIALISE instance Fractional (Polar Float)  #-}
    {-# SPECIALISE instance Fractional (Polar Double) #-}
    z / z'          = mkPolar (magnitude z / magnitude z') (phase z - phase z')
    fromRational r  = mkPolar (fromRational r) 0

instance  (RealFloat a) => Floating (Polar a)  where
    {-# SPECIALISE instance Floating (Polar Float) #-}
    {-# SPECIALISE instance Floating (Polar Double) #-}
    pi             = mkPolar_ pi 0
    exp (r :<< theta) = mkPolar (exp (r * cos theta)) (r * sin theta)
    log (r :<< theta) = fromComplex (log r :+ theta)
    
    -- sqrt (0 :<< _)       =  0
    -- sqrt z@(r :<< theta) =  u :+ (if y < 0 then -v else v)
    --                           where (u,v) = if x < 0 then (v',u') else (u',v')
    --                                 v'    = abs y / (u'*2)
    --                                 u'    = sqrt ((magnitude z + abs x) / 2)
    sqrt = fromComplex.sqrt.fromPolar
    
    -- sin (x:+y)     =  sin x * cosh y :+ cos x * sinh y
    sin = fromComplex.sin.fromPolar
    
    -- cos (x:+y)     =  cos x * cosh y :+ (- sin x * sinh y)
    cos = fromComplex.cos.fromPolar
    
    -- tan (x:+y)     =  (sinx*coshy:+cosx*sinhy)/(cosx*coshy:+(-sinx*sinhy))
    --                   where sinx  = sin x
    --                         cosx  = cos x
    --                         sinhy = sinh y
    --                         coshy = cosh y
    tan = fromComplex.tan.fromPolar
    
    -- sinh (x:+y)    =  cos y * sinh x :+ sin  y * cosh x
    sinh = fromComplex.sinh.fromPolar
    
    -- cosh (x:+y)    =  cos y * cosh x :+ sin y * sinh x
    cosh = fromComplex.cosh.fromPolar
    
    -- tanh (x:+y)    =  (cosy*sinhx:+siny*coshx)/(cosy*coshx:+siny*sinhx)
    --                   where siny  = sin y
    --                         cosy  = cos y
    --                         sinhx = sinh x
    --                         coshx = cosh x
    tanh = fromComplex.tanh.fromPolar
    
    -- asin z@(x:+y)  =  y':+(-x')
    --                   where  (x':+y') = log (((-y):+x) + sqrt (1 - z*z))
    asin = fromComplex.asin.fromPolar
    
    -- acos z         =  y'':+(-x'')
    --                   where (x'':+y'') = log (z + ((-y'):+x'))
    --                         (x':+y')   = sqrt (1 - z*z)
    acos = fromComplex.acos.fromPolar
    
    -- atan z@(x:+y)  =  y':+(-x')
    --                   where (x':+y') = log (((1-y):+x) / sqrt (1+z*z))
    atan = fromComplex.atan.fromPolar
    
    asinh z        =  log (z + sqrt (1+z*z))
    acosh z        =  log (z + (z+1) * sqrt ((z-1)/(z+1)))
    atanh z        =  log ((1+z) / sqrt (1-z*z))
