{-# LANGUAGE CPP #-}

module Main (main) where

import Data.Complex (Complex ((:+)))
import Data.Complex.Polar
#ifdef __GLASGOW_HASKELL__
import GHC.Stack (HasCallStack)
#endif
import System.Exit
  ( ExitCode (ExitFailure, ExitSuccess),
    exitWith,
  )
import Test.HUnit
  ( Assertion,
    Test (TestCase, TestList),
    assertBool,
    assertEqual,
    errors,
    failures,
    runTestTT,
  )

tests :: Test
tests =
  TestList
    [ TestCase $ rAndThetaAre 1 (0.1 - pi) $ mkPolar (- 1) 0.1,
      TestCase $ rAndThetaAre 1 (pi / 2) $ mkPolar 1 (-3 * pi / 2),
      TestCase $ rAndThetaAre 1 (0.5 - pi) $ mkPolar 1 (pi + 0.5),
#if __GLASGOW_HASKELL__ >= 736
      TestCase $ rAndThetaAre 1 (0.1 - pi) $ (- 1) :< 0.1,
      TestCase $ rAndThetaAre 1 (pi / 2) $ 1 :< (- 3 * pi / 2),
      TestCase $ rAndThetaAre 1 (0.5 - pi) $ 1 :< (pi + 0.5),
#endif
      TestCase $ rAndThetaAre 1 0.3 $ cis 0.3,
      TestCase $ rAndThetaAre 1 (0.3 - pi) $ cis (pi + 0.3),
      TestCase $ rAndThetaAre 0 0 $ mkPolar 0 pi,
      TestCase $ rAndThetaAre 1 pi $ signum (mkPolar 1 pi),
      TestCase $ rAndThetaAre 0 0 $ signum (mkPolar 0 pi),
      TestCase $ rAndThetaAre 1 (-3 * pi / 4) $ negate (mkPolar 1 (pi / 4)),
      TestCase $ rAndThetaAre 1 pi $ fromRational (- 1),
      TestCase $ rAndThetaAre 1 0 $ exp $ mkPolar (10 * pi) (pi / 2),
      TestCase $ rAndThetaAre (sqrt 2) (pi / 4) $ fromComplex $ 1 :+ 1,
      TestCase $ isApproximately "realPart" 1 $ realPart $ mkPolar 1 0,
      TestCase $ isApproximately "imagPart" 1 $ imagPart $ mkPolar 1 (pi / 2),
      TestCase $ rAndThetaAre 1 (- pi / 4) $ conjugate $ mkPolar 1 (pi / 4),
      TestCase $
        rAndThetaAre (sqrt 2) (pi / 4) $
          mkPolar 1 0 + mkPolar 1 (pi / 2),
      TestCase $
        rAndThetaAre (sqrt 2) (- pi / 4) $
          mkPolar 1 0 - mkPolar 1 (pi / 2),
      TestCase $ rAndThetaAre 1 0.7 $ mkPolar 1 0.3 * mkPolar 1 0.4,
      TestCase $ rAndThetaAre (sqrt 2) 0 $ abs $ mkPolar (sqrt 2) 0.3,
      TestCase $ rAndThetaAre 1 0 1, -- Test Num fromInteger
      TestCase $ rAndThetaAre 1 0 $ mkPolar 1 0 * mkPolar 2 0.2 / mkPolar 2 0.2,
      TestCase $ rAndThetaAre pi 0 pi, -- Test Floating pi
      TestCase $ rAndThetaAre 54.5984 2.00000 $ exp z,
      TestCase $ rAndThetaAre 1.56798 0.30018 $ log z,
      TestCase $ rAndThetaAre 2.11474 0.23182 $ sqrt z,
      TestCase $ rAndThetaAre 3.70497 (- 2.4472) $ sin z,
      TestCase $ rAndThetaAre 3.68529 2.30135 $ cos z,
      TestCase $ rAndThetaAre 1.00534 1.53455 $ tan z,
      TestCase $ rAndThetaAre 27.3050 1.99974 $ sinh z,
      TestCase $ rAndThetaAre 27.2930 2.00025 $ cosh z,
      TestCase $ rAndThetaAre 1.00043 (- 0.0005) $ tanh z,
      TestCase $ rAndThetaAre 2.44362 1.10527 $ asin z,
      TestCase $ rAndThetaAre 2.23441 (- 1.3570) $ acos z,
      TestCase $ rAndThetaAre 1.37491 0.07018 $ atan z,
      TestCase $ rAndThetaAre 2.24493 0.20357 $ asinh z,
      TestCase $ rAndThetaAre 2.23441 0.21370 $ acosh z,
      TestCase $ rAndThetaAre 1.48069 1.43491 $ atanh z,
      TestCase $ assertBool "Eq instance works" $ mkPolar 1 pi == mkPolar 1 pi,
      TestCase $
        assertEqual "Show output good" "mkPolar 1.0 1.0" $
          show $ mkPolar 1 1,
      TestCase $ readShowInverse z,
      TestCase $ readShowInverse [z, z],
      TestCase $ readShowInverse $ Just z
    ]
  where
    z = mkPolar 4.47214 0.46364

approximatelyEqualFloat :: Float -> Float -> Bool
approximatelyEqualFloat x1 x2 = abs (x1 - x2) < 0.001

isApproximately ::
#ifdef __GLASGOW_HASKELL__
  HasCallStack =>
#endif
  String -> Float -> Float -> Assertion
isApproximately name x1 x2 =
  assertBool (name ++ " = " ++ show x1 ++ " is approximately " ++ show x2) $
    approximatelyEqualFloat x1 x2

rAndThetaAre ::
#ifdef __GLASGOW_HASKELL__
  HasCallStack =>
#endif
  Float -> Float -> Polar Float -> Assertion
rAndThetaAre r theta z = do
  isApproximately "magnitude" r (magnitude z)
  isApproximately "phase" theta (phase z)
  let (r', theta') = polar z
  isApproximately "magnitude from polar" r r'
  isApproximately "phase from polar" theta theta'
#if __GLASGOW_HASKELL >= 786
  let (r'' :< theta') = polar z
  isApproximately "magnitude from :<" r r''
  isApproximately "phase from :<" theta theta''
#endif

readShowInverse ::
  (Show a, Read a,
#ifdef __GLASGOW_HASKELL__
  HasCallStack, 
#endif
  Eq a) =>
  a -> Assertion
readShowInverse a = assertEqual ("read . show = id on " ++ show a)
                                (read $ show a)
                                a
 
main :: IO ()
main = do
  counts <- runTestTT tests
  if errors counts + failures counts == 0
    then exitWith ExitSuccess
    else exitWith (ExitFailure 1)
