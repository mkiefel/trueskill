{-# LANGUAGE RankNTypes #-}
module Main where

import Linear.V2
import Control.Lens

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import TrueSkill.Autodiff

type Operation = (Num a, Floating a) => a -> a -> a
type Gradient = Double -> Double -> Double
type GradientCheck = Double -> Double -> Bool

testExact :: Operation -> Gradient -> Gradient
          -> GradientCheck
testExact operation gX gY x y =
  g^._x == gX x y && g^._y == gY x y
  where
    g = getGradient v
    v = operation x' y'
    x' = makeAD x 0
    y' = makeAD y 1

prop_add :: GradientCheck
prop_add = testExact (+) (\_ _ -> 1) (\_ _ -> 1)
prop_mul :: GradientCheck
prop_mul = testExact (*) (\_ y -> y) (\x _ -> x)
prop_sub :: GradientCheck
prop_sub = testExact (-) (\_ _ -> 1) (\_ _ -> -1)

isFinite :: Double -> Bool
isFinite d = (not $ isNaN d) && (not $ isInfinite d)

testNumerical :: Double -> Double -> Double -> Operation
              -> GradientCheck
testNumerical eps deltaAbs deltaRel operation x y =
  (not $ isFinite $ getValue v) ||
  (not $ isFinite $ g^._x) ||
  (not $ isFinite $ g^._y) ||
  (
    (abs (g^._x - gX) < deltaAbs ||
     abs ((g^._x - gX) / gX) < deltaRel) &&
    (abs (g^._y - gY) < deltaAbs ||
     abs ((g^._y - gY) / gY) < deltaRel)
  )
  where
    g = getGradient v
    v = operation x' y'
    x' = makeAD x 0
    y' = makeAD y 1

    gX = (getValue vX - getValue v) / eps
    gY = (getValue vY - getValue v) / eps

    vX = operation (x' + lift eps) y'
    vY = operation x' (y' + lift eps)

testStdNumerical :: Operation -> GradientCheck
testStdNumerical = testNumerical 1e-6 1e-2 1e-2

prop_numAdd :: GradientCheck
prop_numAdd = testStdNumerical (+)
prop_numMul :: GradientCheck
prop_numMul = testStdNumerical (*)
prop_numSub :: GradientCheck
prop_numSub = testStdNumerical (-)
prop_numNegate :: GradientCheck
prop_numNegate = testStdNumerical (\x _ -> negate x)
prop_numDiv :: GradientCheck
prop_numDiv = testStdNumerical (/)
prop_numExp :: GradientCheck
prop_numExp = testStdNumerical (\x _ -> exp x)
prop_numSqrt :: GradientCheck
prop_numSqrt = testStdNumerical (\x _ -> sqrt x)
prop_numLog :: GradientCheck
prop_numLog = testStdNumerical (\x _ -> log x)

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [ testGroup "exact gradient"
          [ testProperty "add" prop_add
          , testProperty "mul" prop_mul
          , testProperty "sub" prop_sub
          ]
        , testGroup "numerical gradient"
          [ testProperty "add" prop_numAdd
          , testProperty "mul" prop_numMul
          , testProperty "sub" prop_numSub
          , testProperty "negate" prop_numNegate
          , testProperty "div" prop_numDiv
          , testProperty "exp" prop_numExp
          , testProperty "sqrt" prop_numSqrt
          , testProperty "log" prop_numLog
          ]
        ]
