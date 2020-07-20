module Main where

import Test.Framework.Runners.Console (defaultMain)

import qualified UnitTests (tests)
import qualified Properties (tests)
import qualified CTest (tests)

main = defaultMain $ [Properties.tests, UnitTests.tests, CTest.tests]

