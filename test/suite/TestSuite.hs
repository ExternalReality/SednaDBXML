module Main where

-------------------------------------------------------------------------------
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, path)

import Integration.SednaBindingTests

-------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests
    where tests = [ connectionTests
                  , controlTests
                  , transactionTests
                  ]  