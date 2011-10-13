module Main where

-------------------------------------------------------------------------------
import Test.Framework (defaultMain)
import Integration.SednaBindingTests

-------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests
    where tests = [ connectionTests
                  , controlTests
                  , transactionTests
                  ]  