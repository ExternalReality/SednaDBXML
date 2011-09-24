module Test.Integration.BindingsWrappers where

--------------------------------------------------------------------------------

import Control.Exception       (bracket)
import Data.ByteString.Char8   (pack)
import Foreign                 (free)
import System.Exit             (ExitCode)
import System.Process          (runCommand, waitForProcess, readProcess)

import Test.HUnit

import Database.SednaDB.Internal.BindingWrappers
import Database.SednaDB.Internal.SednaConnectionAttributes 
import Database.SednaDB.Internal.SednaResponseCodes

--------------------------------------------------------------------------------

bringUpDB = do 
              readProcess "se_gov" [] "/dev/null"
              readProcess "se_sm" ["testdb"] "/dev/null"
              
--------------------------------------------------------------------------------

bringDownDB = do 
               readProcess "se_smsd" ["testdb"] "/dev/null"
               readProcess "se_stop" [] "/dev/null"

--------------------------------------------------------------------------------

setup :: IO (SednaResponseCode, SednaConnection)
setup = do 
         bringUpDB
         sednaConnect "localhost" "testdb" "SYSTEM" "MANAGER"

tearDown = \(_, conn) -> 
  do
    free conn
    bringDownDB

--------------------------------------------------------------------------------

sednaDBTest  :: ((SednaResponseCode, SednaConnection) -> IO c) -> IO c
sednaDBTest = bracket setup tearDown

--------------------------------------------------------------------------------

connectionTest connFun msg succVal = TestCase $ sednaDBTest $
    (\(resultCode, conn) -> do
       result <- connFun $ conn          
       assertEqual msg succVal result)

--------------------------------------------------------------------------------

testOpenConnection :: Test
testOpenConnection = TestCase $
                     do           
                       (status, conn) <- setup
                       result         <- assertEqual "Testing connection intialization." 
                                                     sessionOpen 
                                                     status 
                       tearDown(status, conn)
                       return result

--------------------------------------------------------------------------------

testCloseConnection :: Test
testCloseConnection =  connectionTest sednaCloseConnection  
                                      "Testing connection closure." 
                                      sessionClosed

--------------------------------------------------------------------------------                                   
                                      
testBeginTransaction :: Test
testBeginTransaction = 
  TestCase $ sednaDBTest  
               (\(_,conn) ->
                 do
                   result <- sednaBegin conn          
                   assertEqual "Testing transaction initialization." 
                               beginTransactionSucceeded
                               result)

--------------------------------------------------------------------------------

testSetConnectionAttr :: Test
testSetConnectionAttr =
  TestCase $ sednaDBTest
               (\(_,conn) ->
                 do
                   result <- sednaSetConnectionAttr conn autoCommitOff
                   assertEqual "Testing set attriubute value funtionality."
                                setAttributeSucceeded
                                result)                   

--------------------------------------------------------------------------------
               
testGetConnectionAttr :: Test
testGetConnectionAttr =
  TestCase $ sednaDBTest
   (\(_,conn) ->
     do
       (resultCode, result) <- sednaGetConnectionAttr conn attrAutoCommit
       assertEqual "Get attribute succeeded"
                   getAttributeSucceeded
                   resultCode                   
       assertEqual "Testing attribute value response."
                   autoCommitOff
                   result)

--------------------------------------------------------------------------------

testLoadData =
 TestCase $ sednaDBTest
   (\(_,conn) -> do
      sednaBegin conn
      resultCode <- sednaLoadData conn 
                    (pack "<?xml version=\"1.0\" standalone=\"yes\"?>") 
                    "testdoc" 
                    "testcollection"
      sednaEndLoadData conn
      assertEqual "Testing proper loading of chunk data."
                  dataChunkLoaded
                  resultCode)
                   
--------------------------------------------------------------------------------
                                                            
connectionTests :: Test
connectionTests = TestList [testOpenConnection, testCloseConnection, testGetConnectionAttr, testSetConnectionAttr]

transactionTests :: Test
transactionTests = TestList [testBeginTransaction, testLoadData]