module Test.Integration.BindingsWrappers where

import Control.Exception       (bracket)
import Data.ByteString.Char8   (pack)
import Foreign                 (free)
import System.Exit             (ExitCode)
import System.Process          (runCommand, waitForProcess)

import Test.HUnit

import Database.SednaDB.Internal.BindingWrappers
import Database.SednaDB.Internal.SednaConnectionAttributes 
import Database.SednaDB.Internal.SednaResponseCodes

--------------------------------------------------------------------------------

bringUpDB :: IO ExitCode
bringUpDB = do 
              pid <- runCommand "se_gov"
              waitForProcess pid
              pid2 <- runCommand "se_sm testdb"
              waitForProcess pid2

--------------------------------------------------------------------------------

bringDownDB :: IO ExitCode
bringDownDB = do 
               pid <- runCommand "se_smsd testdb"
               waitForProcess pid 
               pid2 <- runCommand "se_stop"
               waitForProcess pid2

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
                       result         <- assertEqual "Session opened successfully" 
                                                     sessionOpen 
                                                     status 
                       tearDown(status, conn)
                       return result

--------------------------------------------------------------------------------

testCloseConnection :: Test
testCloseConnection =  connectionTest sednaCloseConnection  
                                      "session Closed successfully" 
                                      sessionClosed

--------------------------------------------------------------------------------                                   
                                      
testBeginTransaction :: Test
testBeginTransaction = 
  TestCase $ sednaDBTest  
               (\(_,conn) ->
                 do
                   result <- sednaBegin conn          
                   assertEqual "Begin transaction successful," 
                               beginTransactionSucceeded
                               result)

--------------------------------------------------------------------------------

testSetConnectionAttr :: Test
testSetConnectionAttr =
  TestCase $ sednaDBTest
               (\(_,conn) ->
                 do
                   result <- sednaSetConnectionAttr conn autoCommitOff
                   assertEqual "Set attribute succeeded"
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
       assertEqual "Get attribute should get correct value"
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
      assertEqual "Should Successfully chunk of XML"
                  dataChunkLoaded
                  resultCode)
                   
--------------------------------------------------------------------------------
                                                            
connectionTests :: Test
connectionTests  = TestList [testOpenConnection, testCloseConnection, testGetConnectionAttr, testSetConnectionAttr]

transactionTests :: Test
transactionTests = TestList [testBeginTransaction, testLoadData]