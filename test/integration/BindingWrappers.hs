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
              readProcess "se_cdb"["testdb"] "/dev/null"            
              readProcess "se_sm" ["testdb"] "/dev/null"
                            
--------------------------------------------------------------------------------

bringDownDB = do 
                readProcess "se_smsd" ["testdb"] "/dev/null"
                readProcess "se_ddb"  ["testdb"] "/dev/null"
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
-- connectionTest is a helper funtion that encapsulates the simple case of a 
-- a sedna function that accepts a connection and returns a value to be checked
-- against a succes value. 

connectionTest :: (Show a, Eq a) => (SednaConnection -> IO a) -> String -> a -> Test
connectionTest connFun msg succVal = TestCase $ sednaDBTest $
    (\(_ , conn) -> do
       result <- connFun $ conn          
       assertEqual msg succVal result)

--------------------------------------------------------------------------------

testOpenConnection :: Test
testOpenConnection = TestCase $
                     do           
                       (status, conn) <- setup
                       result         <- assertEqual 
                                           "Testing connection intialization." 
                                           SessionOpen 
                                           status 
                       tearDown(status, conn)
                       return result

--------------------------------------------------------------------------------

testCloseConnection :: Test
testCloseConnection =  connectionTest sednaCloseConnection  
                                      "Testing connection closure." 
                                       SessionClosed

--------------------------------------------------------------------------------                                   
                                      
testBeginTransaction :: Test
testBeginTransaction = connectionTest sednaBegin
                                      "Testing transaction initialization." 
                                      BeginTransactionSucceeded

--------------------------------------------------------------------------------

testSetConnectionAttr :: Test
testSetConnectionAttr =
  TestCase $ sednaDBTest
               (\(_,conn) ->
                 do
                   result <- sednaSetConnectionAttr conn autoCommitOff
                   assertEqual "Testing set attriubute value funtionality."
                                SetAttributeSucceeded
                                result)                   

--------------------------------------------------------------------------------
               
testGetConnectionAttr :: Test
testGetConnectionAttr =
  TestCase $ sednaDBTest
   (\(_,conn) ->
     do
       (resultCode, result) <- sednaGetConnectionAttr conn attrAutoCommit
       assertEqual "Get attribute succeeded"
                   GetAttributeSucceeded
                   resultCode                   
       assertEqual "Testing attribute value response."
                   autoCommitOff
                   result)

--------------------------------------------------------------------------------

testLoadData :: Test
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
                  DataChunkLoaded
                  resultCode)

--------------------------------------------------------------------------------

testLoadFile = sednaDBTest $ 
               (\(_,conn) -> do
                  loadXMLFile conn
                             "test/fixtures/baseballleague.xml"
                             "testdoc3"
                             "testcollection")

--------------------------------------------------------------------------------

testExecuteQuery :: Test
testExecuteQuery = TestCase $ sednaDBTest $ 
                 (\(_,conn) -> do
                   sednaBegin conn                 
                               
                   queryExecutionStatus <- sednaExecute conn "doc('$documents')"
                   assert <- assertEqual "Testing proper execution of valid query."  
                               queryExecutionStatus
                               QuerySucceeded
                   sednaCommit conn
                   return assert)
                      
--------------------------------------------------------------------------------
                                                            
connectionTests :: Test
connectionTests = TestList [testOpenConnection, testCloseConnection]

--------------------------------------------------------------------------------

controlTests :: Test
controlTests = TestList [testGetConnectionAttr, testSetConnectionAttr]

--------------------------------------------------------------------------------

transactionTests :: Test
transactionTests = TestList [ testBeginTransaction 
                            , testLoadData 
                            , testExecuteQuery
                            ]

--------------------------------------------------------------------------------

allTests :: Test
allTests = TestList [connectionTests, controlTests, transactionTests]