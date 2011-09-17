import Database.SednaDB.Internal.BindingWrappers
import Database.SednaDB.Internal.SednaResponseCodes
import Database.SednaDB.Internal.SednaConnectionAttributes 

import Foreign
import GHC.IO.Exception
import IO
import System.Cmd
import System.Process

import Test.HUnit

bringUpDB :: IO ExitCode
bringUpDB = do 
              pid <- runCommand "se_gov"
              waitForProcess pid2
              pid2 <- runCommand "se_sm testdb"
              waitForProcess pid2

bringDownDB :: IO ExitCode
bringDownDB = do 
               pid <- runCommand "se_smsd testdb"
               waitForProcess pid 
               pid2 <- runCommand "se_stop"
               waitForProcess pid2

setup :: IO (SednaResponseCode, SednaConnection)
setup = do 
         bringUpDB
         sednaConnect "localhost" "testdb" "SYSTEM" "MANAGER"

tearDown = \(_, conn) -> 
  do
    free conn
    bringDownDB

sednaDBConnectionTest  :: ((SednaResponseCode, SednaConnection) -> IO c) -> IO c
sednaDBConnectionTest = bracket setup tearDown

connectionTest connFun msg succVal = TestCase $ sednaDBConnectionTest $
    (\(resultCode, conn) -> do
       result <- connFun $ conn          
       assertEqual msg succVal result)

testOpenConnection :: Test
testOpenConnection = TestCase $
                     do           
                       (status, conn) <- setup
                       result         <- assertEqual "Session opened successfully" 
                                                     sessionOpen 
                                                     status 
                       tearDown(status, conn)
                       return result

testCloseConnection :: Test
testCloseConnection =  connectionTest sednaCloseConnection  
                                      "session Closed successfully" 
                                      sessionClosed                                   
                                      
testBeginTransaction :: Test
testBeginTransaction = 
  TestCase $ sednaDBConnectionTest  
               (\(_,conn) ->
                 do
                   result <- sednaBegin conn          
                   assertEqual "Begin transaction successful," 
                               beginTransactionSucceeded
                               result)

testSetConnectionAttr :: Test
testSetConnectionAttr =
  TestCase $ sednaDBConnectionTest
               (\(_,conn) ->
                 do
                   result <- sednaSetConnectionAttr conn autoCommitOff
                   assertEqual "Set attribute succeeded"
                                setAttributeSucceeded
                                result)                   
               
testGetConnectionAttr :: Test
testGetConnectionAttr =
  TestCase $ sednaDBConnectionTest
   (\(_,conn) ->
     do
       (resultCode, result) <- sednaGetConnectionAttr conn attrAutoCommit
       assertEqual "Get attribute succeeded"
                   getAttributeSucceeded
                   resultCode                   
       assertEqual "Get attribute should get correct value"
                   autoCommitOff
                   result)
          
                                                  
connectionTests :: Test
connectionTests  = TestList [testOpenConnection, testCloseConnection, testGetConnectionAttr, testSetConnectionAttr]

transactionTests :: Test
transactionTests = TestList [testBeginTransaction]