{-# LANGUAGE QuasiQuotes #-}

module Integration.SednaBindingTests (integrationTests) where

--------------------------------------------------------------------------------
import Control.Exception       (bracket)
import Data.ByteString.Char8   (pack, unpack)
import Foreign                 (free)
import System.Process          (readProcess)
import Text.Printf             (printf)

import Test.HUnit hiding (Test)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit

import Database.SednaDB.SednaTypes
import Database.SednaDB.SednaBindings
import Database.SednaDB.Internal.SednaConnectionAttributes
import Database.SednaDB.Internal.SednaResponseCodes

--------------------------------------------------------------------------------
type TestMsg = String

--------------------------------------------------------------------------------
dbName :: [Char]
dbName = "SednaDBXMLTestDB"

--------------------------------------------------------------------------------
bringUpDB :: IO String
bringUpDB = do readProcess "se_cdb"[dbName] "/dev/null"
               readProcess "se_sm" [dbName] "/dev/null"

--------------------------------------------------------------------------------
bringDownDB :: IO String
bringDownDB = do readProcess "se_smsd" [dbName] "/dev/null"
                 readProcess "se_ddb"  [dbName] "/dev/null"

--------------------------------------------------------------------------------
setup :: IO (SednaResponseCode, SednaConnection)
setup = do
         bringUpDB
         sednaConnect "localhost" dbName "SYSTEM" "MANAGER"

tearDown :: (t, SednaConnection) -> IO String
tearDown = \(_, conn) ->
  do
    free conn
    bringDownDB


--------------------------------------------------------------------------------
formatMsg :: String -> String
formatMsg rawMsg = printf "%-60s" rawMsg

--------------------------------------------------------------------------------
testCaseFMsg :: String -> Assertion -> Test
testCaseFMsg = testCase . formatMsg

--------------------------------------------------------------------------------
sednaDBTest  :: ((SednaResponseCode, SednaConnection) -> IO c) -> IO c
sednaDBTest = bracket setup tearDown

--------------------------------------------------------------------------------
-- connectionTest is a helper funtion that encapsulates the simple case of a
-- a sedna function that accepts a connection and returns a value to be checked
-- against a succes value.

connectionTest :: (Show a, Eq a) => (SednaConnection -> IO a) -> String -> a -> Test
connectionTest connFun msg succVal = testCaseFMsg msg $ sednaDBTest $
    (\(_ , conn) -> do
       result <- connFun $ conn
       assertEqual msg succVal result)

--------------------------------------------------------------------------------
testOpenConnection :: Test
testOpenConnection = testCaseFMsg "Test connection initialization" $
                     do
                       (status, conn) <- setup
                       result         <- assertEqual
                                           "Test opening of connection"
                                           SessionOpen
                                           status
                       tearDown(status, conn)
                       return result

--------------------------------------------------------------------------------
testCloseConnection :: Test
testCloseConnection =  connectionTest sednaCloseConnection
                                      "Test connection termination"
                                       SessionClosed

--------------------------------------------------------------------------------
testBeginTransaction :: Test
testBeginTransaction = connectionTest sednaBegin
                                      "Test transaction initialization"
                                      BeginTransactionSucceeded

--------------------------------------------------------------------------------
testSetConnectionAttr :: Test
testSetConnectionAttr =
  testCaseFMsg "Test setting of connection attributes" $ sednaDBTest
               (\(_,conn) ->
                 do
                   result <- sednaSetConnectionAttr conn autoCommitOff
                   assertEqual "Testing set attriubute value funtionality"
                                SetAttributeSucceeded
                                result)

--------------------------------------------------------------------------------
testGetConnectionAttr :: Test
testGetConnectionAttr =
  testCaseFMsg "Test retrieval of connection attributes" $ sednaDBTest
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
 testCaseFMsg "Test loading of XML Data" $ sednaDBTest
   (\(_,conn) -> do
      sednaBegin conn
      resultCode <- sednaLoadData conn
                    (pack "<?xml version=\"1.0\" standalone=\"yes\"?>")
                    "testdoc"
                    "testcollection"
      sednaEndLoadData conn
      assertEqual "Testing proper loading of chunk data"
                  DataChunkLoaded
                  resultCode)

--------------------------------------------------------------------------------
-- testLoadFile = sednaDBTest $
--                (\(_,conn) -> do
--                   loadXMLFile conn
--                              "test/fixtures/baseballleague.xml"
--                              "testdoc3"
--                              "testcollection")

--------------------------------------------------------------------------------
testExecuteQuery :: Test
testExecuteQuery = testCaseFMsg "Test execution of query" $ sednaDBTest $
                 (\(_,conn) -> do
                   sednaBegin conn

                   queryExecutionStatus <- sednaExecute conn "doc('$documents')"
                   assertion <- assertEqual "Testing proper execution of valid query"
                                queryExecutionStatus
                                QuerySucceeded
                   sednaCommit conn
                   return assertion)

--------------------------------------------------------------------------------
testLoadRetrieveData :: Test
testLoadRetrieveData =
    testCaseFMsg "Test loading and retrieval of XML data"$ sednaDBTest $
                 (\(_,conn) -> do
                    let xmlData = pack "<?xml version=\"1.0\" standalone=\"yes\"?><note>Test must have Failed :-( </note>"

                    beginTransactionStatus <- sednaBegin conn
                    assertEqual "Test begin transaction"
                                BeginTransactionSucceeded
                                beginTransactionStatus

                    createCollectionStatus <- sednaExecute conn "CREATE COLLECTION 'testCollection'"
                    assertEqual "Test query and create collection"
                                 UpdateSucceeded
                                 createCollectionStatus

                    loadDataStatus <- sednaLoadData conn xmlData "testdoc" "testCollection"
                    assertEqual "TestLoadData"
                                DataChunkLoaded
                                loadDataStatus

                    endloadStatus <- sednaEndLoadData conn
                    assertEqual "TestLoadData"
                                BulkLoadSucceeded
                                endloadStatus

                    queryExecutionStatus <- sednaExecute conn "doc('testdoc','testCollection')"
                    assertEqual "Test query"
                                QuerySucceeded
                                queryExecutionStatus

                    queryResult <- sednaGetResultString conn
                    assertEqual "Testing proper retrieval of query results"
                                (unpack xmlData)
                                (concat.lines $ queryResult)

                    commitStatus <- sednaCommit conn
                    assertEqual "Testing transaction commit"
                                CommitTransactionSucceeded
                                commitStatus)

--------------------------------------------------------------------------------
connectionTests :: Test
connectionTests = testGroup "Connection Tests" [testOpenConnection, testCloseConnection]

--------------------------------------------------------------------------------
controlTests :: Test
controlTests = testGroup "Control Tests" [testGetConnectionAttr, testSetConnectionAttr]

--------------------------------------------------------------------------------
transactionTests :: Test
transactionTests = testGroup "Transaction Tests" [ testBeginTransaction
                                                 , testLoadData
                                                 , testExecuteQuery
                                                 , testLoadRetrieveData
                                                 ]

--------------------------------------------------------------------------------
integrationTests :: Test
integrationTests = testGroup "Integration Tests" [ connectionTests
                                                 , controlTests
                                                 , transactionTests
                                                 ]
