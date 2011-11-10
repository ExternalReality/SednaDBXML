{-# LANGUAGE ScopedTypeVariables #-}

module Integration.SednaBindingTests (integrationTests) where

--------------------------------------------------------------------------------
import           Prelude hiding          (catch)
import           Control.Exception
import           Data.ByteString.Char8   (pack, unpack)
import           Foreign                 (free)
import           System.Process          (readProcess)
import           Text.Printf             (printf)

import           Test.HUnit hiding (Test)
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit

import           Database.SednaTypes
import           Database.SednaBindings
import           Database.Internal.SednaConnectionAttributes
import           Database.SednaExceptions
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)


--------------------------------------------------------------------------------
type TestMsg = String


--------------------------------------------------------------------------------
testDBName :: String
testDBName = "SednaDBXMLTestDB"

testCollName = "'testCollection'"


--------------------------------------------------------------------------------
bringUpDB :: IO String
bringUpDB = do readProcess "se_cdb"[testDBName] "/dev/null"
               readProcess "se_sm" [testDBName] "/dev/null"
               readProcess "se_term" [ "-query"
                                     ,"CREATE COLLECTION " ++ testCollName
                                     , testDBName
                                     ]
                                     "/dev/null"


--------------------------------------------------------------------------------
bringDownDB :: IO String
bringDownDB = do readProcess "se_smsd" [testDBName] "/dev/null"
                 readProcess "se_ddb"  [testDBName] "/dev/null"


--------------------------------------------------------------------------------
setup :: IO SednaConnection
setup = do
         let url       = "localhost"
         let dbname    = testDBName
         let login     = "SYSTEM"
         let password  = "MANAGER"

         bringUpDB
         onException (sednaConnect url dbname login password)
                     bringDownDB

tearDown :: SednaConnection -> IO String
tearDown conn =
  do
    sednaCloseConnection conn
    bringDownDB


--------------------------------------------------------------------------------
formatMsg :: String -> String
formatMsg = printf "%-60s"


--------------------------------------------------------------------------------
testCaseFMsg :: String -> Assertion -> Test
testCaseFMsg = testCase . formatMsg


--------------------------------------------------------------------------------
sednaDBTest  :: (SednaConnection -> IO c) -> IO c
sednaDBTest = bracket setup tearDown


----------------------------------------------------------------------------------
connectionTest :: (SednaConnection -> IO ()) -> String -> Test
connectionTest connFun msg =
    testCaseFMsg msg $
    catch (sednaDBTest connFun)
          (\(e :: SednaException) -> assertFailure $ show e)


--------------------------------------------------------------------------------
testOpenConnection :: Test
testOpenConnection = testCaseFMsg "Testing connection initialization" openTest


--------------------------------------------------------------------------------
openTest :: IO ()
openTest = do
  bracketOnError bringUpDB
                 (\_ -> bringDownDB >> assertFailure "Open Connection Failed")
                 (\_ -> sednaConnect "localhost" testDBName "SYSTEM" "MANAGER" >>= free)
  bringDownDB
  return ()


--------------------------------------------------------------------------------
--testCloseConnection :: Test
--testCloseConnection =  connectionTest sednaCloseConnection
--                                      "Test connection termination"


 --------------------------------------------------------------------------------
testBeginTransaction :: Test
testBeginTransaction = connectionTest sednaBegin
                                      "Testing transaction initialization"


---------------------------------------------------------------------------------
testSetConnectionAttr :: Test
testSetConnectionAttr =
    connectionTest (`sednaSetConnectionAttr` autoCommitOff)
                   "Testing modification of connection attributes"


---------------------------------------------------------------------------------
testGetConnectionAttr :: Test
testGetConnectionAttr =
 connectionTest (\conn -> do
                   result <- sednaGetConnectionAttr conn attrAutoCommit
                   assertEqual "Testing attribute value response."
                                autoCommitOff
                                result)
                "Testing inspection of connection attributes"


---------------------------------------------------------------------------------
testLoadData :: Test
testLoadData =
    connectionTest (\conn ->  do
                      sednaBegin conn
                      sednaLoadData conn
                                    (pack "<Message>Hello World!!!</Message>")
                                    "testDoc"
                                    "testCollection"
                      sednaEndLoadData conn
                      sednaCommit conn)
                   "Testing proper loading of chunk data"


--------------------------------------------------------------------------------
testLoadFile :: Test
testLoadFile = 
    let testFile = "fixtures/baseballleague.xml" in
    connectionTest (\conn -> sednaLoadFile conn
                                           testFile                                 
                                           "testdoc3"
                                           "testCollection")
    "Testing loading of XML file"


--------------------------------------------------------------------------------
testExecuteQuery :: Test
testExecuteQuery = 
    connectionTest (\conn -> do
                      sednaBegin conn                                 
                      queryExecutionStatus <- sednaExecute conn "doc('$documents')"
                      sednaCommit conn)
                  "Testing Proper Execution of valid query"


--------------------------------------------------------------------------------
testLoadRetrieveData :: Test
testLoadRetrieveData =
    let xmlData = pack "<note>And the world alright with me!</note>" in
    connectionTest (\conn -> do
                      sednaBegin conn
                      sednaLoadData conn xmlData "testdoc" "testCollection"
                      sednaEndLoadData conn
                      sednaExecute conn "doc('testdoc','testCollection')/note"

                      queryResult <- sednaGetResult conn
                      assertEqual "Testing proper retrieval of query results"
                                  (decodeUtf8 xmlData)
                                  (T.concat.T.lines $ queryResult)
                      sednaCommit conn)
                   "Testing loading and retrieval of data."


-----------------------------------------------------------------------------------
connectionTests :: Test
connectionTests = testGroup "Connection Tests" [ testOpenConnection
                                               ]


--------------------------------------------------------------------------------
controlTests :: Test
controlTests = testGroup "Control Tests" [ testGetConnectionAttr
                                         , testSetConnectionAttr
                                         ]


-- --------------------------------------------------------------------------------
transactionTests :: Test
transactionTests = testGroup "Transaction Tests" [ testBeginTransaction
                                                 , testLoadData
                                                 , testLoadFile
                                                 , testExecuteQuery
                                                 , testLoadRetrieveData
                                                 ]


--------------------------------------------------------------------------------
integrationTests :: Test
integrationTests = testGroup "Sedna C API Integration Tests" [ connectionTests
                                                             , controlTests
                                                             , transactionTests
                                                             ]