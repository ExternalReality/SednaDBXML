import Database.SednaDB.Internal.BindingWrappers
import Database.SednaDB.Internal.SednaResponseCodes
import Foreign
import Test.HUnit
import IO
 
setup    = sednaConnect "localhost" "testdb" "SYSTEM" "MANAGER"
tearDown = \conn -> free conn

sednaDBConnectionTest = bracket setup tearDown

connectionTest connFun msg succVal = TestCase $ sednaDBConnectionTest $
    (\conn -> do
       result <- connFun $ conn          
       assertEqual msg succVal result)

testCloseConnection =  connectionTest sednaCloseConnection  
                                      "session Closed successfully" 
                                      sessionClosed

testBeginTransaction = 
    TestCase $ sednaDBConnectionTest  
                 (\conn ->
                      do
                        result <- sednaBegin conn          
                        assertEqual "Begin transaction successful," 
                                    beginTransactionSucceeded
                                    result)
                                                  
connectionTests  = TestList [testCloseConnection]
transactionTests = TestList [testBeginTransaction]