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
              waitForProcess pid
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

sednaLoadDataTest :: Test
sednaLoadDataTest = 