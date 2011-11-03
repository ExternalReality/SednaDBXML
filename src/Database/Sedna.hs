module Database.Sedna where

import Database.SednaBindings
import Database.SednaTypes
import Control.Exception
import Data.ByteString.Char8


-------------------------------------------------------------------------------    
withTransaction :: SednaConnection -> (SednaConnection -> IO a) -> IO a
withTransaction conn func =
    do 
      sednaBegin conn
      r <- onException (func conn) doRollback
      sednaCommit conn
      return r
          where doRollback = 
                    Control.Exception.catch (sednaRollBack conn) doRollbackHandler
                
                -- Discard any exception from (sednaRollBack conn) so original
                -- exception can be re-raised
                doRollbackHandler :: SomeException -> IO ()
                doRollbackHandler _ = return ()


-------------------------------------------------------------------------------    
-- sednaExample :: IO ()
-- sednaExample = do
--   let msg = pack "<msg>Hello Worl!</msg>"
--   conn <- sednaConnect "localhost" "apidb" "SYSTEM" "MANAGER"  
--   withTransaction conn 
--                   (\conn' -> do
--                      (sednaLoadData conn' msg "hello" "coll")
--                      (sednaEndLoadData conn'))






