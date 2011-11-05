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
loadXMLFile :: SednaConnection -> FilePath -> Document -> Collection -> IO ()
loadXMLFile conn path doc coll = withTransaction conn $ 
                                 (\conn' -> sednaLoadFile conn' path doc coll)


-------------------------------------------------------------------------------
loadString conn str  doc coll =
    withTransaction conn $ (\conn' -> sednaLoadData conn' (pack str) doc coll)


-------------------------------------------------------------------------------
version :: SednaConnection -> IO QueryResult
version conn = withTransaction conn $ (\conn' -> do
                                         sednaExecute conn' "doc ('$version')"
                                         sednaGetResultString conn')
