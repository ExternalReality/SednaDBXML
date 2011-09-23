module Database.SednaDB.Sedna where

import Control.Monad.IO.Class

import Data.ByteString.Char8
import Data.Iteratee as I
import Data.Iteratee.IO

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Data.Iteratee.Char as C

import Database.SednaDB.Internal.SednaBindings
import Database.SednaDB.Internal.SednaConnectionAttributes 
import Database.SednaDB.Internal.SednaResponseCodes

import Data.Maybe (fromJust)

import Database.SednaDB.Internal.BindingWrappers
import Database.SednaDB.Internal.SednaResponseCodes

loadXMLBytes  :: MonadIO m => 
                 SednaConnection -> String -> String -> Iteratee ByteString m ()
loadXMLBytes conn doc coll =  liftIO (sednaBegin conn) >> liftI step
  where 
    step s@(I.Chunk xs) 
      | xs == (pack "") = liftI step
      | otherwise = do 
        response <- liftIO $ sednaLoadData conn xs doc coll
        if response == dataChunkLoaded 
          then liftIO (print s) >>  liftI step
          else error $ "something bad happened" ++ (show s)
        
    step stream = do
      response <- liftIO $ sednaEndLoadData conn 
      if response == bulkLoadSucceeded
        then liftIO  (sendaCommit conn) >> idone () stream
        else error "the bulk load did not succeed"
    
loadXMLFile conn file doc coll = do 
  iteratee <- enumFile 8 file $ loadXMLBytes conn doc coll  
  run iteratee
    
loadData = do
  conn <- sednaConnect "localhost" "testdb" "SYSTEM" "MANAGER"
  if (fst conn) == sessionOpen 
       then  print "sessionOpen"
       else  error "connectionFailed"
  
  loadXMLFile (snd conn) 
              "/home/objectivity/dev/SednaDB-Haskell/test/fixtures/baseballleague.xml"
              "test2"
              "testcollection"


