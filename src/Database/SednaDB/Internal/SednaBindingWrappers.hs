module Database.SednaDB.Internal.SednaBindingWrappers 
    ( sednaGetResultString
    , sednaQueryFromString
    , sednaQueryFromFile
    , sednaLoadFile
    ) where

--------------------------------------------------------------------------------
import Control.Exception
import Control.Monad.Trans
import Data.ByteString as BS
import Data.ByteString.Char8 as C (pack,unpack,concat,append, empty)
import Data.Maybe
import Foreign
import Prelude hiding             (replicate,concat)

import Data.Iteratee as I hiding  (mapM_, peek)
import Data.Iteratee.IO

import Database.SednaDB.SednaBindings
import Database.SednaDB.SednaExceptions
import Database.SednaDB.SednaTypes
import Database.SednaDB.Internal.SednaResponseCodes

--------------------------------------------------------------------------------
sednaGetQueryResult :: Transaction (IO QueryResult)
sednaGetQueryResult = do TransactionEnv conn _ _ <- getTransactionEnv
                         return $ sednaGetResultString conn 

--------------------------------------------------------------------------------
loadXMLBytes:: MonadIO m => SednaConnection
            -> String
            -> String
            -> Iteratee ByteString m ()
loadXMLBytes conn doc coll =  liftIO (sednaBegin conn) >> liftI step
  where
    step s@(I.Chunk xs)
      | xs == (C.pack "") = liftI step
      | otherwise = do
        response <- liftIO $ sednaLoadData conn xs doc coll
        if response == DataChunkLoaded
          then liftIO (print s) >>  liftI step
          else throw SednaFailedException

    step stream = do
      response <- liftIO $ sednaEndLoadData conn
      case response of
         BulkLoadSucceeded -> liftIO  (sednaCommit conn) >> idone () stream
         BulkLoadFailed    -> throw SednaBulkLoadFailedException
         _                 -> throw SednaFailedException

--------------------------------------------------------------------------------
sednaLoadFile :: String -> Transaction ()
sednaLoadFile file = do
   (TransactionEnv conn coll doc) <- getTransactionEnv
   iteratee                       <- enumFile 8 file $ loadXMLBytes 
                                                       conn 
                                                       doc 
                                                       coll
   run iteratee

--------------------------------------------------------------------------------
sednaQuery :: String -> Transaction ()
sednaQuery query = do 
  TransactionEnv conn _ _ <- getTransactionEnv 
  response                <- liftIO $ sednaExecute conn query 
  case response of
    QueryFailed    -> throw SednaQueryFailedException
    QuerySucceeded -> return ()
    _              -> throw SednaFailedException 
    

--------------------------------------------------------------------------------
sednaQueryFromFile :: FilePath -> Transaction ()
sednaQueryFromFile pathToFile = sednaQuery pathToFile

sednaQueryFromString :: Query -> Transaction ()
sednaQueryFromString queryString = sednaQuery queryString 

