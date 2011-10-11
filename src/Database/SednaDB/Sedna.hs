module Database.SednaDB.Sedna
    ( sednaGetResultString
    , sednaQueryFromString
    , sednaQueryFromFile
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
sednaQuery :: Query -> Transaction ()
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

