module Database.SednaDB.SednaTypes
    ( SednaConnection
    , Document
    , Collection
    , collection
    , connection
    , document
    , runTransaction
    ) where 
            
--------------------------------------------------------------------------------
import Control.Monad.Error
import Control.Monad.Reader
import Foreign (Ptr)

import Database.SednaDB.Internal.SednaCBindings (C'SednaConnection)

--------------------------------------------------------------------------------
type Collection      = String
type Document        = String

--------------------------------------------------------------------------------
type SednaConnection = Ptr C'SednaConnection

--------------------------------------------------------------------------------
type QueryResult = String

--------------------------------------------------------------------------------
type ErrorMsg = String

--------------------------------------------------------------------------------
data TransactionEnv = TransactionEnv { connection :: SednaConnection
                                     , collection :: Collection
                                     , document   :: Document
                                     } deriving (Show)

--------------------------------------------------------------------------------
type Transaction a = ReaderT TransactionEnv (ErrorT ErrorMsg IO) a

--------------------------------------------------------------------------------
runTransaction :: SednaConnection
               -> Document 
               -> Collection 
               -> Transaction QueryResult 
               -> IO (Either ErrorMsg QueryResult)
runTransaction conn doc coll trans = runErrorT (runReaderT trans env)
    where env = TransactionEnv conn coll doc




