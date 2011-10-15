module Database.SednaDB.SednaTypes
    ( SednaConnection
    , Transaction
    , QueryResult
    , Query
    , getTransactionEnv
    , TransactionEnv (TransactionEnv)
    , Document
    , Collection
    , runTransaction
    , URL
    , Password
    , DBName
    , UserName
    ) where

--------------------------------------------------------------------------------
import Control.Monad.Error
import Control.Monad.Reader
import Foreign (Ptr)

import Database.SednaDB.Internal.SednaCBindings (C'SednaConnection)

--------------------------------------------------------------------------------
type URL = String 

--------------------------------------------------------------------------------
type DBName = String

--------------------------------------------------------------------------------
type UserName = String
type Password = String

--------------------------------------------------------------------------------
type Collection = String
type Document   = String

--------------------------------------------------------------------------------
type SednaConnection = Ptr C'SednaConnection

--------------------------------------------------------------------------------
type Query       = String
type QueryResult = String

--------------------------------------------------------------------------------
type ErrorMsg = String

--------------------------------------------------------------------------------
data TransactionEnv = TransactionEnv SednaConnection Collection Document
                      deriving (Show)

--------------------------------------------------------------------------------
type Transaction a = ReaderT TransactionEnv (ErrorT ErrorMsg IO) a

--------------------------------------------------------------------------------
getTransactionEnv :: Transaction TransactionEnv
getTransactionEnv = ask

--------------------------------------------------------------------------------
runTransaction :: SednaConnection
               -> Document
               -> Collection
               -> Transaction QueryResult
               -> IO (Either ErrorMsg QueryResult)
runTransaction conn doc coll trans = runErrorT (runReaderT trans env)
    where env = TransactionEnv conn coll doc