{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}

--------------------------------------------------------------------------------
module Database.SednaExceptions
    ( SednaAuthenticationFailedException      (SednaAuthenticationFailedException)
    , SednaBulkLoadFailedException            (SednaBulkLoadFailedException)
    , SednaOpenSessionFailedException         (SednaOpenSessionFailedException)
    , SednaCloseSessionFailedException        (SednaCloseSessionFailedException)
    , SednaCommitTransactionFailedException   (SednaCommitTransactionFailedException)
    , SednaFailedException                    (SednaFailedException)
    , SednaNextItemFailedException            (SednaNextItemFailedException)
    , SednaRollBackTransactionFailedException (SednaRollBackTransactionFailedException)
    , SednaTransactionException               (SednaTransactionException)
    , SednaBeginTransactionFailedException    (SednaBeginTransactionFailedException)
    , SednaException                          (SednaException)
    , SednaQueryException                     (SednaQueryException)
    , SednaQueryFailedException               (SednaQueryFailedException)
    , SednaUpdateFailedException              (SednaUpdateFailedException)
    ) where

--------------------------------------------------------------------------------
import Control.Exception
import Data.Typeable

--------------------------------------------------------------------------------
data SednaException =
    forall e . Exception e => SednaException e
           deriving Typeable

instance Show SednaException where
    show (SednaException e) = show e

instance Exception SednaException where

sednaExceptionToException :: Exception e => e -> SomeException
sednaExceptionToException = toException . SednaException

sednaExceptionFromException :: Exception e => SomeException -> Maybe e
sednaExceptionFromException x = do
  SednaException a <- fromException x
  cast a

--------------------------------------------------------------------------------
data SednaFailedException =
    SednaFailedException
    deriving (Typeable, Show)

instance Exception SednaFailedException  where
    toException   = sednaExceptionToException
    fromException = sednaExceptionFromException

--------------------------------------------------------------------------------
data SednaBulkLoadFailedException =
    SednaBulkLoadFailedException
    deriving (Typeable, Show)

instance Exception SednaBulkLoadFailedException  where
    toException   = sednaExceptionToException
    fromException = sednaExceptionFromException

--------------------------------------------------------------------------------
data SednaNextItemFailedException =
    SednaNextItemFailedException
    deriving (Typeable, Show)

instance Exception SednaNextItemFailedException  where
    toException   = sednaExceptionToException
    fromException = sednaExceptionFromException

--------------------------------------------------------------------------------
data SednaConnectionException =
    forall e . Exception e => SednaConnectionException e
           deriving Typeable

instance Show SednaConnectionException where
    show (SednaConnectionException e) = show e

instance Exception SednaConnectionException where
    toException   = sednaExceptionToException
    fromException = sednaExceptionFromException

sednaConnectionExceptionToException :: Exception e => e -> SomeException
sednaConnectionExceptionToException = toException . SednaConnectionException

sednaConnectionExceptionFromException :: Exception e => SomeException -> Maybe e
sednaConnectionExceptionFromException x = do
  SednaConnectionException a <- fromException x
  cast a

--------------------------------------------------------------------------------
data SednaOpenSessionFailedException =
  SednaOpenSessionFailedException
  deriving (Typeable, Show)

instance Exception SednaOpenSessionFailedException  where
    toException   = sednaConnectionExceptionToException
    fromException = sednaConnectionExceptionFromException

--------------------------------------------------------------------------------
data SednaCloseSessionFailedException =
  SednaCloseSessionFailedException
  deriving (Typeable, Show)

instance Exception SednaCloseSessionFailedException where
    toException   = sednaConnectionExceptionToException
    fromException = sednaConnectionExceptionFromException

--------------------------------------------------------------------------------
data SednaAuthenticationFailedException =
    SednaAuthenticationFailedException
    deriving (Typeable, Show)

instance Exception SednaAuthenticationFailedException  where
    toException   = sednaConnectionExceptionToException
    fromException = sednaConnectionExceptionFromException

--------------------------------------------------------------------------------
data SednaconnectionFailedException =
    SednaconnectionFailedException
    deriving (Typeable, Show)

instance Exception SednaconnectionFailedException  where
    toException   = sednaConnectionExceptionToException
    fromException = sednaConnectionExceptionFromException

--------------------------------------------------------------------------------
data SednaTransactionException =
    forall e . Exception e => SednaTransactionException e
           deriving Typeable

instance Show SednaTransactionException where
    show (SednaTransactionException e) = show e

instance Exception SednaTransactionException where
    toException   = sednaExceptionToException
    fromException = sednaExceptionFromException

sednaTransactionExceptionToException :: Exception e => e -> SomeException
sednaTransactionExceptionToException = toException . SednaTransactionException

sednaTransactionExceptionFromException :: Exception e => SomeException -> Maybe e
sednaTransactionExceptionFromException x = do
  SednaTransactionException a <- fromException x
  cast a

--------------------------------------------------------------------------------
data SednaBeginTransactionFailedException =
    SednaBeginTransactionFailedException
    deriving (Typeable, Show)

instance Exception SednaBeginTransactionFailedException  where
    toException   = sednaTransactionExceptionToException
    fromException = sednaTransactionExceptionFromException

--------------------------------------------------------------------------------

data SednaCommitTransactionFailedException =
    SednaCommitTransactionFailedException
    deriving (Typeable, Show)

instance Exception SednaCommitTransactionFailedException  where
    toException   = sednaTransactionExceptionToException
    fromException = sednaTransactionExceptionFromException

--------------------------------------------------------------------------------
data SednaRollBackTransactionFailedException =
    SednaRollBackTransactionFailedException
    deriving (Typeable, Show)

instance Exception SednaRollBackTransactionFailedException  where
    toException   = sednaTransactionExceptionToException
    fromException = sednaTransactionExceptionFromException

--------------------------------------------------------------------------------
data SednaQueryException =
    forall e . Exception e => SednaQueryException e
           deriving Typeable

instance Show SednaQueryException where
    show (SednaQueryException e) = show e

instance Exception SednaQueryException where
    toException   = sednaExceptionToException
    fromException = sednaExceptionFromException

sednaQueryExceptionToException :: Exception e => e -> SomeException
sednaQueryExceptionToException = toException . SednaQueryException

sednaQueryExceptionFromException :: Exception e => SomeException -> Maybe e
sednaQueryExceptionFromException x = do
  SednaQueryException a <- fromException x
  cast a

--------------------------------------------------------------------------------
data SednaQueryFailedException =
    SednaQueryFailedException
    deriving (Typeable, Show)

instance Exception SednaQueryFailedException  where
    toException   = sednaQueryExceptionToException
    fromException = sednaQueryExceptionFromException

--------------------------------------------------------------------------------
data SednaUpdateFailedException =
    SednaUpdateFailedException
    deriving (Typeable, Show)

instance Exception SednaUpdateFailedException  where
    toException   = sednaQueryExceptionToException
    fromException = sednaQueryExceptionFromException