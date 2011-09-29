module Database.SednaDB.Internal.SednaResponseCodes where

--------------------------------------------------------------------------------

import Foreign.C.Types

--------------------------------------------------------------------------------

#include <libsedna.h>

--------------------------------------------------------------------------------
data SednaResponseCode = OperationSucceeded
                       | SessionOpen
                       | SessionClosed
                       | AuthenticationFailed
                       | OpenSessionFailed
                       | CloseSessionFailed
                       | QuerySucceded
                       | QueryFailed
                       | UpdateSucceded
                       | UpdateFailed
                       | BulkLoadSucceeded
                       | BulkLoadFailed
                       | BeginTransactionSucceeded
                       | BeginTransactionFailed
                       | RollBackTansactionSucceeded
                       | RollBackTransactionFailed
                       | CommitTransactionSucceded
                       | CommitTransactionFailed
                       | NextItemSucceeded
                       | NextItemFailed
                       | NoItem
                       | ResultEnd
                       | DataChunkLoaded
                       | SednaError
                       | TransactionActive
                       | NoTransaction
                       | ConnectionOK
                       | ConnectionClosed
                       | ConnectionFailed
                       | SetAttributeSucceeded
                       | GetAttributeSucceeded
                       | ResetAttributeSucceeded
                         deriving ( Eq, Show )

--------------------------------------------------------------------------------

fromCConstant :: CInt -> SednaResponseCode
fromCConstant x = case x of
  #{const SEDNA_OPERATION_SUCCEEDED}            -> OperationSucceeded
  #{const SEDNA_SESSION_OPEN}                   -> SessionOpen
  #{const SEDNA_SESSION_CLOSED}                 -> SessionClosed
  #{const SEDNA_AUTHENTICATION_FAILED}          -> AuthenticationFailed
  #{const SEDNA_OPEN_SESSION_FAILED}            -> OpenSessionFailed
  #{const SEDNA_CLOSE_SESSION_FAILED}           -> CloseSessionFailed
  #{const SEDNA_QUERY_SUCCEEDED}                -> QuerySucceded
  #{const SEDNA_QUERY_FAILED}                   -> QueryFailed
  #{const SEDNA_UPDATE_SUCCEEDED}               -> UpdateSucceded
  #{const SEDNA_UPDATE_FAILED}                  -> UpdateFailed
  #{const SEDNA_BULK_LOAD_SUCCEEDED}            -> BulkLoadSucceeded
  #{const SEDNA_BULK_LOAD_FAILED}               -> BulkLoadFailed
  #{const SEDNA_BEGIN_TRANSACTION_SUCCEEDED}    -> BeginTransactionSucceeded
  #{const SEDNA_BEGIN_TRANSACTION_FAILED}       -> BeginTransactionFailed
  #{const SEDNA_ROLLBACK_TRANSACTION_SUCCEEDED} -> RollBackTansactionSucceeded
  #{const SEDNA_ROLLBACK_TRANSACTION_FAILED}    -> RollBackTransactionFailed
  #{const SEDNA_COMMIT_TRANSACTION_SUCCEEDED}   -> CommitTransactionSucceded
  #{const SEDNA_COMMIT_TRANSACTION_FAILED}      -> CommitTransactionFailed
  #{const SEDNA_NEXT_ITEM_SUCCEEDED}            -> NextItemSucceeded
  #{const SEDNA_NEXT_ITEM_FAILED}               -> NextItemFailed
  #{const SEDNA_NO_ITEM}                        -> NoItem
  #{const SEDNA_RESULT_END}                     -> ResultEnd
  #{const SEDNA_DATA_CHUNK_LOADED}              -> DataChunkLoaded
  #{const SEDNA_ERROR}                          -> SednaError
  #{const SEDNA_TRANSACTION_ACTIVE}             -> TransactionActive
  #{const SEDNA_NO_TRANSACTION}                 -> NoTransaction
  #{const SEDNA_CONNECTION_OK}                  -> ConnectionOK
  #{const SEDNA_CONNECTION_CLOSED}              -> ConnectionClosed
  #{const SEDNA_CONNECTION_FAILED}              -> ConnectionFailed
  #{const SEDNA_SET_ATTRIBUTE_SUCCEEDED}        -> SetAttributeSucceeded
  #{const SEDNA_GET_ATTRIBUTE_SUCCEEDED}        -> GetAttributeSucceeded
  #{const SEDNA_RESET_ATTRIBUTES_SUCCEEDED}     -> ResetAttributeSucceeded
  _                                             -> error "Undefined Constant"

--------------------------------------------------------------------------------

toCConstant :: (Integral a) => SednaResponseCode -> a
toCConstant x = case x of
  OperationSucceeded           ->  #{const SEDNA_OPERATION_SUCCEEDED}
  SessionOpen                  ->  #{const SEDNA_SESSION_OPEN}
  SessionClosed                ->  #{const SEDNA_SESSION_CLOSED}
  AuthenticationFailed         ->  #{const SEDNA_AUTHENTICATION_FAILED}
  OpenSessionFailed            ->  #{const SEDNA_OPEN_SESSION_FAILED}
  CloseSessionFailed           ->  #{const SEDNA_CLOSE_SESSION_FAILED}
  QuerySucceded                ->  #{const SEDNA_QUERY_SUCCEEDED}
  QueryFailed                  ->  #{const SEDNA_QUERY_FAILED}
  UpdateSucceded               ->  #{const SEDNA_UPDATE_SUCCEEDED}
  UpdateFailed                 ->  #{const SEDNA_UPDATE_FAILED}
  BulkLoadSucceeded            ->  #{const SEDNA_BULK_LOAD_SUCCEEDED}
  BulkLoadFailed               ->  #{const SEDNA_BULK_LOAD_FAILED}
  BeginTransactionSucceeded    ->  #{const SEDNA_BEGIN_TRANSACTION_SUCCEEDED}
  BeginTransactionFailed       ->  #{const SEDNA_BEGIN_TRANSACTION_FAILED}
  RollBackTansactionSucceeded  ->  #{const SEDNA_ROLLBACK_TRANSACTION_SUCCEEDED}
  RollBackTransactionFailed    ->  #{const SEDNA_ROLLBACK_TRANSACTION_FAILED}
  CommitTransactionSucceded    ->  #{const SEDNA_COMMIT_TRANSACTION_SUCCEEDED}
  CommitTransactionFailed      ->  #{const SEDNA_COMMIT_TRANSACTION_FAILED}
  NextItemSucceeded            ->  #{const SEDNA_NEXT_ITEM_SUCCEEDED}
  NextItemFailed               ->  #{const SEDNA_NEXT_ITEM_FAILED}
  NoItem                       ->  #{const SEDNA_NO_ITEM}
  ResultEnd                    ->  #{const SEDNA_RESULT_END}
  DataChunkLoaded              ->  #{const SEDNA_DATA_CHUNK_LOADED}
  SednaError                   ->  #{const SEDNA_ERROR}
  TransactionActive            ->  #{const SEDNA_TRANSACTION_ACTIVE}
  NoTransaction                ->  #{const SEDNA_NO_TRANSACTION}
  ConnectionOK                 ->  #{const SEDNA_CONNECTION_OK}
  ConnectionClosed             ->  #{const SEDNA_CONNECTION_CLOSED}
  ConnectionFailed             ->  #{const SEDNA_CONNECTION_FAILED}
  SetAttributeSucceeded        ->  #{const SEDNA_SET_ATTRIBUTE_SUCCEEDED}
  GetAttributeSucceeded        ->  #{const SEDNA_GET_ATTRIBUTE_SUCCEEDED}
  ResetAttributeSucceeded      ->  #{const SEDNA_RESET_ATTRIBUTES_SUCCEEDED}