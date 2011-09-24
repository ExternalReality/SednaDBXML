module Database.SednaDB.Internal.SednaResponseCodes where

--------------------------------------------------------------------------------

import Foreign.C.Types

--------------------------------------------------------------------------------

#include <libsedna.h>

--------------------------------------------------------------------------------

newtype SednaResponseCode = SednaResponseCode { sednaResponseCode :: CInt }
    deriving (Eq)

instance Show SednaResponseCode where
    show code = 
        case sednaResponseCode code of
           #{const SEDNA_OPERATION_SUCCEEDED}                -> "SEDNA_OPERATION_SUCCEEDED"                  
           #{const SEDNA_SESSION_OPEN}                       -> "SEDNA_SESSION_OPEN"                          
           #{const SEDNA_SESSION_CLOSED}                     -> "SEDNA_SESSION_CLOSED"                       
           #{const SEDNA_AUTHENTICATION_FAILED}              -> "SEDNA_AUTHENTICATION_FAILED"
           #{const SEDNA_OPEN_SESSION_FAILED}                -> "SEDNA_OPEN_SESSION_FAILED"                 
           #{const SEDNA_CLOSE_SESSION_FAILED}               -> "SEDNA_CLOSE_SESSION_FAILED"  
           #{const SEDNA_QUERY_SUCCEEDED}                    -> "SEDNA_QUERY_SUCCEEDED"                     
           #{const SEDNA_QUERY_FAILED}                       -> "SEDNA_QUERY_FAILED"
           #{const SEDNA_UPDATE_SUCCEEDED}                   -> "SEDNA_UPDATE_SUCCEEDED"                    
           #{const SEDNA_UPDATE_FAILED}                      -> "SEDNA_UPDATE_FAILED"  
           #{const SEDNA_BULK_LOAD_SUCCEEDED}                -> "SEDNA_BULK_LOAD_SUCCEEDED"             
           #{const SEDNA_BULK_LOAD_FAILED}                   -> "SEDNA_BULK_LOAD_FAILED" 
           #{const SEDNA_BEGIN_TRANSACTION_SUCCEEDED}        -> "SEDNA_BEGIN_TRANSACTION_SUCCEEDED"        
           #{const SEDNA_BEGIN_TRANSACTION_FAILED}           -> "SEDNA_BEGIN_TRANSACTION_FAILED"           
           #{const SEDNA_ROLLBACK_TRANSACTION_SUCCEEDED}     -> "SEDNA_ROLLBACK_TRANSACTION_SUCCEEDED"      
           #{const SEDNA_ROLLBACK_TRANSACTION_FAILED}        -> "SEDNA_ROLLBACK_TRANSACTION_FAILED"        
           #{const SEDNA_COMMIT_TRANSACTION_SUCCEEDED}       -> "SEDNA_COMMIT_TRANSACTION_SUCCEEDED"      
           #{const SEDNA_NEXT_ITEM_SUCCEEDED}                -> "SEDNA_NEXT_ITEM_SUCCEEDED"                
           #{const SEDNA_NEXT_ITEM_FAILED}                   -> "SEDNA_NEXT_ITEM_FAILED"                  
           #{const SEDNA_NO_ITEM}                            -> "SEDNA_NO_ITEM"                            
           #{const SEDNA_RESULT_END}                         -> "SEDNA_RESULT_END"                         
           #{const SEDNA_DATA_CHUNK_LOADED}                  -> "SEDNA_DATA_CHUNK_LOADED"                 
           #{const SEDNA_ERROR}                              -> "SEDNA_ERROR"                             
           #{const SEDNA_TRANSACTION_ACTIVE}                 -> "SEDNA_TRANSACTION_ACTIVE"                 
           #{const SEDNA_NO_TRANSACTION}                     -> "SEDNA_NO_TRANSACTION"                     
           #{const SEDNA_CONNECTION_OK}                      -> "SEDNA_CONNECTION_OK"                      
           #{const SEDNA_CONNECTION_CLOSED}                  -> "SEDNA_CONNECTION_CLOSED"                  
           #{const SEDNA_CONNECTION_FAILED}                  -> "SEDNA_CONNECTION_FAILED"                             
           #{const SEDNA_SET_ATTRIBUTE_SUCCEEDED}            -> "SEDNA_SET_ATTRIBUTE_SUCCEEDED"            
           #{const SEDNA_GET_ATTRIBUTE_SUCCEEDED}            -> "SEDNA_GET_ATTRIBUTE_SUCCEEDED"          
           #{const SEDNA_RESET_ATTRIBUTES_SUCCEEDED}         -> "SEDNA_RESET_ATTRIBUTES_SUCCEEDED"
           _                                                 -> "UNKNOWN RESPONSE_CODE"    
                    
--------------------------------------------------------------------------------

#{enum SednaResponseCode, SednaResponseCode
 , operationSucceded            = SEDNA_OPERATION_SUCCEEDED                 
 , sessionOpen                  = SEDNA_SESSION_OPEN                          
 , sessionClosed                = SEDNA_SESSION_CLOSED                       
 , authenticationFailed         = SEDNA_AUTHENTICATION_FAILED               
 , openSessionFailed            = SEDNA_OPEN_SESSION_FAILED                 
 , closeSessionFailed           = SEDNA_CLOSE_SESSION_FAILED                
 , querySucceded                = SEDNA_QUERY_SUCCEEDED                     
 , queryFailed                  = SEDNA_QUERY_FAILED
 , updateSucceded               = SEDNA_UPDATE_SUCCEEDED                    
 , updateFailed                 = SEDNA_UPDATE_FAILED                       
 , bulkLoadSucceeded            = SEDNA_BULK_LOAD_SUCCEEDED                 
 , bulkLoadFailed               = SEDNA_BULK_LOAD_FAILED 
 , beginTransactionSucceeded    = SEDNA_BEGIN_TRANSACTION_SUCCEEDED        
 , beginTransactionFailed       = SEDNA_BEGIN_TRANSACTION_FAILED           
 , rollbackTansactionSucceeded  = SEDNA_ROLLBACK_TRANSACTION_SUCCEEDED      
 , rollbackTransactionFailed    = SEDNA_ROLLBACK_TRANSACTION_FAILED        
 , commitTransactionSucceded    = SEDNA_COMMIT_TRANSACTION_SUCCEEDED       
 , commitTransactionFailed      = SEDNA_COMMIT_TRANSACTION_FAILED          
 , nextItemSucceeded            = SEDNA_NEXT_ITEM_SUCCEEDED                
 , nextItemFailed               = SEDNA_NEXT_ITEM_FAILED                   
 , noItem                       = SEDNA_NO_ITEM                            
 , resultEnd                    = SEDNA_RESULT_END                         
 , dataChunkLoaded              = SEDNA_DATA_CHUNK_LOADED                  
 , sednaError                   = SEDNA_ERROR                              
 , transactionActive            = SEDNA_TRANSACTION_ACTIVE                 
 , noTransaction                = SEDNA_NO_TRANSACTION                     
 , connectionOK                 = SEDNA_CONNECTION_OK                      
 , connectionClosed             = SEDNA_CONNECTION_CLOSED                  
 , connectionFailed             = SEDNA_CONNECTION_FAILED                             
 , setAttributeSucceeded        = SEDNA_SET_ATTRIBUTE_SUCCEEDED            
 , getAttributeSucceeded        = SEDNA_GET_ATTRIBUTE_SUCCEEDED          
 , resetAttributeSucceeded      = SEDNA_RESET_ATTRIBUTES_SUCCEEDED         
 }                                    
  