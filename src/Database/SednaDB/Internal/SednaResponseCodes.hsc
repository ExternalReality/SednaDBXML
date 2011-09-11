module Database.SednaDB.Internal.SednaResponseCodes where

import Foreign.C.Types

#include <libsedna.h>

newtype SednaResponseCode = SednaResponseCode { sednaResponseCode :: CInt }
    deriving (Eq,Show)

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
 , autoCommitOff                = SEDNA_AUTOCOMMIT_OFF                     
 , autoCommitOn                 = SEDNA_AUTOCOMMIT_ON                      
 , setAttributeSucceeded        = SEDNA_SET_ATTRIBUTE_SUCCEEDED            
 , getAttributeSucceeded        = SEDNA_GET_ATTRIBUTE_SUCCEEDED          
 , resetAttributeSucceeded      = SEDNA_RESET_ATTRIBUTES_SUCCEEDED         
 , boundarySpacePreserverOff    = SEDNA_BOUNDARY_SPACE_PRESERVE_OFF        
 , boundarySpacePreserveOn      = SEDNA_BOUNDARY_SPACE_PRESERVE_ON  
 }                                    
  