#include <bingings.dsl.h>
#include <libsedna.h>

module Database.SednaDB where
#strict_import

import Database.SpDefs

#num QUERY_EXECUTION_TIME                      
#num BULK_LOAD_PORTION                         

#num SEDNA_OPERATION_SUCCEEDED                

#num SEDNA_SESSION_OPEN                       
#num SEDNA_SESSION_CLOSED                     
#num SEDNA_AUTHENTICATION_FAILED              
#num SEDNA_OPEN_SESSION_FAILED                
#num SEDNA_CLOSE_SESSION_FAILED               

#num SEDNA_QUERY_SUCCEEDED                    
#num SEDNA_QUERY_FAILED                       

#num SEDNA_UPDATE_SUCCEEDED                   
#num SEDNA_UPDATE_FAILED                      

#num SEDNA_BULK_LOAD_SUCCEEDED                
#num SEDNA_BULK_LOAD_FAILED                   

#num SEDNA_BEGIN_TRANSACTION_SUCCEEDED        
#num SEDNA_BEGIN_TRANSACTION_FAILED           

#num SEDNA_ROLLBACK_TRANSACTION_SUCCEEDED     
#num SEDNA_ROLLBACK_TRANSACTION_FAILED        
                                                                        
#num SEDNA_COMMIT_TRANSACTION_SUCCEEDED                              
#num SEDNA_COMMIT_TRANSACTION_FAILED                                 
                                                                        
#num SEDNA_NEXT_ITEM_SUCCEEDED                                       
#num SEDNA_NEXT_ITEM_FAILED                                          
                                                                        
#num SEDNA_NO_ITEM                                                   
#num SEDNA_RESULT_END                                                
                                                                        
#num SEDNA_DATA_CHUNK_LOADED                                         
                                                                        
#num SEDNA_ERROR                                                     

#num SEDNA_TRANSACTION_ACTIVE                                        
#num SEDNA_NO_TRANSACTION                                            
                                                                        
#num SEDNA_CONNECTION_OK                                             
#num SEDNA_CONNECTION_CLOSED                                         
#num SEDNA_CONNECTION_FAILED                        

#num SEDNA_AUTOCOMMIT_OFF                                            
#num SEDNA_AUTOCOMMIT_ON                                             
                                                                        
#num SEDNA_SET_ATTRIBUTE_SUCCEEDED                                   
#num SEDNA_GET_ATTRIBUTE_SUCCEEDED                                   
                                                                        
#num SEDNA_RESET_ATTRIBUTES_SUCCEEDED                                
                                                                        
#num SEDNA_BOUNDARY_SPACE_PRESERVE_OFF                               
#num SEDNA_BOUNDARY_SPACE_PRESERVE_ON

#integral_t enum SEattr
#num SEDNA_ATTR_AUTOCOMMIT
#num SEDNA_ATTR_SESSION_DIRECTORY
#num SEDNA_ATTR_DEBUG
#num SEDNA_ATTR_BOUNDARY_SPACE_PRESERVE_WHILE_LOAD
#num SEDNA_ATTR_CONCURRENCY_TYPE
#num SEDNA_ATTR_QUERY_EXEC_TIMEOUT
#num SEDNA_ATTR_LOG_AMOUNT
#num SEDNA_ATTR_MAX_RESULT_SIZE

#starttype struct conn_bulk_load
#field bulk_load_started , CStringLen; 
#field doc_name,         , CStringLen;
#field col_name          , CStringLen;
#stoptype

#callback debug_handler_t, FunPtr (se_debug_info_type -> CString -> IO ())

#opaque_t SednaConnection

#callback SEconnect,  Ptr SednaConnection -> CString -> CString -> CString -> CString -> IO CInt
#callback SEclose,    Ptr SednaConnection -> IO CInt
#callback SEbegin,    Ptr SednaConnection -> IO CInt
#callback SErollBack, Ptr SednaConnection -> IO CInt
#callback SEcommit,   Ptr SednaConnection -> IO CInt


#callback SEexecuteLong, Ptr SednaConnection -> CString -> IO CInt
#callback SEexecute,     Ptr SednaConnection -> CString -> IO CInt
 
#callback SEgetData,     Ptr SednaConnection -> CString -> CInt -> IO CInt
#callback SEloadData,    Ptr SednaConnection -> CString -> CInt ->  CString -> CString -> IO CInt
#callback SEendLoadData, Ptr SednaConnection  -> IO CInt

#callback SEnext -> Ptr SednaConnection  -> IO CInt

#callback SEgetLastErrorCode, Ptr SednaConnection  -> IO CInt
#callback SEgetLastErrorMsg , Ptr SednaConnection  -> IO CInt

#callback SEconnectionStatus,  Ptr SednaConnection  -> IO CInt
#callback SEtransactionStatus, Ptr SednaConnection  -> IO CInt

#callback SEshowTime , Ptr SednaConnection  -> IO CInt

#callback SEsetConnectionAttr, Ptr SednaConnection -> SEattr -> Ptr attrValue -> Ptr attValueLength -> IO CInt 
#callback SEgetConnectionAttr, Ptr SednaConnection -> SEattr -> Ptr attrValue -> Ptr attValueLength -> IO CInt 

#callback SEresetAllConnectionAttr , Ptr SednaConnection  -> IO CInt

#callbackSEsetDebugHandler , Ptr SednaConnection  -> debug_handler_t -> Ptr ()