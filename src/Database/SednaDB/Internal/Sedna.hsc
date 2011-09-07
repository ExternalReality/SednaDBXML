#include <bindings.dsl.h>
#include <libsedna.h>

module Database.SednaDB.Internal.Sedna where
#strict_import

import Database.SednaDB.Internal.SpDefs

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
#field       bulk_load_started , CChar    
#array_field doc_name          , CChar
#array_field col_name          , CChar
#stoptype

#callback debug_handler_t, FunPtr (<se_debug_info_type> -> CString -> IO ())

#starttype struct SednaConnection
#array_field  url                    , CChar
#array_field  db_name                , CChar
#array_field  login                  , CChar
#array_field  password               , CChar
#array_field  session_directory      , CChar
#field        socket                 , CInt

#field       last_error              , CInt
#array_field last_error_msg          , CChar
#array_field query_time              , CChar

#field       socket_keeps_data       , CChar
#field       first_next              , CChar
#field       result_end              , CChar
#field       in_query                , CChar        
#field       cbl                     , <conn_bulk_load>

#field       isInTransaction         , CInt
#field       isConnectionOk          , CInt

#field       autocommit              , CInt

#field       local_data_length       , CInt 
#field       local_data_offset       , CInt 
#array_field local_data_buf          , CChar

#field       msg                     , <msg_struct>
        
#field       debug_handler           , <debug_handler_t>
        
#field       boundary_space_preserve , CChar
#field       query_timeout           , CInt 
#field       max_result_size         , CInt
#stoptype

#ccall SEconnect,  Ptr <SednaConnection> -> CString -> CString -> CString -> CString -> IO CInt
#ccall SEclose,    Ptr <SednaConnection> -> IO CInt
#ccall SEbegin,    Ptr <SednaConnection> -> IO CInt
#ccall SErollback, Ptr <SednaConnection> -> IO CInt
#ccall SEcommit,   Ptr <SednaConnection> -> IO CInt


#ccall SEexecuteLong, Ptr <SednaConnection> -> CString -> IO CInt
#ccall SEexecute,     Ptr <SednaConnection> -> CString -> IO CInt
 
#ccall SEgetData,     Ptr <SednaConnection> -> CString -> CInt -> IO CInt
#ccall SEloadData,    Ptr <SednaConnection> -> CString -> CInt ->  CString -> CString -> IO CInt
#ccall SEendLoadData, Ptr <SednaConnection>  -> IO CInt

#ccall SEnext, Ptr <SednaConnection>  -> IO CInt

#ccall SEgetLastErrorCode, Ptr <SednaConnection>  -> IO CInt
#ccall SEgetLastErrorMsg , Ptr <SednaConnection>  -> IO CInt

#ccall SEconnectionStatus,  Ptr <SednaConnection>  -> IO CInt
#ccall SEtransactionStatus, Ptr <SednaConnection>  -> IO CInt

#ccall SEshowTime , Ptr <SednaConnection>  -> IO CInt

#ccall SEsetConnectionAttr, Ptr <SednaConnection> -> <SEattr> -> Ptr () -> CInt -> IO CInt 
#ccall SEgetConnectionAttr, Ptr <SednaConnection> -> <SEattr> -> Ptr () -> CInt -> IO CInt 

#ccall SEresetAllConnectionAttr, Ptr <SednaConnection>  -> IO CInt

#ccall SEsetDebugHandler, Ptr <SednaConnection>  -> <debug_handler_t> -> Ptr ()