module DataBase.SednaDB.BindingWrappers where

import Data.ByteString
import Data.ByteString.Unsafe

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc

import Database.SednaDB.Internal.Sedna
import Database.SednaDB.Internal.SednaResponseCodes

type SednaConnection = Ptr C'SednaConnection
type DebugHandler    = C'debug_handler_t

sednaConnect :: String -> String -> String -> String -> IO SednaConnection
sednaConnect url dbname login password  =  
    do
      conn      <- malloc
      cUrl      <- newCString url 
      cDbname   <- newCString dbname 
      cLogin    <- newCString login
      cPassword <- newCString password 
      status    <- c'SEconnect conn 
                               cUrl 
                               cDbname 
                               cLogin 
                               cPassword
                               
      mapM_ free [cUrl,cDbname,cLogin,cPassword]
      return conn
      
withSednaConnection :: (SednaConnection ->  IO CInt) -> SednaConnection -> IO SednaResponseCode
withSednaConnection sednaAction conn = 
  do 
    result <- sednaAction $ conn
    return  $ SednaResponseCode result
    
sednaCloseConnection :: SednaConnection -> IO SednaResponseCode 
sednaCloseConnection = withSednaConnection c'SEclose 

sednaBegin :: SednaConnection -> IO SednaResponseCode
sednaBegin = withSednaConnection c'SEbegin

sednaRollback :: SednaConnection -> IO SednaResponseCode
sednaRollback = withSednaConnection c'SErollback

sendaCommit :: SednaConnection -> IO SednaResponseCode
sendaCommit = withSednaConnection c'SEcommit

sednaExecuteAction :: (SednaConnection -> CString -> IO CInt) -> SednaConnection -> String -> IO SednaResponseCode 
sednaExecuteAction sednaQueryAction conn query = do 
  resultCode <- withCString query $ sednaQueryAction conn
  return $ SednaResponseCode resultCode
  
sednaExecuteLong :: SednaConnection -> String -> IO SednaResponseCode
sednaExecuteLong = sednaExecuteAction c'SEexecuteLong

sednaExecute :: SednaConnection -> String -> IO SednaResponseCode
sednaExecute = sednaExecuteAction c'SEexecute

sednaGetData :: SednaConnection -> Int -> IO SednaResponseCode
sednaGetData = undefined

sednaLoadData :: SednaConnection -> ByteString -> String -> String -> IO SednaResponseCode
sednaLoadData conn buff docName colName = 
  do
    useAsCStringLen buff loadData 
      where
        loadData s = do
          let buff' = fst s
          let bytes = fromIntegral $ snd s
          cDocName <- newCString docName
          cColName <- newCString colName
          result   <- c'SEloadData conn buff' bytes cDocName cColName  
          mapM_ free [cDocName, cColName]
          return $ SednaResponseCode result
                         
sednaEndLoadData :: SednaConnection -> IO SednaResponseCode
sednaEndLoadData = withSednaConnection c'SEendLoadData 

sednaNext :: SednaConnection -> IO SednaResponseCode
sednaNext = withSednaConnection c'SEnext

sednaGetLastErrorCode :: SednaConnection -> IO SednaResponseCode
sednaGetLastErrorCode = withSednaConnection c'SEgetLastErrorCode

sednaGetLastErrorMsg :: SednaConnection -> IO SednaResponseCode
sednaGetLastErrorMsg = withSednaConnection c'SEgetLastErrorMsg

sednaTransactionStatus :: SednaConnection -> IO SednaResponseCode
sednaTransactionStatus = withSednaConnection c'SEtransactionStatus

sednaShowTime :: SednaConnection -> IO SednaResponseCode
sednaShowTime = withSednaConnection c'SEshowTime

sednaSetConnectionAttr :: SednaConnection -> Int -> IO SednaResponseCode
sednaSetConnectionAttr = undefined

sednaGetConnectionAttr :: SednaConnection -> Int -> IO SednaResponseCode
sednaGetConnectionAttr = undefined

sednaResetAllConnectionAttr :: SednaConnection -> IO SednaResponseCode
sednaResetAllConnectionAttr = withSednaConnection c'SEresetAllConnectionAttr

sednaSetDebugHandler :: SednaConnection -> DebugHandler -> IO ()
sednaSetDebugHandler = undefined