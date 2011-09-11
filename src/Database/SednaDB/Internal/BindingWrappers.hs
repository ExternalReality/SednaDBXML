module Database.SednaDB.BindingWrappers where

import Data.ByteString
import Data.ByteString.Unsafe

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc

import Database.SednaDB.Internal.Sedna

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
      
withSednaConnection :: (SednaConnection ->  IO CInt) -> SednaConnection -> IO Int
withSednaConnection sednaAction conn = 
  do 
    resultCode <- sednaAction $ conn
    return $ fromIntegral resultCode
    
sednaCloseConnection :: SednaConnection -> IO Int 
sednaCloseConnection = withSednaConnection c'SEclose 

sednaBegin :: SednaConnection -> IO Int
sednaBegin = withSednaConnection c'SEbegin

sednaRollback :: SednaConnection -> IO Int
sednaRollback = withSednaConnection c'SErollback

sendaCommit :: SednaConnection -> IO Int
sendaCommit = withSednaConnection c'SEcommit

sednaExecuteAction :: (SednaConnection -> CString -> IO CInt) -> SednaConnection -> String -> IO Int 
sednaExecuteAction sednaQueryAction conn query = do 
  resultCode <- withCString query $ sednaQueryAction conn
  return $ fromIntegral resultCode
  
sednaExecuteLong :: SednaConnection -> String -> IO Int
sednaExecuteLong = sednaExecuteAction c'SEexecuteLong

sednaExecute :: SednaConnection -> String -> IO Int
sednaExecute = sednaExecuteAction c'SEexecute

sednaGetData :: SednaConnection -> Int -> IO Int
sednaGetData = undefined

sednaLoadData :: SednaConnection -> ByteString -> Int -> String -> String -> IO Int
sednaLoadData conn buff byteCount docName colName = undefined

sednaEndLoadData :: SednaConnection -> IO Int
sednaEndLoadData = withSednaConnection c'SEendLoadData 

sednaNext :: SednaConnection -> IO Int
sednaNext = withSednaConnection c'SEnext

sednaGetLastErrorCode :: SednaConnection -> IO Int
sednaGetLastErrorCode = withSednaConnection c'SEgetLastErrorCode

sednaGetLastErrorMsg :: SednaConnection -> IO Int
sednaGetLastErrorMsg = withSednaConnection c'SEgetLastErrorMsg

sednaTransactionStatus :: SednaConnection -> IO Int
sednaTransactionStatus = withSednaConnection c'SEtransactionStatus

sednaShowTime :: SednaConnection -> IO Int
sednaShowTime = withSednaConnection c'SEshowTime

sednaSetConnectionAttr :: SednaConnection -> Int -> IO Int
sednaSetConnectionAttr = undefined

sednaGetConnectionAttr :: SednaConnection -> Int -> IO Int
sednaGetConnectionAttr = undefined

sednaResetAllConnectionAttr :: SednaConnection -> IO Int
sednaResetAllConnectionAttr = withSednaConnection c'SEresetAllConnectionAttr

sednaSetDebugHandler :: SednaConnection -> DebugHandler -> IO ()
sednaSetDebugHandler = undefined