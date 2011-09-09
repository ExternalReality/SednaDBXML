module Database.SednaDB.Connection where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc

import Control.Monad
import Control.Monad.Reader
import Database.SednaDB.Internal.Sedna


type SednaConnection = Ptr C'SednaConnection

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




  
                            
     
                     
                      