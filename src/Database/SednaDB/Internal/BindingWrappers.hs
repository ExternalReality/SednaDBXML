module Database.SednaDB.Internal.BindingWrappers where

import Data.ByteString
import Data.Maybe 

import qualified Data.Map as DM (fromList, lookup)


import Database.SednaDB.Internal.Sedna
import Database.SednaDB.Internal.SednaConnectionAttributes 
import Database.SednaDB.Internal.SednaResponseCodes

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import Prelude hiding (replicate)

type SednaConnection = Ptr C'SednaConnection
type DebugHandler    = C'debug_handler_t

data SednaResponse = SednaResponse { responseCode :: SednaResponseCode
                                   , result       :: ByteString 
                                   }  

sednaConnect :: String -> String -> String -> String -> IO (SednaResponseCode, SednaConnection)
sednaConnect url dbname login password  =  
    do
      conn      <- malloc --they say malloc is very expensive and I'm not rich
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
      return (SednaResponseCode status, conn)
      
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

sednaGetData :: SednaConnection -> Int -> IO SednaResponse
sednaGetData conn size = useAsCStringLen (replicate size 0) loadData
  where
    loadData buff = do
      let buff' = fst buff 
      let size' = fromIntegral (snd buff)
      resultCode <- fmap SednaResponseCode $ c'SEgetData conn buff' size'      
      result     <- packCStringLen (buff', fromIntegral size')
      return $ SednaResponse resultCode result 
  
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

sednaConnectionAttributeMap :: SednaConnAttrValue -> Maybe SednaConnectionAttr
sednaConnectionAttributeMap attr = DM.lookup attr attrValToAttrMap 
    where 
      attrValToAttrMap =  DM.fromList [ (autoCommitOff            , attrAutoCommit)                
                                      , (autoCommitOn             , attrAutoCommit)             
                                      , (readOnlyTransaction      , attrConcurrencyType)                          
                                      , (updateTransaction        , attrConcurrencyType) 
                                      , (debugOn                  , attrDebug)  
                                      , (debugOff                 , attrDebug)  
                                      , (logLess                  , attrLogAmount)
                                      , (logFull                  , attrLogAmount)
                                      , (boundarySpacePreserveOn  , attrBoundarySpacePreserveWhileLoad)
                                      , (boundarySpacePreserveOff , attrBoundarySpacePreserveWhileLoad) 
                                      ]
                                                
sednaSetConnectionAttr :: SednaConnection -> SednaConnAttrValue -> IO SednaResponseCode
sednaSetConnectionAttr conn attrVal = alloca (\ptrAttrVal -> 
                                                  do
                                                    let connAttr = fromIntegral $ sednaConnectionAttr $ fromJust (sednaConnectionAttributeMap attrVal)
                                                    let attr     = sednaConnAttrValue attrVal
                                                    let size     = sizeOf attr
                                                    poke ptrAttrVal attr                  
                                                    result  <- c'SEsetConnectionAttr conn connAttr (castPtr ptrAttrVal) attr
                                                    return $ SednaResponseCode result)

sednaGetConnectionAttr :: SednaConnection -> SednaConnectionAttr -> IO SednaResponseCode
sednaGetConnectionAttr = undefined

sednaResetAllConnectionAttr :: SednaConnection -> IO SednaResponseCode
sednaResetAllConnectionAttr = withSednaConnection c'SEresetAllConnectionAttr

sednaSetDebugHandler :: SednaConnection -> DebugHandler -> IO ()
sednaSetDebugHandler = undefined