module Database.SednaDB.Internal.BindingWrappers where

--------------------------------------------------------------------------------

import Control.Monad.Trans
import Control.Exception
import Data.ByteString as BS
import Data.ByteString.Char8 as C (pack)
import Data.Maybe 
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Prelude hiding (replicate)
import qualified Data.Map as DM (fromList, lookup)

import Data.Iteratee as I hiding (mapM_, peek)
import Data.Iteratee.IO

import Database.SednaDB.Internal.SednaBindings
import Database.SednaDB.Internal.SednaConnectionAttributes 
import Database.SednaDB.Internal.SednaResponseCodes
import Database.SednaDB.Internal.SednaExceptions

--------------------------------------------------------------------------------

type SednaConnection = Ptr C'SednaConnection
type DebugHandler    = C'debug_handler_t

--------------------------------------------------------------------------------

sednaConnect :: String 
             -> String 
             -> String 
             -> String 
             -> IO (SednaResponseCode, SednaConnection)
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
    return (SednaResponseCode status, conn)
      
--------------------------------------------------------------------------------

withSednaConnection :: (SednaConnection ->  IO CInt) 
                       -> SednaConnection 
                       -> IO SednaResponseCode
withSednaConnection sednaAction conn = 
  do 
    response <- sednaAction $ conn
    return  $ SednaResponseCode response

--------------------------------------------------------------------------------
    
sednaCloseConnection :: SednaConnection -> IO SednaResponseCode 
sednaCloseConnection = withSednaConnection c'SEclose 

--------------------------------------------------------------------------------

sednaBegin :: SednaConnection -> IO SednaResponseCode
sednaBegin = withSednaConnection c'SEbegin

--------------------------------------------------------------------------------

sednaRollback :: SednaConnection -> IO SednaResponseCode
sednaRollback = withSednaConnection c'SErollback

--------------------------------------------------------------------------------

sendaCommit :: SednaConnection -> IO SednaResponseCode
sendaCommit = withSednaConnection c'SEcommit

--------------------------------------------------------------------------------

sednaExecuteAction :: (SednaConnection -> CString -> IO CInt) 
                   -> SednaConnection 
                   -> String 
                   -> IO SednaResponseCode 
sednaExecuteAction sednaQueryAction conn query = do 
  resultCode <- withCString query $ sednaQueryAction conn
  return $ SednaResponseCode resultCode
  
--------------------------------------------------------------------------------
  
sednaExecuteLong :: SednaConnection -> String -> IO SednaResponseCode
sednaExecuteLong = sednaExecuteAction c'SEexecuteLong

--------------------------------------------------------------------------------

sednaExecute :: SednaConnection -> String -> IO SednaResponseCode
sednaExecute = sednaExecuteAction c'SEexecute

--------------------------------------------------------------------------------

sednaGetData :: SednaConnection -> Int -> IO (SednaResponseCode, ByteString)
sednaGetData conn size = useAsCStringLen (BS.replicate size 0) loadData
  where
    loadData buff = do
      let buff' = fst buff 
      let size' = fromIntegral (snd buff)
      resultCode <- fmap SednaResponseCode $ c'SEgetData conn buff' size'      
      response   <- packCStringLen (buff', fromIntegral size')
      return $ (resultCode, response)
  
--------------------------------------------------------------------------------
      
sednaLoadData :: SednaConnection 
              -> ByteString 
              -> String 
              -> String 
              -> IO SednaResponseCode
sednaLoadData conn buff docName colName = do
  useAsCStringLen buff loadData 
      where
        loadData s = do
          let buff' = fst s
          let bytes = fromIntegral $ snd s
          cDocName <- newCString docName
          cColName <- newCString colName
          response <- c'SEloadData conn buff' bytes cDocName cColName  
          mapM_ free [cDocName, cColName]
          return $ SednaResponseCode response
                         
--------------------------------------------------------------------------------
          
sednaEndLoadData :: SednaConnection -> IO SednaResponseCode
sednaEndLoadData = withSednaConnection c'SEendLoadData

--------------------------------------------------------------------------------

loadXMLBytes:: MonadIO m => SednaConnection 
            -> String  
            -> String 
            -> Iteratee ByteString m ()
loadXMLBytes conn doc coll =  liftIO (sednaBegin conn) >> liftI step
  where 
    step s@(I.Chunk xs) 
      | xs == (C.pack "") = liftI step
      | otherwise = do 
        response <- liftIO $ sednaLoadData conn xs doc coll
        if response == dataChunkLoaded 
          then liftIO (print s) >>  liftI step
          else throw SednaFailedException
        
    step stream = do
      response <- liftIO $ sednaEndLoadData conn 
      if response == bulkLoadSucceeded
        then liftIO  (sendaCommit conn) >> idone () stream
        else throw SednaBulkLoadFailedException
             
--------------------------------------------------------------------------------
    
loadXMLFile :: SednaConnection -> String -> String -> String -> IO ()       
loadXMLFile conn file doc coll = do 
  iteratee <- enumFile 8 file $ loadXMLBytes conn doc coll  
  run iteratee
  
--------------------------------------------------------------------------------

sednaNext :: SednaConnection -> IO SednaResponseCode
sednaNext = withSednaConnection c'SEnext

--------------------------------------------------------------------------------

sednaGetLastErrorCode :: SednaConnection -> IO SednaResponseCode
sednaGetLastErrorCode = withSednaConnection c'SEgetLastErrorCode

--------------------------------------------------------------------------------

sednaGetLastErrorMsg :: SednaConnection -> IO SednaResponseCode
sednaGetLastErrorMsg = withSednaConnection c'SEgetLastErrorMsg

--------------------------------------------------------------------------------

sednaTransactionStatus :: SednaConnection -> IO SednaResponseCode
sednaTransactionStatus = withSednaConnection c'SEtransactionStatus

--------------------------------------------------------------------------------

sednaShowTime :: SednaConnection -> IO SednaResponseCode
sednaShowTime = withSednaConnection c'SEshowTime

--------------------------------------------------------------------------------

sednaConnectionAttributeMap :: SednaConnAttrValue -> Maybe SednaConnectionAttr
sednaConnectionAttributeMap attr = DM.lookup attr attrValToAttrMap 
  where 
    attrValToAttrMap = 
      DM.fromList [ (autoCommitOff            , attrAutoCommit)                
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
                         
--------------------------------------------------------------------------------
                                                
sednaSetConnectionAttr :: SednaConnection 
                          -> SednaConnAttrValue 
                          -> IO SednaResponseCode
sednaSetConnectionAttr conn attrVal = 
  alloca (\ptrAttrVal -> do
             let connAttr = fromIntegral        $ 
                            sednaConnectionAttr $ 
                            fromJust (sednaConnectionAttributeMap attrVal)
             let attr     = sednaConnAttrValue attrVal
             let size     = fromIntegral $ sizeOf attr
             poke ptrAttrVal attr                  
             response <- c'SEsetConnectionAttr 
                         conn 
                         connAttr 
                         (castPtr ptrAttrVal) 
                         size
             return $ SednaResponseCode response)
                                      
--------------------------------------------------------------------------------

sednaGetConnectionAttr :: SednaConnection 
                          -> SednaConnectionAttr 
                          -> IO (SednaResponseCode, SednaConnAttrValue)                          
sednaGetConnectionAttr conn connAttr = 
  alloca (\sizePtr -> do
             let attr = fromIntegral $ sednaConnectionAttr connAttr
             resultPtr  <- malloc :: IO (Ptr CInt)
             resultCode <- c'SEgetConnectionAttr conn 
                                                 attr 
                                                 (castPtr resultPtr) 
                                                 sizePtr
             response   <- peek (castPtr resultPtr)
             return (SednaResponseCode resultCode, SednaConnAttrValue response))
                                       
--------------------------------------------------------------------------------
                                                    
sednaResetAllConnectionAttr :: SednaConnection -> IO SednaResponseCode
sednaResetAllConnectionAttr = withSednaConnection c'SEresetAllConnectionAttr

--------------------------------------------------------------------------------

sednaSetDebugHandler :: SednaConnection -> DebugHandler -> IO ()
sednaSetDebugHandler = undefined