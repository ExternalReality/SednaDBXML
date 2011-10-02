module Database.SednaDB.Internal.SednaBindings
    ( SednaConnection
    , sednaNext
    , sednaGetData
    , sednaConnect
    , sednaCloseConnection
    , sednaBegin
    , sednaRollBack
    , sednaCommit
    , sednaExecute
    , sednaExecuteLong
    , sednaLoadData
    , sednaTransactionStatus
    , sednaShowTime
    , sednaGetLastErrorMsg
    , sednaGetConnectionAttr
    , sednaSetConnectionAttr
    , sednaGetLastErrorCode
    , sednaResetAllConnectionAttr
    , sednaEndLoadData
    , sednaSetDebugHandler
    ) where

--------------------------------------------------------------------------------
import Data.ByteString as BS
import Data.Maybe 
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Prelude hiding             (replicate,concat)
import qualified Data.Map as DM   (fromList, lookup)

import Database.SednaDB.Internal.SednaCBindings
import Database.SednaDB.Internal.SednaConnectionAttributes 
import Database.SednaDB.Internal.SednaResponseCodes

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
    return (fromCConstant status, conn)
      
--------------------------------------------------------------------------------
withSednaConnection :: (SednaConnection ->  IO CInt) 
                       -> SednaConnection 
                       -> IO SednaResponseCode
withSednaConnection sednaAction conn = 
  do 
    response <- sednaAction $ conn
    return  $ fromCConstant response

--------------------------------------------------------------------------------   
sednaCloseConnection :: SednaConnection -> IO SednaResponseCode 
sednaCloseConnection = withSednaConnection c'SEclose 

--------------------------------------------------------------------------------
sednaBegin :: SednaConnection -> IO SednaResponseCode
sednaBegin = withSednaConnection c'SEbegin

--------------------------------------------------------------------------------
sednaRollBack :: SednaConnection -> IO SednaResponseCode
sednaRollBack = withSednaConnection c'SErollback

--------------------------------------------------------------------------------
sednaCommit :: SednaConnection -> IO SednaResponseCode
sednaCommit = withSednaConnection c'SEcommit

--------------------------------------------------------------------------------
sednaExecuteAction :: (SednaConnection -> CString -> IO CInt) 
                   -> SednaConnection 
                   -> String 
                   -> IO SednaResponseCode 
sednaExecuteAction sednaQueryAction conn query = do 
  resultCode <- withCString query $ sednaQueryAction conn
  return $ fromCConstant resultCode
  
--------------------------------------------------------------------------------
sednaExecuteLong :: SednaConnection -> String -> IO SednaResponseCode
sednaExecuteLong = sednaExecuteAction c'SEexecuteLong

--------------------------------------------------------------------------------
sednaExecute :: SednaConnection -> String -> IO SednaResponseCode
sednaExecute = sednaExecuteAction c'SEexecute

--------------------------------------------------------------------------------
-- SednaGetData deals with an underlying c function c'SEgetData which
-- has heterogeneous response types. To remedy this, SednaGetData
-- returns an OperationSucceeded response on success when receiving
-- the number of bytes read from the underlying c function. This is
-- why you see this function returning its own response code instead
-- of simply encoding values from the response of c'SEgetData.
   
sednaGetData :: SednaConnection -> Int -> IO (SednaResponseCode, ByteString)
sednaGetData conn size = useAsCStringLen (BS.replicate size 0) loadData
  where
    loadData bufferLengthPair = do
      let buff  = fst bufferLengthPair
      let size' = fromIntegral (snd bufferLengthPair)

      numOfBytesRead  <- c'SEgetData conn buff size'
      response        <- return $ getResponse numOfBytesRead size'
      bytes           <- packCStringLen (buff, fromIntegral numOfBytesRead)
     
      return $ (response, bytes)
        where
          getResponse num buffSize | num > buffSize         = SednaError
                                   | num < 0                = fromCConstant num                              
                                   | num == 0               = ResultEnd                      
                                   | num > 0                = OperationSucceeded
                                   | otherwise              = SednaError
                                                                                                        
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
          return $ fromCConstant response
                         
--------------------------------------------------------------------------------        
sednaEndLoadData :: SednaConnection -> IO SednaResponseCode
sednaEndLoadData = withSednaConnection c'SEendLoadData

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
             return $ fromCConstant response)
                                      
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
             return (fromCConstant resultCode, SednaConnAttrValue response))
                                       
--------------------------------------------------------------------------------                                                  
sednaResetAllConnectionAttr :: SednaConnection -> IO SednaResponseCode
sednaResetAllConnectionAttr = withSednaConnection c'SEresetAllConnectionAttr

--------------------------------------------------------------------------------
sednaSetDebugHandler :: SednaConnection -> DebugHandler -> IO ()
sednaSetDebugHandler = undefined