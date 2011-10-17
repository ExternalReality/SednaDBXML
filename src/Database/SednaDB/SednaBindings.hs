module Database.SednaDB.SednaBindings
    ( sednaBegin
    , sednaCloseConnection
    , sednaCommit
    , sednaConnect
    , sednaEndLoadData
    , sednaExecute
    , sednaExecuteLong
    , sednaGetConnectionAttr
    , sednaGetData
    , sednaGetLastErrorCode
    , sednaGetLastErrorMsg
    , sednaGetResultString
    , sednaLoadData
    , sednaNext
    , sednaResetAllConnectionAttr
    , sednaRollBack
    , sednaSetConnectionAttr
    , sednaShowTime
    , sednaTransactionStatus
    , sednaLoadFile
    ) where

--------------------------------------------------------------------------------
import Data.ByteString as BS
import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Prelude hiding             (replicate,concat)
import qualified Data.Map as DM   (fromList, lookup)
import Data.Iteratee as I hiding  (mapM_, peek)
import Data.Iteratee.IO
import Control.Monad.Trans
import Data.ByteString.Char8 as C
import Control.Exception

import Database.SednaDB.SednaTypes
import Database.SednaDB.Internal.SednaCBindings
import Database.SednaDB.Internal.SednaConnectionAttributes
import Database.SednaDB.Internal.SednaResponseCodes
import Database.SednaDB.SednaExceptions

--------------------------------------------------------------------------------
sednaConnect :: URL
             -> DBName
             -> UserName
             -> Password
             -> IO SednaConnection
sednaConnect url dbname login password =
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
   
    case fromCConstant status of 
      SessionOpen           -> return $ conn
      OpenSessionFailed     -> free conn >> throw SednaOpenSessionFailedException
      AuthenticationFailed  -> free conn >> throw SednaAuthenticationFailedException
      _                     -> free conn >> throw SednaFailedException 
    
--------------------------------------------------------------------------------
sednaCloseConnection :: SednaConnection -> IO ()
sednaCloseConnection conn = do 
  resultCode <- c'SEclose conn  
  free conn
  case fromCConstant resultCode of 
    SessionClosed      -> return ()
    CloseSessionFailed -> throw SednaCloseSessionFailedException
    _                  -> throw SednaFailedException
    
--------------------------------------------------------------------------------
sednaBegin :: SednaConnection -> IO ()
sednaBegin conn = do
  resultCode <- c'SEbegin conn
  case fromCConstant resultCode of
    BeginTransactionSucceeded -> return ()
    BeginTransactionFailed    -> throw SednaBeginTransactionFailedException
    _                         -> throw SednaFailedException

--------------------------------------------------------------------------------
sednaRollBack :: SednaConnection -> IO ()
sednaRollBack conn = do
  resultCode <- c'SErollback conn
  case fromCConstant resultCode of
    RollBackTansactionSucceeded -> return ()
    RollBackTransactionFailed   -> throw SednaRollBackTransactionFailedException
    _                           -> throw SednaFailedException 
                
--------------------------------------------------------------------------------
sednaCommit :: SednaConnection -> IO ()
sednaCommit conn = do
  resultCode <- c'SEcommit conn
  case fromCConstant resultCode of
    CommitTransactionSucceeded -> return ()
    CommitTransactionFailed    -> throw SednaCommitTransactionFailedException
    _                          -> throw SednaFailedException

--------------------------------------------------------------------------------
sednaExecuteAction :: (SednaConnection -> CString -> IO CInt)
                   -> SednaConnection
                   -> Query
                   -> IO ()
sednaExecuteAction sednaQueryAction conn query = do
  resultCode <- withCString query $ sednaQueryAction conn
  case fromCConstant resultCode of
    QuerySucceeded -> return ()
    QueryFailed    -> throw SednaQueryFailedException
    _              -> throw SednaFailedException

--------------------------------------------------------------------------------
sednaExecuteLong :: SednaConnection -> Query -> IO ()
sednaExecuteLong = sednaExecuteAction c'SEexecuteLong

--------------------------------------------------------------------------------
sednaExecute :: SednaConnection -> Query -> IO ()
sednaExecute = sednaExecuteAction c'SEexecute

--------------------------------------------------------------------------------
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
          getResponse num buffSize | num > buffSize = SednaError
                                   | num < 0        = fromCConstant num
                                   | num == 0       = ResultEnd
                                   | num > 0        = OperationSucceeded
                                   | otherwise      = SednaError

--------------------------------------------------------------------------------
sednaLoadData :: SednaConnection
              -> ByteString
              -> Document
              -> Collection
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
sednaEndLoadData conn = do
    resultCode <- c'SEendLoadData conn
    return $ fromCConstant resultCode
            
--------------------------------------------------------------------------------
sednaNext :: SednaConnection -> IO SednaResponseCode
sednaNext conn = do
  resultCode <- c'SEnext conn
  return $ fromCConstant resultCode
           
--------------------------------------------------------------------------------
sednaGetLastErrorCode :: SednaConnection -> IO SednaResponseCode
sednaGetLastErrorCode conn = do
  resultCode <- c'SEgetLastErrorCode conn
  return $ fromCConstant resultCode

--------------------------------------------------------------------------------
sednaGetLastErrorMsg :: SednaConnection -> IO String 
sednaGetLastErrorMsg conn = peekCAString  =<< c'SEgetLastErrorMsg conn 

--------------------------------------------------------------------------------
sednaTransactionStatus :: SednaConnection -> IO SednaResponseCode
sednaTransactionStatus conn = do resultCode <- c'SEtransactionStatus conn
                                 return $ fromCConstant resultCode

--------------------------------------------------------------------------------
sednaShowTime :: SednaConnection -> IO String
sednaShowTime conn = peekCAString =<< c'SEshowTime conn

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
                       -> IO ()
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
            case fromCConstant response of
              SetAttributeSucceeded  -> return ()
              _                      -> throw SednaFailedException)

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
sednaResetAllConnectionAttr :: SednaConnection -> IO ()
sednaResetAllConnectionAttr conn = do
  resultCode <- c'SEresetAllConnectionAttr conn
  case fromCConstant resultCode of
    ResetAttributeSucceeded -> return ()
    _                       -> throw SednaFailedException

--------------------------------------------------------------------------------
sednaGetResultString :: SednaConnection -> IO QueryResult
sednaGetResultString conn = procItemStream conn 8 getXMLData

--------------------------------------------------------------------------------
getXMLData :: (Monad m) => Iteratee [ByteString] m QueryResult
getXMLData = icont (step C.empty) Nothing
    where
      step acc (Chunk bs) 
          | bs == []  = icont (step acc) Nothing
          | otherwise = icont (step $ C.append acc (C.concat $ bs)) Nothing
      step acc (EOF _)                = idone (C.unpack acc) (EOF Nothing)

--------------------------------------------------------------------------------
procItemStream :: SednaConnection ->  Int -> Iteratee [ByteString] IO a -> IO a
procItemStream conn size iter = step iter
    where step iter' = do
            iter'' <- enumItemChunked conn size iter' >>= run
            res    <- sednaNext conn
            case res of
              NextItemSucceeded -> step iter''
              ResultEnd         -> run iter''
              NextItemFailed    -> throw SednaNextItemFailedException
              _                 -> throw SednaFailedException

--------------------------------------------------------------------------------
enumItemChunked  :: SednaConnection
                 -> Int
                 -> Iteratee [ByteString] IO a
                 -> IO (Iteratee ByteString IO (Iteratee [ByteString] IO a))
enumItemChunked conn size = (enumItem conn size) .  I.group size

--------------------------------------------------------------------------------
enumItem :: SednaConnection -> Int -> Enumerator ByteString IO a
enumItem conn size = enumFromCallback cb ()
    where
      cb () = do
        (code, result) <- sednaGetData conn size
        case code of
          OperationSucceeded -> return $ Right ((True, ()), result)
          ResultEnd          -> return $ Right ((False, ()), result)
          _                  -> throw SednaFailedException

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
        if response == DataChunkLoaded
          then liftIO (print s) >>  liftI step
          else throw SednaFailedException

    step stream = do
      response <- liftIO $ sednaEndLoadData conn
      case response of
         BulkLoadSucceeded -> liftIO  (sednaCommit conn) >> idone () stream
         BulkLoadFailed    -> throw SednaBulkLoadFailedException
         _                 -> throw SednaFailedException

--------------------------------------------------------------------------------
sednaLoadFile :: FilePath -> SednaConnection ->  Document -> Collection -> IO ()
sednaLoadFile file conn doc coll = do
   iteratee  <- enumFile 8 file $ loadXMLBytes 
                                  conn 
                                  doc 
                                  coll
   run iteratee
