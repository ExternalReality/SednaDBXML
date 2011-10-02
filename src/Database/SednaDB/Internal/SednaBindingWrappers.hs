module Database.SednaDB.Internal.SednaBindingWrappers 
    ( sednaGetResultString
    , sednaQuery
    , sednaQueryFromFile
    , sednaLoadFile
    ) where

--------------------------------------------------------------------------------
import Control.Exception
import Control.Monad.Trans
import Data.ByteString as BS
import Data.ByteString.Char8 as C (pack,unpack,concat,append, empty)
import Data.Maybe
import Foreign
import Prelude hiding             (replicate,concat)

import Data.Iteratee as I hiding  (mapM_, peek)
import Data.Iteratee.IO

import Database.SednaDB.Internal.SednaBindings
import Database.SednaDB.Internal.SednaResponseCodes
import Database.SednaDB.Internal.SednaExceptions

--------------------------------------------------------------------------------
sednaGetResultString :: SednaConnection -> IO String
sednaGetResultString conn = procItemStream conn 8 getXMLData

--------------------------------------------------------------------------------
getXMLData :: (Monad m) => Iteratee [ByteString] m String
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
sednaLoadFile :: SednaConnection
              -> String
              -> String
              -> String
              -> IO ()
sednaLoadFile conn file doc coll = do
  iteratee <- enumFile 8 file $ loadXMLBytes conn doc coll
  run iteratee

--------------------------------------------------------------------------------
sednaQuery :: String -> IO ()
sednaQuery = undefined

--------------------------------------------------------------------------------
sednaQueryFromFile :: String -> IO ()
sednaQueryFromFile = undefined

--------------------------------------------------------------------------------


