module Database.SednaDB.Sedna where

import Control.Monad.Identity 
import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader

import Data.ByteString hiding (head)
import Data.Enumerator

--iriscouch.com
--couchbase.com
--cloudant.com
--couchdb.apache.org

import Data.Maybe (fromJust)

import Prelude hiding (head, concat)

import Database.SednaDB.Internal.BindingWrappers

type SednaTransaction a = ReaderT String (ErrorT String (WriterT [String] (StateT SednaConnection IO))) a 
                           
-- sednaLoadXMLData:: (MonadState s m) => String -> String -> Iteratee ByteString m () 
-- sednaLoadXMLData coll doc = do
--   chunk  <- head
--   conn   <- get
--   case chunk of
--     Nothing   -> return ()
--     Just buff -> do 
--       response <- returnI $ sednaLoadData conn (fromJust chunk) coll doc
--       sednaLoadXMLData coll doc
      
sednaLoadXMLData :: SednaConnection -> String -> String -> Step ByteString IO ()
sednaLoadXMLData conn coll doc = Continue $ go conn coll doc
  where
    go :: SednaConnection -> String -> String -> Stream ByteString -> Iteratee ByteString IO ()
    go conn coll doc (Chunks bytes) = do 
      let m = sednaLoadData conn (concat bytes) coll doc
      continue $ go conn coll doc
   
    go conn coll doc EOF = yield () EOF


