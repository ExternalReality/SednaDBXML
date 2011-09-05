module Database.SednaDB.Connection where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import Database.SednaDB.Internal.Sedna

sednaConnect :: IO Int
sednaConnect = alloca $ \conn -> do
               url      <- newCString "localHost" 
               dbname   <- newCString "test" 
               login    <- newCString "" 
               password <- newCString "" 
               status   <- c'SEconnect conn url dbname login password
               return (fromIntegral status)