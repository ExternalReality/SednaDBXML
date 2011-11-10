module Database.SednaTypes
    ( SednaConnection
    , Query
    , Document
    , Collection
    , URL
    , Password
    , DBName
    , UserName
    ) where


--------------------------------------------------------------------------------
import Foreign (Ptr)
import Database.Internal.SednaCBindings (C'SednaConnection)


--------------------------------------------------------------------------------
type URL = String 


--------------------------------------------------------------------------------
type DBName = String


--------------------------------------------------------------------------------
type UserName = String
type Password = String


--------------------------------------------------------------------------------
type Collection = String
type Document   = String


--------------------------------------------------------------------------------
type SednaConnection = Ptr C'SednaConnection


--------------------------------------------------------------------------------
type Query       = String

--------------------------------------------------------------------------------
type ErrorMsg = String