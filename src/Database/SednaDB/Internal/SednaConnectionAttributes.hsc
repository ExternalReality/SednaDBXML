module Database.SednaDB.Internal.SednaConnectionAttributes where

import Foreign.C.Types

#include <libsedna.h>
#include <sp_defs.h>

newtype SednaConnectionAttr = SednaConnectionAttr { sednaConnectionAttr :: CInt } deriving(Eq, Ord, Show)
newtype SednaConnAttrValue  = SednaConnAttrValue  { sednaConnAttrValue  :: CInt } deriving(Eq, Ord, Show)

#{enum SednaConnectionAttr, SednaConnectionAttr
 , attrAutoCommit                     = SEDNA_ATTR_AUTOCOMMIT
 , attrSessionDirectory               = SEDNA_ATTR_SESSION_DIRECTORY
 , attrDebug                          = SEDNA_ATTR_DEBUG
 , attrBoundarySpacePreserveWhileLoad = SEDNA_ATTR_BOUNDARY_SPACE_PRESERVE_WHILE_LOAD
 , attrConcurrencyType                = SEDNA_ATTR_CONCURRENCY_TYPE
 , attrQueryExecTimeOut               = SEDNA_ATTR_QUERY_EXEC_TIMEOUT
 , attrLogAmount                      = SEDNA_ATTR_LOG_AMOUNT
 , attrMaxResultSize                  = SEDNA_ATTR_MAX_RESULT_SIZE
 }

#{enum SednaConnAttrValue, SednaConnAttrValue
 , autoCommitOff                = SEDNA_AUTOCOMMIT_OFF                     
 , autoCommitOn                 = SEDNA_AUTOCOMMIT_ON
 , readOnlyTransaction          = SEDNA_READONLY_TRANSACTION
 , updateTransaction            = SEDNA_UPDATE_TRANSACTION
 , debugOn                      = SEDNA_DEBUG_ON
 , debugOff                     = SEDNA_DEBUG_OFF
 , logLess                      = SEDNA_LOG_LESS
 , logFull                      = SEDNA_LOG_FULL
 , boundarySpacePreserveOn      = SEDNA_BOUNDARY_SPACE_PRESERVE_ON
 , boundarySpacePreserveOff     = SEDNA_BOUNDARY_SPACE_PRESERVE_OFF
 }
