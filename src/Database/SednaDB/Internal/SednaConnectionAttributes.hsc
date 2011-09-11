module Database.SednaDB.Internal.SednaConnectionAttributes where

import Foreign.C.Types

#include <libsedna.h>

newtype SednaConnectionAttr = SednaConnectionAttr { sednaConnectionAttr :: CInt }

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