#include <bindings.dsl.h>
#include <sp_defs.h>

--------------------------------------------------------------------------------

module Database.Internal.SpDefs where

--------------------------------------------------------------------------------

import Foreign
import Foreign.C.Types

--------------------------------------------------------------------------------

#num  SEDNA_MAX_RESULT_SIZE

--------------------------------------------------------------------------------

#integral_t enum se_debug_info_type
#num se_QueryTrace
#num se_QueryDebug

--------------------------------------------------------------------------------

#integral_t sp_int32

--------------------------------------------------------------------------------

#starttype struct msg_struct
#field       instruction , <sp_int32>
#field       length      , <sp_int32>
#array_field body        , CChar
#stoptype
