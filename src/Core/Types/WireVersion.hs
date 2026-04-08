module Core.Types.WireVersion
    ( currentWireVersion
    , readWireVersion
    , wireVersionField
    )
where

import Data.Map.Strict (Map)
import Lib.JSON.Canonical.Extra
    ( getFieldWithDefault
    , intJSON
    )
import Text.JSON.Canonical
    ( Int54
    , JSValue
    , ReportSchemaErrors
    )

-- | Current wire format version for on-chain fact
-- values.
currentWireVersion :: Int54
currentWireVersion = 1

-- | Read the wire version from a JSON object mapping,
-- defaulting to 0 for backwards compatibility with
-- pre-versioned facts.
readWireVersion
    :: ReportSchemaErrors m
    => Map String JSValue
    -> m Int54
readWireVersion = getFieldWithDefault "v" 0

-- | Key-value pair for including the current version
-- in a JSON object.
wireVersionField :: Monad m => (String, m JSValue)
wireVersionField = ("v", intJSON currentWireVersion)
