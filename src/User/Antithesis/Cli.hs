-- | User-facing Antithesis CLI commands.
module User.Antithesis.Cli
    ( AntithesisCommand (..)
    , antithesisCmd
    , antithesisCommandParser
    )
where

import Data.Aeson qualified as Aeson
import Lib.Box (Box (..))
import Lib.JSON.Canonical.Extra ()
import OptEnvConf (Parser, command, commands)

data AntithesisCommand a where
    Runs :: AntithesisCommand Aeson.Value

antithesisCmd :: AntithesisCommand a -> IO a
antithesisCmd = \case
    Runs -> pure $ Aeson.object []

antithesisCommandParser :: Parser (Box AntithesisCommand)
antithesisCommandParser =
    commands
        [ command "runs" runsHelp $
            pure $ Box Runs
        ]

runsHelp :: String
runsHelp =
    "List Antithesis runs via the Moog proxy. Exit codes: \
    \0 success, 2 authorization or SSO failure, 3 proxy or network failure, \
    \4 invalid JSON from proxy."
