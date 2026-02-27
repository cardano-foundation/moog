module Docker
    ( build
    , dockerCompose
    )
where

import Core.Types.Basic
import Lib.System (runSystemCommand)

type CLI = (String, [String])

build
    :: FilePath -- Dockerfile path
    -> FilePath -- Context path
    -> String -- Tag
    -> String -- Label
    -> CLI
build dockerfile context tag label =
    ( "docker"
    ,
        [ "build"
        , "-f"
        , dockerfile
        , "-t"
        , tag
        , "--label"
        , label
        , context
        ]
    )

dockerCompose :: Directory -> [String] -> IO (Either String String)
dockerCompose (Directory dirname) args = do
    runSystemCommand
        [("INTERNAL_NETWORK", "true")]
        "docker"
        $ ["compose", "--project-directory", dirname] ++ args
