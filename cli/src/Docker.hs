module Docker
    ( build
    , dockerCompose
    , collectImagesFromAssets
    )
where

import Core.Types.Basic
import Data.Functor ((<&>))
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
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

collectImagesFromAssets :: Directory -> IO (Either String [String])
collectImagesFromAssets dirname = do
    output <- dockerCompose dirname ["config", "--images"]
    let images = uniqueAfterSort . filter (not . null) . lines
    return $ output <&> images

-- Function to get unique elements after sorting a list
uniqueAfterSort :: Ord a => [a] -> [a]
uniqueAfterSort xs = map NE.head (NE.group (sort xs))
