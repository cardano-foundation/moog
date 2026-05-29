import Data.String (fromString)
import Network.Wai.Handler.Warp qualified as Warp
import Proxy.Antithesis.Application (productionApplication)
import Proxy.Antithesis.Config
    ( Settings (..)
    , loadSettings
    )

main :: IO ()
main = do
    settings <- loadSettings
    application <- productionApplication settings
    Warp.runSettings (warpSettings settings) application

warpSettings :: Settings -> Warp.Settings
warpSettings settings =
    Warp.setHost (fromString $ settingsBindAddr settings)
        $ Warp.setPort (settingsBindPort settings)
        $ Warp.defaultSettings
