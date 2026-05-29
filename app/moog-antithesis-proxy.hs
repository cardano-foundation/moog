import Proxy.Antithesis.Config (loadSettings)

main :: IO ()
main = do
    _settings <- loadSettings
    pure ()
