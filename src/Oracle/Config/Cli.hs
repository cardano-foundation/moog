module Oracle.Config.Cli
    ( configCmd
    , ConfigCmd (..)
    )
where

import Control.Monad.Trans.Class (lift)
import Core.Context (WithContext, askMpfs, askSubmit, askValidation)
import Core.Types.Basic (Success (..), TokenId)
import Core.Types.Fact (Fact (..))
import Core.Types.Tx (WithTxHash (..), setWithTxHashValue)
import Core.Types.Wallet (Wallet)
import Facts (FactsSelection (..), factsCmd)
import MPFS.API
    ( MPFS (mpfsRequestInsert, mpfsRequestUpdate)
    , RequestInsertBody (RequestInsertBody, key, value)
    , RequestUpdateBody (RequestUpdateBody, key, newValue, oldValue)
    )
import Oracle.Config.Types
    ( Config (..)
    , ConfigKey (ConfigKey)
    )
import Submitting (Submission (..))
import Text.JSON.Canonical (ToJSON (toJSON))

data ConfigCmd a where
    SetConfig
        :: TokenId -> Wallet -> Config -> ConfigCmd (WithTxHash Success)

deriving instance Show (ConfigCmd a)
deriving instance Eq (ConfigCmd a)

configCmd
    :: Monad m => ConfigCmd a -> WithContext m a
configCmd (SetConfig tokenId wallet config) = do
    mpfs <- askMpfs
    Submission submit <- askSubmit wallet
    validation <- askValidation $ Just tokenId
    present <- lift $ factsCmd validation ConfigFact
    jkey <- toJSON ConfigKey
    jvalue <- toJSON config
    let run = lift . fmap (`setWithTxHashValue` Success) . submit
    case present of
        [oldConfig] -> do
            oldValue <- toJSON $ factValue oldConfig
            run $ \address ->
                mpfsRequestUpdate mpfs address tokenId
                    $ RequestUpdateBody
                        { key = jkey
                        , newValue = jvalue
                        , oldValue
                        }
        _ -> do
            run $ \address ->
                mpfsRequestInsert mpfs address tokenId
                    $ RequestInsertBody{key = jkey, value = jvalue}
