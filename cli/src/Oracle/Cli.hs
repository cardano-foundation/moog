module Oracle.Cli
    ( OracleCommand (..)
    , oracleCmd
    ) where

import Core.Types (TokenId, Wallet)
import Oracle.Token.Cli (TokenCommand, tokenCmdCore)
import Oracle.Validate.Cli (ValidateCommand, validateCmd)
import Servant.Client (ClientM)
import Submitting (Submitting)
import Text.JSON.Canonical (JSValue)

data OracleCommand a where
    OracleTokenCommand :: TokenCommand a -> OracleCommand a
    OracleValidateCommand :: ValidateCommand -> OracleCommand JSValue

deriving instance Show (OracleCommand a)
deriving instance Eq (OracleCommand a)

oracleCmd
    :: Submitting -> Wallet -> Maybe TokenId -> OracleCommand a -> ClientM a
oracleCmd sbmt wallet (Just tk) = \case
    OracleTokenCommand tokenCommand ->
        tokenCmdCore
            sbmt
            wallet
            (Just tk)
            tokenCommand
    OracleValidateCommand validateCommand ->
        validateCmd
            tk
            validateCommand
oracleCmd sbmt wallet Nothing = \case
    OracleTokenCommand tokenCommand ->
        tokenCmdCore
            sbmt
            wallet
            Nothing
            tokenCommand
    OracleValidateCommand _validateCommand ->
        error "TokenId is required for ValidateCommand"
