module Oracle.Token.Cli
    ( tokenCmdCore
    , TokenCommand (..)
    , TokenUpdateFailure (..)
    ) where

import Control.Exception (Exception)
import Control.Monad (void, when)
import Control.Monad.Trans.Class (lift)
import Core.Context
    ( WithContext
    , askConfig
    , askMpfs
    , askSubmit
    , askValidation
    )
import Core.Types.Basic (RequestRefId, TokenId)
import Core.Types.Tx (TxHash, WithTxHash (WithTxHash))
import Core.Types.Wallet (Wallet (..))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Data.List (find)
import Lib.JSON.Canonical.Extra (object, (.=))
import MPFS.API
    ( MPFS (..)
    , mpfsGetToken
    )
import Oracle.Types
    ( RequestZoo
    , Token (..)
    , TokenState (..)
    , requestZooRefId
    )
import Oracle.Validate.Failure (RequestValidationFailure)
import Oracle.Validate.Request
    ( validateRequest
    )
import Oracle.Validate.Types
    ( AValidationResult (..)
    , Validate
    , liftMaybe
    , mapFailure
    , notValidated
    , runValidate
    , sequenceValidate
    )
import Submitting (Submission (..))
import Text.JSON.Canonical
    ( FromJSON (fromJSON)
    , ToJSON (..)
    )

data TokenUpdateRequestValidation = TokenUpdateRequestValidation
    { validationRequestId :: RequestRefId
    , problem :: TokenUpdateRequestValidationProblem
    }
    deriving (Show, Eq)

instance Monad m => ToJSON m TokenUpdateRequestValidation where
    toJSON (TokenUpdateRequestValidation reqId problem) =
        object
            [ "requestId" .= reqId
            , "problem" .= problem
            ]

data TokenUpdateRequestValidationProblem
    = TokenUpdateRequestNotFound
    | TokenUpdateRequestValidationFailure
        RequestValidationFailure
    deriving (Show, Eq)

instance Monad m => ToJSON m TokenUpdateRequestValidationProblem where
    toJSON = \case
        TokenUpdateRequestNotFound ->
            toJSON ("Request not found" :: String)
        TokenUpdateRequestValidationFailure e ->
            object ["validationFailure" .= e]

data TokenUpdateFailure
    = TokenNotParsable TokenId
    | TokenUpdateOfNoRequests
    | TokenUpdateRequestValidations
        [TokenUpdateRequestValidation]
    | TokenUpdateConfigNotFound
    | TokenUpdateNotRequestedFromTokenOwner
    deriving (Show, Eq)

instance Exception TokenUpdateFailure

data TokenBootFailure
    = NoTokenIdReturned
    | TokenIdNotValidJSON
    deriving (Show, Eq)

instance Monad m => ToJSON m TokenBootFailure where
    toJSON = \case
        NoTokenIdReturned ->
            toJSON ("No TokenId returned" :: String)
        TokenIdNotValidJSON ->
            toJSON ("TokenId is not valid JSON" :: String)

instance Monad m => ToJSON m TokenUpdateFailure where
    toJSON = \case
        TokenNotParsable tk ->
            object ["tokenNotParsable" .= tk]
        TokenUpdateOfNoRequests ->
            toJSON ("No requests to include" :: String)
        TokenUpdateRequestValidations validations ->
            object ["tokenUpdateRequestValidations" .= validations]
        TokenUpdateConfigNotFound ->
            toJSON ("Token update config not available" :: String)
        TokenUpdateNotRequestedFromTokenOwner ->
            toJSON ("Token update not requested from token owner" :: String)

data TokenCommand a where
    BootToken
        :: Wallet
        -> TokenCommand
            (AValidationResult TokenBootFailure (WithTxHash TokenId))
    UpdateToken
        :: TokenId
        -> Wallet
        -> [RequestRefId]
        -> TokenCommand
            ( AValidationResult
                TokenUpdateFailure
                TxHash
            )
    EndToken :: TokenId -> Wallet -> TokenCommand TxHash

deriving instance Show (TokenCommand a)
deriving instance Eq (TokenCommand a)

promoteFailure
    :: Functor m
    => RequestZoo
    -> Validate RequestValidationFailure m a
    -> Validate TokenUpdateRequestValidation m a
promoteFailure req =
    mapFailure
        ( TokenUpdateRequestValidation
            (requestZooRefId req)
            . TokenUpdateRequestValidationFailure
        )

tokenCmdCore
    :: Monad m
    => TokenCommand a
    -> WithContext m a
tokenCmdCore command = do
    mpfs <- askMpfs
    case command of
        -- validateRequest oracle mconfig validation
        UpdateToken tk wallet wanted -> do
            Submission submit <- askSubmit wallet
            validation <- askValidation $ Just tk
            mconfig <- askConfig tk
            lift $ runValidate $ do
                when (null wanted) $ notValidated TokenUpdateOfNoRequests
                mpendings <- lift $ fromJSON <$> mpfsGetToken mpfs tk
                token <- liftMaybe (TokenNotParsable tk) mpendings
                let requests = tokenRequests token
                    oracle = tokenOwner $ tokenState token
                when (owner wallet /= oracle)
                    $ notValidated TokenUpdateNotRequestedFromTokenOwner
                when (null requests) $ notValidated TokenUpdateOfNoRequests
                void
                    $ mapFailure TokenUpdateRequestValidations
                    $ sequenceValidate
                    $ wanted
                    <&> \req -> do
                        Identity tokenRequest <-
                            liftMaybe
                                (TokenUpdateRequestValidation req TokenUpdateRequestNotFound)
                                $ find ((== req) . requestZooRefId . runIdentity) requests
                        promoteFailure tokenRequest
                            $ validateRequest oracle mconfig validation tokenRequest
                WithTxHash txHash _ <- lift
                    $ submit
                    $ \address -> mpfsUpdateToken mpfs address tk wanted
                pure txHash
        BootToken wallet -> do
            Submission submit <- askSubmit wallet
            lift $ do
                WithTxHash txHash jTokenId <- submit $ mpfsBootToken mpfs
                runValidate $ do
                    tkId <- liftMaybe NoTokenIdReturned jTokenId
                    tokenId <- liftMaybe TokenIdNotValidJSON $ fromJSON tkId
                    pure $ WithTxHash txHash (Just tokenId)
        EndToken tk wallet -> do
            Submission submit <- askSubmit wallet
            lift $ do
                WithTxHash txHash _ <- submit
                    $ \address -> mpfsEndToken mpfs address tk
                pure txHash
