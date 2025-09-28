module Oracle.Validate.Requests.Lib
    ( keyAlreadyPendingFailure
    ) where

import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.List (find)
import Effects (Effects (..))
import Oracle.Types (RequestZoo)
import Oracle.Validate.Types (Validate, throwJusts)

keyAlreadyPendingFailure
    :: (Monad m, Eq k)
    => Effects m
    -> (k -> e)
    -> k
    -> (RequestZoo -> Maybe k)
    -> Validate e m ()
keyAlreadyPendingFailure Effects{mpfsGetTokenRequests} e key requestZooGetRegisterUserKey = do
    rqs <- lift mpfsGetTokenRequests
    void
        $ throwJusts
        $ e key
            <$ find (\r -> requestZooGetRegisterUserKey r == Just key) rqs
