{-# LANGUAGE StrictData #-}

module Core.Types.CageDatum
    ( CageDatum (..)
    ) where

import Core.Types.Basic
import Core.Types.Change (Change (..))
import Core.Types.Operation (Operation (..))
import Core.Types.Tx (Root)
import PlutusTx (Data (..), builtinDataToData)
import PlutusTx.IsData.Class (FromData (..), fromData)
import Text.JSON.Canonical
    ( FromJSON (..)
    )

data CageDatum k op
    = RequestDatum
        { tokenId :: TokenId
        , owner :: Owner
        , change :: Change k op
        }
    | StateDatum
        { owner :: Owner
        , root :: Root
        }

deriving instance
    (Show k, Show (Operation op)) => Show (CageDatum k op)
deriving instance (Eq k, Eq (Operation op)) => Eq (CageDatum k op)

instance
    (FromJSON Maybe k, FromData (Operation op))
    => FromData (CageDatum k op)
    where
    fromBuiltinData = parse . builtinDataToData
      where
        parse = \case
            Constr 0 [tokenIdD, ownerD, keyD, valueD] ->
                RequestDatum
                    <$> fromData tokenIdD
                    <*> fromData ownerD
                    <*> (Change <$> fromData keyD <*> fromData valueD)
            Constr 1 [ownerD, rootD] ->
                StateDatum
                    <$> fromData ownerD
                    <*> fromData rootD
            _ -> Nothing
