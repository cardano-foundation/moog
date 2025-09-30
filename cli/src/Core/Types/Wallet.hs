{-# LANGUAGE StrictData #-}

module Core.Types.Wallet
    ( Wallet (..)
    ) where

import Core.Types.Basic (Address, Owner)
import Core.Types.Mnemonics (Mnemonics, MnemonicsPhase (DecryptedS))
import Core.Types.Tx (SignTxError, SignedTx, UnsignedTx)
import Crypto.PubKey.Ed25519 qualified as Ed25519

data Wallet = Wallet
    { address :: Address
    , owner :: Owner
    , sign :: UnsignedTx -> Either SignTxError SignedTx
    , encrypted :: Bool
    , mnemonics :: Mnemonics 'DecryptedS
    , privateKey :: Ed25519.SecretKey
    , publicKey :: String -- bech32-encoded public key
    }

instance Show Wallet where
    show (Wallet addr owner _ encrypted _ _ publicKey) =
        "Wallet { address: "
            ++ show addr
            ++ ", owner: "
            ++ show owner
            ++ ", encrypted: "
            ++ show encrypted
            ++ ", publicKey: "
            ++ publicKey
            ++ "}"

instance Eq Wallet where
    (Wallet addr1 owner1 _ _ _ _ _) == (Wallet addr2 owner2 _ _ _ _ _) =
        addr1 == addr2 && owner1 == owner2
