module Lib.EdToCurveSpec (spec) where

import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Curve25519 qualified as Curve25519
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteString qualified as B
import Lib.EdToCurve
    ( convertPublicKey
    , convertSecretKey
    )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "EdToCurve public golden" $ do
        it "should convert edward1pk to curve1pk" $ do
            convertPublicKey edward1pk
                `shouldBe` Right curve1pk
        it "should convert edward2p to curve2pk" $ do
            convertPublicKey edward2pk
                `shouldBe` Right curve2pk
        it "should convert edward3pk to curve3pk" $ do
            convertPublicKey edward3pk
                `shouldBe` Right curve3pk
        it "should convert edward4pk to curve4pk" $ do
            convertPublicKey edward4pk
                `shouldBe` Right curve4pk
    describe "EdToCurve secret golden" $ do
        it "should convert edward1sk to curve1sk" $ do
            convertSecretKey edward1sk
                `shouldBe` Right curve1sk
        it "should convert edward2sk to curve2sk" $ do
            convertSecretKey edward2sk
                `shouldBe` Right curve2sk
        it "should convert edward3sk to curve3sk" $ do
            convertSecretKey edward3sk
                `shouldBe` Right curve3sk
        it "should convert edward4sk to curve4sk" $ do
            convertSecretKey edward4sk
                `shouldBe` Right curve4sk

failCrypto :: CryptoFailable a -> a
failCrypto (CryptoFailed err) = error (show err)
failCrypto (CryptoPassed a) = a

edward1pk :: Ed25519.PublicKey
edward1pk =
    failCrypto
        $ Ed25519.publicKey
        $ B.pack
            [ 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            ]
curve1pk :: Curve25519.PublicKey
curve1pk =
    failCrypto
        $ Curve25519.publicKey
        $ B.pack
            [ 1
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            ]

edward2pk :: Ed25519.PublicKey
edward2pk =
    failCrypto
        $ Ed25519.publicKey
        $ B.pack
            [ 59
            , 106
            , 39
            , 188
            , 206
            , 182
            , 164
            , 45
            , 98
            , 163
            , 168
            , 208
            , 42
            , 111
            , 13
            , 115
            , 101
            , 50
            , 21
            , 119
            , 29
            , 226
            , 67
            , 166
            , 58
            , 192
            , 72
            , 161
            , 139
            , 89
            , 218
            , 41
            ]
curve2pk :: Curve25519.PublicKey
curve2pk =
    failCrypto
        $ Curve25519.publicKey
        $ B.pack
            [ 91
            , 245
            , 92
            , 115
            , 184
            , 46
            , 190
            , 34
            , 190
            , 128
            , 243
            , 67
            , 6
            , 103
            , 175
            , 87
            , 15
            , 174
            , 37
            , 86
            , 166
            , 65
            , 94
            , 107
            , 48
            , 212
            , 6
            , 83
            , 0
            , 170
            , 148
            , 125
            ]

edward3pk :: Ed25519.PublicKey
edward3pk =
    failCrypto
        $ Ed25519.publicKey
        $ B.pack
            [ 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            ]

curve3pk :: Curve25519.PublicKey
curve3pk =
    failCrypto
        $ Curve25519.publicKey
        $ B.pack
            [ 63
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            , 75
            ]

edward4pk :: Ed25519.PublicKey
edward4pk =
    failCrypto
        $ Ed25519.publicKey
        $ B.pack
            [ 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            ]
curve4pk :: Curve25519.PublicKey
curve4pk =
    failCrypto
        $ Curve25519.publicKey
        $ B.pack
            [ 104
            , 95
            , 90
            , 83
            , 157
            , 170
            , 246
            , 17
            , 94
            , 54
            , 137
            , 75
            , 37
            , 49
            , 21
            , 237
            , 195
            , 120
            , 99
            , 23
            , 144
            , 27
            , 226
            , 175
            , 12
            , 45
            , 209
            , 61
            , 12
            , 42
            , 81
            , 21
            ]

edward1sk :: Ed25519.SecretKey
edward1sk =
    failCrypto
        $ Ed25519.secretKey
        $ B.pack
            [ 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
            ]
curve1sk :: Curve25519.SecretKey
curve1sk =
    failCrypto
        $ Curve25519.secretKey
        $ B.pack
            [ 80
            , 70
            , 173
            , 193
            , 219
            , 168
            , 56
            , 134
            , 123
            , 43
            , 187
            , 253
            , 208
            , 195
            , 66
            , 62
            , 88
            , 181
            , 121
            , 112
            , 181
            , 38
            , 122
            , 144
            , 245
            , 121
            , 96
            , 146
            , 74
            , 135
            , 241
            , 86
            ]

edward2sk :: Ed25519.SecretKey
edward2sk =
    failCrypto
        $ Ed25519.secretKey
        $ B.pack
            [ 59
            , 106
            , 39
            , 188
            , 206
            , 182
            , 164
            , 45
            , 98
            , 163
            , 168
            , 208
            , 42
            , 111
            , 13
            , 115
            , 101
            , 50
            , 21
            , 119
            , 29
            , 226
            , 67
            , 166
            , 58
            , 192
            , 72
            , 161
            , 139
            , 89
            , 218
            , 41
            ]
curve2sk :: Curve25519.SecretKey
curve2sk =
    failCrypto
        $ Curve25519.secretKey
        $ B.pack
            [ 120
            , 213
            , 207
            , 198
            , 121
            , 171
            , 99
            , 66
            , 88
            , 192
            , 85
            , 232
            , 188
            , 134
            , 28
            , 208
            , 38
            , 161
            , 106
            , 36
            , 31
            , 16
            , 193
            , 27
            , 94
            , 41
            , 51
            , 63
            , 180
            , 215
            , 198
            , 95
            ]

edward3sk :: Ed25519.SecretKey
edward3sk =
    failCrypto
        $ Ed25519.secretKey
        $ B.pack
            [ 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            , 255
            ]
curve3sk :: Curve25519.SecretKey
curve3sk =
    failCrypto
        $ Curve25519.secretKey
        $ B.pack
            [ 32
            , 205
            , 105
            , 53
            , 134
            , 71
            , 22
            , 167
            , 157
            , 116
            , 221
            , 95
            , 171
            , 189
            , 137
            , 100
            , 48
            , 64
            , 81
            , 202
            , 65
            , 163
            , 28
            , 70
            , 89
            , 21
            , 142
            , 187
            , 124
            , 61
            , 11
            , 87
            ]

edward4sk :: Ed25519.SecretKey
edward4sk =
    failCrypto
        $ Ed25519.secretKey
        $ B.pack
            [ 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            , 1
            , 0
            ]
curve4sk :: Curve25519.SecretKey
curve4sk =
    failCrypto
        $ Curve25519.secretKey
        $ B.pack
            [ 80
            , 3
            , 130
            , 244
            , 162
            , 119
            , 86
            , 76
            , 89
            , 94
            , 70
            , 69
            , 118
            , 240
            , 130
            , 108
            , 79
            , 241
            , 99
            , 40
            , 92
            , 94
            , 4
            , 161
            , 16
            , 251
            , 13
            , 126
            , 154
            , 225
            , 205
            , 99
            ]
