{-# LANGUAGE OverloadedStrings #-}

module User.Antithesis.LoginSpec
    ( spec
    )
where

import Lib.GitHub.Auth.DeviceFlow (OAuthToken (..))
import System.FilePath ((</>))
import System.Posix.Files
    ( fileMode
    , getFileStatus
    , intersectFileModes
    , ownerReadMode
    , ownerWriteMode
    , unionFileModes
    )
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , shouldBe
    , shouldReturn
    )
import System.IO.Temp (withSystemTempDirectory)
import User.Antithesis.Login
    ( ensureTokenAt
    , readCachedToken
    , writeCachedToken
    )

spec :: Spec
spec =
    describe "User.Antithesis.Login" $ do
        it "writes a missing token cache with mode 0600" $
            withSystemTempDirectory "moog-antithesis-login" $ \tmp -> do
                let path = tmp </> "github-oauth.json"
                    expected = OAuthToken "gh-token-one"

                token <- ensureTokenAt path $ pure $ Right expected

                token `shouldBe` expected
                readCachedToken path `shouldReturn` Just expected
                assertMode0600 path

        it "reads a cached token without running the device flow" $
            withSystemTempDirectory "moog-antithesis-login" $ \tmp -> do
                let path = tmp </> "github-oauth.json"
                    expected = OAuthToken "gh-token-two"
                writeCachedToken path expected

                token <-
                    ensureTokenAt path $ do
                        expectationFailure "device flow should not run"
                        pure $ Right $ OAuthToken "unexpected"

                token `shouldBe` expected

assertMode0600 :: FilePath -> IO ()
assertMode0600 path = do
    status <- getFileStatus path
    let permissionBits = fileMode status `intersectFileModes` 0o777
    permissionBits `shouldBe` (ownerReadMode `unionFileModes` ownerWriteMode)
