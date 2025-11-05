module Test.QuickCheck.Commit
    ( CommitValue (..)
    )
where

import Test.QuickCheck (Arbitrary (..), elements, vectorOf)

-- d9fb8d2bcfa321497ae3a89244bf13513a9a9a14
sha1Chars :: String
sha1Chars = ['0' .. '9'] <> ['a' .. 'f']

newtype CommitValue = CommitValue {getCommitValue :: String}
    deriving (Show, Eq)

instance Arbitrary CommitValue where
    arbitrary = CommitValue <$> vectorOf 40 (elements sha1Chars)
