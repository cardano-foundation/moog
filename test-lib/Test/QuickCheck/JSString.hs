module Test.QuickCheck.JSString
    ( genAscii
    )
where

import Data.Char (isAscii)
import Test.QuickCheck (Arbitrary (..), Gen, listOf, suchThat)

genAscii :: Gen [Char]
genAscii = listOf $ arbitrary `suchThat` isAscii
