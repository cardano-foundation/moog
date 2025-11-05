module Test.QuickCheck.Same
    ( Same (Same, NotSame)
    , tryDifferent
    , theSame
    , isTheSame
    ) where

import Test.QuickCheck
    ( Arbitrary (..)
    , elements
    , suchThat
    )

data Same a = Same a | NotSame a a
    deriving (Show, Eq)

tryDifferent :: Same a -> a
tryDifferent (Same a) = a
tryDifferent (NotSame _ a) = a

theSame :: Same a -> a
theSame (Same a) = a
theSame (NotSame a _) = a

instance (Eq a, Arbitrary a) => Arbitrary (Same a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary `suchThat` (/= a)
        elements [Same a, NotSame a b]

isTheSame :: Same a -> Bool
isTheSame (Same _) = True
isTheSame (NotSame _ _) = False
