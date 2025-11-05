module Test.QuickCheck.EGen
    ( gen
    , egenProperty
    , genA
    , genShrink
    , genShrinkA
    , genShow
    , genShowA
    , genShrinkShow
    , genShrinkShowA
    , genBlind
    , genBlindA
    , genShrinkBlind
    , EGen
    )
where

import Control.Monad.Trans.Cont (Cont, cont, runCont)
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , forAllShow
    , forAllShrinkShow
    )
import Test.QuickCheck.Property
    ( Property
    , Testable (property)
    , forAll
    , forAllBlind
    , forAllShrink
    , forAllShrinkBlind
    )

newtype EGen a = EGen (Cont Property a)
    deriving (Functor, Applicative, Monad)

egen :: ((a -> Property) -> Property) -> EGen a
egen = EGen . cont

gen :: Show a => Gen a -> EGen a
gen = egen . forAll

genA :: (Show a, Arbitrary a) => EGen a
genA = gen arbitrary

genShrink :: Show a => Gen a -> (a -> [a]) -> EGen a
genShrink g s = egen $ forAllShrink g s

genShrinkA :: (Show a, Arbitrary a) => EGen a
genShrinkA = genShrink arbitrary shrink

genShow :: Gen a -> (a -> String) -> EGen a
genShow g s = egen $ forAllShow g s

genShowA :: (Show a, Arbitrary a) => EGen a
genShowA = genShow arbitrary show

genShrinkShow
    :: Gen a -> (a -> [a]) -> (a -> String) -> EGen a
genShrinkShow g s showFn = egen $ forAllShrinkShow g s showFn

genShrinkShowA :: (Show a, Arbitrary a) => EGen a
genShrinkShowA = genShrinkShow arbitrary shrink show

genBlind :: Gen a -> EGen a
genBlind g = egen $ forAllBlind g

genBlindA :: Arbitrary a => EGen a
genBlindA = genBlind arbitrary

genShrinkBlind :: Gen a -> (a -> [a]) -> EGen a
genShrinkBlind g s = egen $ forAllShrinkBlind g s

egenProperty :: Testable a => EGen a -> Property
egenProperty (EGen f) = runCont f property
