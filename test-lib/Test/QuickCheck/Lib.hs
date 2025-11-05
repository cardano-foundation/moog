module Test.QuickCheck.Lib
    ( withAPresence
    , withNothing
    , withAPresenceInAList
    ) where

import Test.QuickCheck
    ( Gen
    , frequency
    , listOf
    , suchThat
    )

oneOrTheOther :: Float -> Gen a -> Gen a -> Gen a
oneOrTheOther presence genA genB = do
    let presence100 = round (presence * 100)
    frequency
        [ (presence100, genA)
        , (100 - presence100, genB)
        ]

withAPresence :: Eq a => Float -> a -> Gen a -> Gen a
withAPresence presence a generate = do
    b <- generate `suchThat` (/= a)
    oneOrTheOther
        presence
        (pure a)
        (pure b)

withNothing :: Float -> Gen a -> Gen (Maybe a)
withNothing presence gen = do
    bool <- oneOrTheOther presence (return True) (return False)
    if bool
        then Just <$> gen
        else pure Nothing

withAPresenceInAList :: Eq a => Float -> a -> Gen a -> Gen [a]
withAPresenceInAList presence a generate = do
    bs <- listOf $ generate `suchThat` (/= a)
    oneOrTheOther
        presence
        (pure $ a : bs)
        (pure bs)
