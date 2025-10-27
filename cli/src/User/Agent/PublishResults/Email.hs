{-
This is a module to check emails for test results.
-}
module User.Agent.PublishResults.Email
    ( readEmails
    , readEmail
    , printEmails
    , clockTimeToUTCTime
    , utcTimeToClockTime
    , Result (..)
    , Parameters (..)
    , EmailException (..)
    , ParsingError (..)
    , WithDateError (..)
    , EmailUser (..)
    , EmailPassword (..)
    )
where

import User.Types ( Outcome(..), TestRun )
import Control.Applicative (asum, (<|>))
import Control.Arrow (left)
import Control.Lens (view, (&), (^.), (^..))
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except
    ( ExceptT
    , except
    , runExceptT
    , withExceptT
    )
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.IMF
    ( Message
    , body
    , headerDate
    , message
    , parse
    )
import Data.List (sortBy)
import Data.MIME (EncStateWire, MIME (..), entities, mime)
import Data.MIME.TransferEncoding
    ( HasTransferEncoding (..)
    , transferDecoded'
    )
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
    ( LocalTime
    , UTCTime (..)
    , ZonedTime (zonedTimeToLocalTime)
    , addUTCTime
    , getCurrentTime
    , getCurrentTimeZone
    , secondsToNominalDiffTime
    , utcToLocalTime
    )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    , utcTimeToPOSIXSeconds
    )
import Data.Typeable (Typeable)
import Lib.JSON.Canonical.Extra (object, (.=))
import Network.HaskellNet.IMAP
    ( SearchQuery
    , fetch
    , login
    , search
    , select
    )
import Network.HaskellNet.IMAP.SSL
    ( SearchQuery (..)
    , connectIMAPSSL
    )
import Network.HaskellNet.IMAP.Types (MailboxName)
import Streaming
    ( Of
    , Stream
    )
import Streaming.Prelude (each)
import Streaming.Prelude qualified as S
import System.Directory.Internal.Prelude (Exception, try)
import System.Time
    ( ClockTime (..)
    , TimeDiff (..)
    , addToClockTime
    , getClockTime
    , noTimeDiff
    , toCalendarTime
    )
import Text.HTML.Parser (Attr (..), Token (..), parseTokens)
import Text.JSON.Canonical (FromJSON (..), parseCanonicalJSON)
import Text.JSON.Canonical.Class (ToJSON (..))
import Text.Read (readMaybe)
import User.Agent.PushTest (TestRunWithId (..))

newtype EmailUser = EmailUser String
    deriving (Show, Eq)

newtype EmailPassword = EmailPassword String
    deriving (Show, Eq)

newtype Hours = Hours Int
    deriving (Show, Eq)

tryX :: Exception e' => (e' -> e) -> IO c -> ExceptT e IO c
tryX f a = withExceptT f $ lift (try a) >>= except

data EmailException where
    ConnectionFailed :: IOError -> EmailException
    LoginFailed :: IOError -> EmailException
    ListFailed :: IOError -> EmailException
    SelectFailed :: IOError -> EmailException
    SearchFailed :: IOError -> EmailException
    FetchFailed :: IOError -> EmailException
    deriving (Show, Typeable, Eq)

instance Exception EmailException

data Result = Result
    { description :: TestRun
    , date :: ZonedTime
    , link :: T.Text
    , outcome :: Outcome
    }
    deriving (Show)

instance Monad m => ToJSON m Result where
    toJSON (Result desc d l s) =
        object
            [ "description" .= desc
            , "date" .= show d
            , "link" .= l
            , "success" .= s
            ]

instance Eq Result where
    a == b =
        description a == description b
            && zonedTimeToLocalTime (date a) == zonedTimeToLocalTime (date b)
            && link a == link b

compareResults
    :: Either ParsingError Result -> Either ParsingError Result -> Ordering
compareResults (Right a) (Right b) =
    compare
        (zonedTimeToLocalTime $ date b)
        (zonedTimeToLocalTime $ date a)
compareResults (Left (WithDate d' _)) (Left (WithDate d'' _)) =
    compare (zonedTimeToLocalTime d'') (zonedTimeToLocalTime d')
compareResults (Left (WithDate d' _)) (Right (Result _ d'' _ _)) =
    compare (zonedTimeToLocalTime d'') (zonedTimeToLocalTime d')
compareResults (Right (Result _ d' _ _)) (Left (WithDate d'' _)) =
    compare (zonedTimeToLocalTime d'') (zonedTimeToLocalTime d')
compareResults (Right _) _ = GT
compareResults (Left (WithDate _ _)) _ = GT
compareResults _ _ = LT

sortResults
    :: [Either ParsingError Result] -> [Either ParsingError Result]
sortResults = sortBy compareResults

keepInLimit
    :: LocalTime
    -> [Either ParsingError Result]
    -> [Either ParsingError Result]
keepInLimit limit = takeWhile $ not . isOld
  where
    isOld (Right (Result _ d _ _)) = zonedTimeToLocalTime d < limit
    isOld (Left (WithDate d _)) = zonedTimeToLocalTime d < limit
    isOld _ = False

data Parameters
    = Parameters
    { paramHours :: Hours
    , paramFrom :: String
    }
    deriving (Show, Eq)

parameters :: Parameters
parameters =
    Parameters
        { paramHours = Hours 24
        , paramFrom = "antithesis@cardanofoundation.org"
        }

printEmails
    :: Stream
        (Of (Either ParsingError Result))
        (ExceptT EmailException IO)
        ()
    -> IO (Either EmailException ())
printEmails emails = runExceptT $ S.mapM_ (liftIO . print) emails

readEmails
    :: EmailUser
    -> EmailPassword
    -> Int
    -- ^ days
    -- ^ limit to emails since this time
    -> Stream
        (Of (Either ParsingError Result))
        (ExceptT EmailException IO)
        ()
readEmails (EmailUser username) (EmailPassword password) past = do
    conn <- lift $ tryX ConnectionFailed $ connectIMAPSSL "imap.gmail.com"
    _ <- lift $ tryX LoginFailed $ login conn username password
    _ <- lift $ tryX SelectFailed $ select conn allMail
    now <- liftIO getCurrentTime
    let limit =
            addUTCTime
                (negate $ secondsToNominalDiffTime $ fromIntegral $ past * 24 * 60 * 60)
                now
    let clockLimit = utcTimeToClockTime limit
    tz <- liftIO getCurrentTimeZone -- wrong, should be the email server's timezone
    let localTimeLimit = utcToLocalTime tz limit
    let go from
            | from < clockLimit = pure ()
            | otherwise = do
                (timeSearch, to) <- liftIO $ nextSlot from (paramHours parameters)
                uids <-
                    lift
                        $ tryX SearchFailed
                        $ search conn
                        $ antithesisMail : timeSearch
                results <- forM uids $ \uid -> do
                    content <- lift $ tryX FetchFailed $ fetch conn uid
                    if B.null content
                        then pure Nothing
                        else pure $ Just $ readEmail content
                each $ keepInLimit localTimeLimit $ sortResults $ catMaybes results
                go to
    liftIO getClockTime >>= go . addToClockTime noTimeDiff{tdDay = 1}

nextSlot :: ClockTime -> Hours -> IO ([SearchQuery], ClockTime)
nextSlot now (Hours hours) = do
    let oldestTime = addToClockTime noTimeDiff{tdHour = -hours} now
    newestCalendar <- toCalendarTime now
    oldestCalendar <- toCalendarTime oldestTime
    pure
        ([NOTs $ BEFOREs oldestCalendar, BEFOREs newestCalendar], oldestTime)

antithesisMail :: SearchQuery
antithesisMail = FROMs (paramFrom parameters)

allMail :: MailboxName
allMail = "[Gmail]/All Mail"

data ParsingError
    = ParsingError String B.ByteString
    | WithDate ZonedTime WithDateError
    | NoDate
    deriving (Show)

data WithDateError
    = DescriptionMissingOrUnusable
    | LinkMissing
    deriving (Show, Eq)

readEmail
    :: B.ByteString -> Either ParsingError Result
readEmail input =
    case parse (message mime) input of
        Left errMsg -> Left $ ParsingError errMsg input
        Right msg -> parseEmail msg

toTokens :: B.ByteString -> [Token]
toTokens = parseTokens . T.decodeUtf8

nothingLeft :: a -> Maybe b -> Either a b
nothingLeft = flip maybe Right . Left

parseEmail :: Message EncStateWire MIME -> Either ParsingError Result
parseEmail content = do
    content ^.. entities & \es -> case es of
        [entity] ->
            view headerDate content & \mdate -> do
                date <- nothingLeft NoDate mdate
                let pe err =
                        ParsingError
                            ("Transfer decoding error: " ++ show err)
                            ""
                decoded <- left pe $ entity ^. transferDecoded'
                toTokens (decoded ^. body)
                    & \ts ->
                        Result
                            <$> nothingLeft
                                (WithDate date DescriptionMissingOrUnusable)
                                (findDescription ts)
                            <*> pure date
                            <*> nothingLeft
                                (WithDate date LinkMissing)
                                (listToMaybe (findLinks ts))
                            <*> pure (findOutcome ts)
        __ ->
            Left
                $ ParsingError ("Expected one entity, got " ++ show (length es)) ""

findOutcome :: [Token] -> Outcome
findOutcome tokens = do
    let
        findFirst :: (T.Text -> Maybe a) -> Maybe a
        findFirst f = asum $ map f contentTexts

        failIfNonZero 0 = OutcomeSuccess
        failIfNonZero _ = OutcomeFailure

    case (findFirst findNew, findFirst findOngoing) of
        (Just new, Just ongoing) -> failIfNonZero (new + ongoing)
        (Nothing, Just ongoing) -> failIfNonZero ongoing
        (_, Nothing) -> OutcomeUnknown
  where
    findNew :: T.Text -> Maybe Word
    findNew "No findings introduced this run." = pure 0
    findNew t = case T.breakOn " " t of
        (n, txt)
            | "new" `T.isInfixOf` txt -> readMaybe $ T.unpack n
            | otherwise -> Nothing

    findOngoing :: T.Text -> Maybe Word
    findOngoing t = case T.breakOn " " t of
        (n, txt) | "ongoing" `T.isInfixOf` txt -> readMaybe $ T.unpack n
        _ -> Nothing

    contentTexts = mapMaybe maybeGetContentText tokens

    maybeGetContentText = \case
        ContentText s -> Just s
        _ -> Nothing

findDescription :: [Token] -> Maybe TestRun
findDescription = asum . fmap f
  where
    f (ContentText d)
        | " Description:" `T.isPrefixOf` d = do
            json <-
                BL.fromStrict . T.encodeUtf8 . T.strip
                    <$> T.stripPrefix " Description:" d
            case parseCanonicalJSON json of
                Left _ -> Nothing
                Right value -> testRun <$> fromJSON value <|> fromJSON value
    f _ = Nothing

findLinks :: [Token] -> [T.Text]
findLinks = mapMaybe f
  where
    f (TagOpen "a" [Attr "href" l]) = Just l
    f _ = Nothing

utcTimeToClockTime :: UTCTime -> ClockTime
utcTimeToClockTime utc =
    let
        (seconds, picos) = properFraction $ utcTimeToPOSIXSeconds utc
    in
        TOD seconds $ fromIntegral $ fromEnum picos

clockTimeToUTCTime :: ClockTime -> UTCTime
clockTimeToUTCTime ct =
    let TOD sec pico = ct
        posix =
            secondsToNominalDiffTime (fromIntegral sec)
                + toEnum (fromIntegral pico)
    in  posixSecondsToUTCTime posix
