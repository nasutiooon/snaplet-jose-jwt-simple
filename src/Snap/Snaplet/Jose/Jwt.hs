module Snap.Snaplet.Jose.Jwt
  ( HasJwtEncoder (..)
  , JwtEncoder(..)
  , jwtEncoderInit
  , withJwtEncoder
  , encode
  , decode
  , assocTime
  ) where

import           Control.Monad.Trans           (MonadIO, liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import           Crypto.Random                 (MonadRandom)
import qualified Data.Aeson                    as A
import qualified Data.ByteString               as B
import qualified Data.Configurator             as C
import qualified Data.Configurator.Types       as C
import           Jose.Jwk                      (Jwk)
import           Jose.Jwt                      (Jwt, JwtContent, JwtEncoding,
                                                JwtError, Payload, JwtClaims(..), IntDate(..))
import qualified Jose.Jwt                      as JT
import           Paths_snaplet_jose_jwt_simple (getDataDir)
import           Snap.Snaplet                  (SnapletInit,
                                                getSnapletUserConfig,
                                                makeSnaplet)

class (MonadRandom m, MonadIO m) => HasJwtEncoder m where
  getJwtEncoder :: m JwtEncoder

data JwtEncoder = JwtEncoder { duration :: Maybe Integer
                             , keys     :: [Jwk] }

newtype Keystore = Keystore [Jwk]  deriving (Show)

instance A.FromJSON Keystore where
  parseJSON = A.withObject "" $ \o ->
    Keystore <$> o A..: "keys"

jwtEncoderInit :: SnapletInit b JwtEncoder
jwtEncoderInit =
  makeSnaplet "jose-jwt-simple" desc dataDir $ do
  config <- getSnapletUserConfig
  duration <- liftIO $ C.lookup config "duration"
  keys' <- liftIO $ getKeys config
  return $ JwtEncoder duration keys'
  where
    desc = "jwt encoder snaplet"
    dataDir = Just $ (<>"/resources/jwt") <$> getDataDir

withJwtEncoder :: HasJwtEncoder m => (JwtEncoder -> m a) -> m a
withJwtEncoder = (=<< getJwtEncoder)

encode
  :: HasJwtEncoder m
  => JwtEncoding
  -> Payload
  -> m (Either JwtError Jwt)
encode encoding payload =
  withJwtEncoder $ \encoder -> JT.encode (keys encoder) encoding payload

decode
  :: HasJwtEncoder m
  => Maybe JwtEncoding
  -> B.ByteString
  -> m (Either JwtError JwtContent)
decode encoding token =
  withJwtEncoder $ \encoder -> JT.decode (keys encoder) encoding token

assocTime :: HasJwtEncoder m => JwtClaims -> m JwtClaims
assocTime claims =  withJwtEncoder f
  where
    f encoder = do
      now <- liftIO getPOSIXTime
      let iat = Just $ IntDate now
          expAt = IntDate . (now +) . fromInteger <$> duration encoder
      return $ claims { jwtIat = iat
                      , jwtExp = expAt }

getKeys :: C.Config -> IO [Jwk]
getKeys config = do
  path <- C.require config "path"
  mKeystore <- A.decodeFileStrict path
  case mKeystore of
    Nothing               -> fail $ "Unable to read json file at: " <> path
    Just (Keystore keys') -> return keys'
