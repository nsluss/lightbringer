module HueAPI where
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Map
import Data.Monoid
import Data.Text
import GHC.Generics
import Network.HTTP.Client hiding (Proxy)
import Servant
import Servant.Client

api :: Proxy LightsAPI
api = Proxy

stateChange :: String -> Text -> StateChange -> IO (Either ServantError Text)
stateChange bridgeIp lightId newState = do
  manager' <- newManager defaultManagerSettings
  runClientM (lightState (Username "foTHIvt54544Wa9nf5YiSKMSVi6EUBviMAKl1ong") lightId  newState) (ClientEnv manager' (BaseUrl Http bridgeIp 80 ""))

lightsOff :: String -> Text -> IO (Either ServantError Text)
lightsOff ip lightId = stateChange ip lightId $ On False <> Done

lightsOn :: String -> Text -> IO (Either ServantError Text)
lightsOn ip lightId = stateChange ip lightId $ On True <> Done

lights :: Username -> ClientM (Map Int Light)
lightState :: Username -> Text -> StateChange -> ClientM Text
(lights :<|> lightState) = client api

type HueAPI = "api" :> Capture "username" Username

type LightsAPI =  "api" :> Capture "username" Username :> "lights" :> Get '[JSON] (Map Int Light)
            :<|> "api" :> Capture "username" Username  :> "lights" :> Capture "id" Text :> "state" :> ReqBody '[JSON] StateChange :> Put '[JSON] Text


newtype Username = Username { username :: Text } deriving (Generic, Show)
instance ToHttpApiData Username where
  toUrlPiece u = username u

data Light =
  Light {
    state :: LightState
  , light_type :: Text
  , name :: Text
  , modelid :: Text
  , swversion :: Text
  } deriving (Show, Generic)

instance FromJSON Light

data LightState =
  LightState {
    on :: Bool
  , bri :: Int
  , hue :: Int
  , sat :: Int
  , xy :: (Double, Double)
  , ct :: Int
  , alert :: Text
  , effect :: Text
  , colormode :: Text
  , reachable :: Bool
  } deriving (Show, Generic)

instance FromJSON LightState

data StateChange where
  Done :: StateChange
  More :: StateChange -> StateChange -> StateChange
  On  :: Bool -> StateChange
  Bri :: Int -> StateChange
  Hue :: Int -> StateChange
  Sat :: Int -> StateChange
  Xy  :: (Double, Double) -> StateChange
  Ct  :: Int -> StateChange
  Alert :: Text -> StateChange
  Effect :: Text -> StateChange
  TransitionTime :: Int -> StateChange
  Bri_inc :: Int -> StateChange
  Sat_inc :: Int -> StateChange
  Hue_inc :: Int -> StateChange
  Ct_inc :: Int -> StateChange
  Xy_inc :: (Double, Double) -> StateChange
  deriving (Show, Generic)

instance Monoid StateChange where
  mempty = Done
  mappend = More

instance ToJSON StateChange where
  toJSON sc = object $ stateToList sc
    where
      stateToList :: StateChange -> [Pair]
      stateToList Done = []
      stateToList (More x y) = stateToList x ++ stateToList y
      stateToList (On b) = [ "on" .= b ]
      stateToList (Bri i) = [ "bri" .= i ]
      stateToList (Hue i) = [ "hue" .= i ]
      stateToList (Sat i) = [ "sat" .= i ]
      stateToList (Xy (d1, d2)) = [ "xy" .= [ d1, d2 ] ]
      stateToList (Ct i) = [ "ct" .= i ]
      stateToList (Alert t) = [ "alert" .= t ]
      stateToList (Effect t) = [ "effect" .= t ]
      stateToList (TransitionTime i) = [ "transitiontime" .= i ]
      stateToList (Bri_inc i) = [ "bri_inc" .= i ]
      stateToList (Sat_inc i) = [ "sat_inc" .= i ]
      stateToList (Hue_inc i) = [ "hue_inc" .= i ]
      stateToList (Ct_inc i) =  [ "ct_inc" .= i ]
      stateToList (Xy_inc (d1, d2)) = [ "xy_inc" .= [d1, d2] ]




