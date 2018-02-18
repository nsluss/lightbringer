module Lib ( someFunc ) where
import Data.Aeson
import Data.Text
import qualified Data.ByteString.Lazy as BS
import GHC.Generics
import HueAPI

someFunc :: IO ()
someFunc = putStrLn "hello"

turnALightOff :: IO ()
turnALightOff = do
  putStrLn "someFunc"
  config <- readConfig
  case config of
    Just (Config ip) -> lightsOff ip "1" >> putStrLn "done"
    Nothing -> putStrLn "no config"

readConfig :: IO (Maybe Config)
readConfig = decode <$> BS.readFile "config.json"

data Config =
  Config {
    bridgeIp :: String
  } deriving (Show, Generic)

instance FromJSON Config
