module Lib ( someFunc ) where
import Data.Aeson
import Data.Text
import qualified Data.ByteString.Lazy as BS
import GHC.Generics
import HueAPI

someFunc :: IO ()
someFunc = do
  putStrLn "someFunc"
  config <- readConfig
  case config of
    Just (Config port) -> lightsOff port "1" >> putStrLn "done"
    Nothing -> putStrLn "no config"

readConfig :: IO (Maybe Config)
readConfig = decode <$> BS.readFile "config.json"

data Config =
  Config {
    bridgePort :: String
  } deriving (Show, Generic)

instance FromJSON Config
