module WebApp where
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API.Raw
import Servant.Server
import Servant.Utils.StaticFiles


type WebApp = Raw

server :: Server WebApp
server = serveDirectoryWebApp "public"

webApp :: Proxy WebApp
webApp = Proxy

app :: Application
app = serve webApp server

runApp = run 8080 app
