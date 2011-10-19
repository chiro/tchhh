module HTC where
import Network.Browser
import Network.HTTP

main = do
  rsp <- browse $ do setAllowRedirects True
                     request $ getRequest "http://google.com/"
  putStrLn $ show $ fmap (take 100) (getResponseBody rsp)