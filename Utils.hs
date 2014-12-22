module Utils where

import Control.Exception
import Network.CGI (liftIO)
import Network.HTTP

getDirPath :: String -> String
getDirPath filePath =
    reverse .
    dropWhile ( \x -> x /= '/') .
    reverse $
    filePath

maybeListGet :: [a] -> Int -> Maybe a
maybeListGet list index
	| index < 0 = Nothing
	| index >= length list = Nothing
	| otherwise = Just $ list !! index

maybeHTTP :: String -> Int -> IO (Maybe String)
maybeHTTP url countDown = maybeHTTP' url countDown Nothing
    where
      maybeHTTP' :: String -> Int -> Maybe String -> IO (Maybe String)
      maybeHTTP' url 0 lastResult = do
          print $ "Failed : " ++ url
          return Nothing
      maybeHTTP' url countDown Nothing = do 
          result <- catch (((Network.HTTP.simpleHTTP $ getRequest url) >>= getResponseBody >>= (return . Just)) ) ( \(SomeException e) -> return Nothing)
          maybeHTTP' url ( countDown - 1 ) result
      maybeHTTP' url countDown lastResult = do
          print $ "Completed : " ++ url
          return lastResult
