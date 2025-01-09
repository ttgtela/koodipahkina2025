module HttpRequest (sendCreateRequest, sendActionRequest, decodeJsonResponse) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS ( tlsManagerSettings)
import Network.HTTP.Types.Header (hAuthorization, hContentType,HeaderName(..))
import Network.HTTP.Types.Method (methodPost)
import Data.ByteString.Char8 (pack)
import Text.JSON (decode, encode, Result(..), JSON)
import qualified Data.ByteString.Lazy.Char8 as L8
import GameTypes (TakeCardRequest(..), JSON(..))

baseUrl :: String
baseUrl = "https://koodipahkina.monad.fi/api/game"

-- | Sends a POST request to the api with the specified
-- headers and body.
sendPostRequest :: String -> [(HeaderName, String)] -> Maybe L8.ByteString -> IO (Either String L8.ByteString)
sendPostRequest url headers body = do
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest url
    let request = initialRequest
            { method = methodPost
            , requestHeaders = map (\(k, v) -> (k, pack v)) headers
            , requestBody = maybe mempty RequestBodyLBS body
            }
    response <- httpLbs request manager
    return $ Right (responseBody response)

-- | Sends a request to create a new game.
sendCreateRequest :: String -> IO (Either String L8.ByteString)
sendCreateRequest token =
    sendPostRequest baseUrl [(hAuthorization, "Bearer " ++ token)] Nothing

createTakeCardRequestBody :: TakeCardRequest -> L8.ByteString
createTakeCardRequestBody request = L8.pack (encode request)

-- | Sends an action request to the api containing either
-- True or False corresponding to if the player will take the
-- card or not.
sendActionRequest :: String -> String -> Bool -> IO (Either String L8.ByteString)
sendActionRequest token gameId val = do
    let url = baseUrl ++ "/" ++ gameId ++ "/action"
    let body = Just $ createTakeCardRequestBody (TakeCardRequest val)
    sendPostRequest url
        [ (hAuthorization, "Bearer " ++ token)
        , (hContentType, "application/json")
        ]
        body

decodeJsonResponse :: JSON a => L8.ByteString -> Either String a
decodeJsonResponse body =
    case decode (L8.unpack body) of
        Ok value -> Right value
        Error err -> Left $ "JSON decoding error: " ++ err