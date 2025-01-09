module Main (main) where

import HttpRequest (sendCreateRequest, sendActionRequest, decodeJsonResponse)
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Monad (foldM)
import GameTypes (Player(..), Status(..), GameResponse(..), JSON(..))
import Strategy(makeMove)

create :: String -> IO Bool
create token = do
    response <- sendCreateRequest token
    case response of
        Left err -> handleRequestFailure err
        Right body -> handleCreateSuccess body token

handleRequestFailure :: String -> IO Bool
handleRequestFailure err = do
    putStrLn $ "Request failed: " ++ err
    return False

handleCreateSuccess :: L8.ByteString -> String -> IO Bool
handleCreateSuccess body token = do
    let decodedResponse = decodeJsonResponse body :: Either String GameResponse
    case decodedResponse of
        Left decodeErr -> handleDecodeFailure decodeErr
        Right myResponse -> handleDecodedCreateResponse myResponse token

handleDecodeFailure :: String -> IO Bool
handleDecodeFailure decodeErr = do
    putStrLn decodeErr
    return False

handleDecodedCreateResponse :: GameResponse -> String -> IO Bool
handleDecodedCreateResponse myResponse token = do
    putStrLn $ "Decoded response: " ++ show myResponse
    case gameId myResponse of
        Nothing -> return False
        Just gameId -> playWithGameState gameId token myResponse

playWithGameState :: String -> String -> GameResponse -> IO Bool
playWithGameState gameId token response = do
    let action=makeMove response
    play gameId token action

play :: String -> String -> Bool -> IO Bool
play gameId token val = do
    response <- sendActionRequest token gameId val
    case response of
        Left err -> handleRequestFailure err
        Right body -> handlePlaySuccess body gameId token

handlePlaySuccess :: L8.ByteString -> String -> String -> IO Bool
handlePlaySuccess body gameId token = do
    let decodedResponse = decodeJsonResponse body :: Either String GameResponse
    case decodedResponse of
        Left decodeErr -> handleDecodeFailure decodeErr
        Right myResponse -> handleDecodedPlayResponse myResponse gameId token


handleDecodedPlayResponse :: GameResponse -> String -> String -> IO Bool
handleDecodedPlayResponse myResponse gameId token = do
    putStrLn $ "Decoded response: " ++ show myResponse
    if finished (status myResponse)
        then return True
        else playWithGameState gameId token myResponse


playHundredTimes :: String -> Int -> IO ()
playHundredTimes _ 0 = return ()
playHundredTimes token counter = do
    success <- create token
    if success
        then playHundredTimes token (counter - 1)
        else do
            putStrLn "An error ocurred, playing has stopped."

main :: IO ()
main = do
    putStrLn "Playing one hundred times in a row"
    -- Input token used for API authentication here
    let token = ""
    playHundredTimes token 100