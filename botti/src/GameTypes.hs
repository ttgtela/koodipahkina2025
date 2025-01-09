module GameTypes (Player(..), Status(..), GameResponse(..), JSON(..), TakeCardRequest(..)) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Text.JSON

data Player = Player
  { name :: String
  , money :: Int
  , cards :: [[Int]]
  } deriving (Show,Eq,Ord)

data Status = Status
  { card :: Maybe Int
  , moneyOnCard :: Int
  , players :: [Player]
  , cardsLeft :: Int
  , finished :: Bool
  } deriving (Show,Eq,Ord)

data GameResponse = GameResponse
  {
  gameId :: Maybe String,
  status :: Status
  } deriving (Show,Eq,Ord)

instance JSON GameResponse where
  readJSON (JSObject obj) = do
      status <- valFromObj "status" obj
      let gameId = case lookup "gameId" (fromJSObject obj) of
            Just v  -> case readJSON v of
                          Ok x  -> Just x
                          Error _ -> Nothing
            Nothing -> Nothing
      return GameResponse { gameId = gameId, status = status }

  readJSON _ = fail "Expected Game JSON object"

  showJSON gameResponse = JSObject $ toJSObject
      [ ("gameId", maybe (JSNull) showJSON $ gameId gameResponse)
      , ("status", showJSON $ status gameResponse)
      ]

instance JSON Player where
  readJSON (JSObject obj) = do
    name <- valFromObj "name" obj
    money <- valFromObj "money" obj
    cards <- valFromObj "cards" obj
    return Player { name = name, money = money, cards = cards }

  readJSON _ = fail "Expected Player JSON object"

  showJSON player = JSObject $ toJSObject
    [ ("name", showJSON $ name player)
    , ("money", showJSON $ money player)
    , ("cards", showJSON $ cards player)
    ]

instance JSON Status where
  readJSON (JSObject obj) = do
    let card = case lookup "card" (fromJSObject obj) of
                Just v  -> case readJSON v of
                           Ok x  -> Just x
                           Error _ -> Nothing
                Nothing -> Nothing
    moneyOnCard <- valFromObj "money" obj
    players <- valFromObj "players" obj
    cardsLeft <- valFromObj "cardsLeft" obj
    finished <- valFromObj "finished" obj
    return Status { card = card, moneyOnCard = moneyOnCard, players = players, cardsLeft = cardsLeft, finished = finished }

  readJSON _ = fail "Expected Status JSON object"

  showJSON status = JSObject $ toJSObject
    [ ("card", showJSON $ card status)
    , ("money", showJSON $ moneyOnCard status)
    , ("players", showJSON $ players status)
    , ("cardsLeft", showJSON $ cardsLeft status)
    , ("finished", showJSON $ finished status)
    ]

data TakeCardRequest = TakeCardRequest
  { takeCard :: Bool
  } deriving (Show)


instance JSON TakeCardRequest where
  readJSON (JSObject obj) = do
    takeCard <- valFromObj "takeCard" obj
    return TakeCardRequest { takeCard = takeCard }

  readJSON _ = fail "Expected TakeCardRequest JSON object"

  showJSON request = JSObject $ toJSObject
    [ ("takeCard", showJSON $ takeCard request)
    ]
