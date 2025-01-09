module Strategy (makeMove) where

import qualified Data.List as List
import GameTypes (Player(..), Status(..), GameResponse(..), JSON(..))
import Data.Maybe (fromMaybe)
import Data.List (nub, (\\))

-- | A strategy for the card game that aims
-- for getting as much coins on the early stages
-- of the game as possible and in the later stages
-- only taking cards that are sequential to the already
-- taken cards or if the player is about to run out of coins
-- then it tries to take a card which value is lower than the
-- possible future cards. The strategy contains threshold values
-- for the different parameters that can be adjusted, but the
-- current values have been found to work quite well for the game.


 -- | Determines whether to make a move based on the game response.
makeMove :: GameResponse -> Bool
makeMove (GameResponse _ status) =
  case players status of
    (botPlayer : _) -> decideMove botPlayer status
    [] -> error "No players available in the game status."

-- | Determines if the game is early game or not
-- by counting the amount of cards the bot holds
isEarlyGame :: Player -> Bool
isEarlyGame botPlayer=if length (concat (cards botPlayer))<2
                      then True
                      else
                        False

-- | Decides whether to take the current card or place
-- a coin on it based on whether the game is still in its
-- early stages, the amount of coins the card has, the amount
-- of coins the player has and the estimated average value
-- of the possible remaining cards.

decideMove :: Player -> Status -> Bool
decideMove botPlayer status =
  let
    currentCard = fromMaybe (error "No current card in status.") (card status)
    coinsOnCard = moneyOnCard status
    botCoins = money botPlayer
    opponentPlayers = filter (\p -> name p /= name botPlayer) (players status)
    averageFutureValue = estimateAvgFutureValue status currentCard
    cardValue = evaluateCardValue currentCard (cards botPlayer)
    coinThreshold=5
    coinsOnCardThreshold=7
    earlyGameValueThreshold=25
  in
    if isEarlyGame botPlayer && botCoins/=0
    then
      if coinsOnCard >=coinsOnCardThreshold && (cardValue-coinsOnCard) <= earlyGameValueThreshold
      then
        True
      else
        False
    else
      if botCoins > coinThreshold && cardValue /= 0
      then False
      else (cardValue - coinsOnCard <= averageFutureValue) || botCoins == 0




-- | Evaluates the value of the current card based on the player's sequences.
evaluateCardValue :: Int -> [[Int]] -> Int
evaluateCardValue card sequences =
  if any (\seq -> card == head seq - 1 || card == last seq + 1) sequences
    then 0
    else card

-- | Returns the full range of cards in the game.
allCards :: [Int]
allCards = [3 .. 35]

-- | Determines the remaining possible cards in the game.
remainingPossibleCards :: Status -> Int -> [Int]
remainingPossibleCards status currentCard =
  let
    heldCards = concatMap (concat . cards) (players status)
    seenCards = nub (heldCards ++ [currentCard])
  in
    allCards \\ seenCards

-- | Calculates the average of the list of all possible future cards
avgOfRemainingPossibleCards :: [Int] -> Int -> Int -> Int
avgOfRemainingPossibleCards [] total numOfCards = total `div` numOfCards
avgOfRemainingPossibleCards (c:cards) total numOfCards = avgOfRemainingPossibleCards cards (total + c) numOfCards

-- | Estimates the average value of future cards in the game.
estimateAvgFutureValue :: Status -> Int -> Int
estimateAvgFutureValue status currentCard =
  let remainingCards = remainingPossibleCards status currentCard
  in avgOfRemainingPossibleCards remainingCards 0 (length remainingCards)