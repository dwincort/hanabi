
module Main where

import Control.Arrow (second)

import Data.Maybe (fromJust)

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import Data.Sequence (Sequence)
import qualified Data.Sequence as Seq

data State = State
  { deck            :: Deck
  , hands           :: Map PlayerId Hand
  , discards        :: [Card]
  , played_cards    :: Map CardColor CardNumber
  , number_of_clues :: Int
  , number_of_fails :: Int
  , player_order    :: (Seq PlayerId, Continue)
  } deriving (Eq, Show)

-- | Make a default start state using a sequence of players (in order by their
-- turn) and a (hopefully) shuffled deck
mkStartState :: Seq PlayerId -> Deck -> State
mkStartState players deck = State
  { deck            = deck
  , hands           = Map.fromList $ Seq.toList players `zip` repeat []
  , discards        = []
  , played_cards    = mempty
  , number_of_clues = 8
  , number_of_fails = 0
  , player_order    = (players, Loop)


data Continue = Loop | Fixed
  deriving (Eq, Show)

type PlayerId = Int

type Deck = [Card]
type Hand = [Card]

type Card = (CardColor, CardNumber)

type CardNumber = Int

data CardColor =
    Colored Color
  | Rainbow
  -- | Colorless
  -- ^ These would be cards that can never be identified by color clues
  deriving (Eq, Ord, Show)

data Color = Blue | White | Yellow | Green | Red
  deriving (Eq, Ord, Enum, Bounded, Show)

data Action =
    GiveClue PlayerId Clue
  | PlayCard Index
  | Discard  Index
  | TopDeck

type Index = Int

-- | The current score of the state, i.e. how many cards have been successfully played
score :: State -> Int
score State{..} = sum played_cards

-- | Returns True if the game state cannot accept more actions.
isGameOver :: State -> Bool
isGameOver State{..} =
     number_of_fails >= 3
  || (Seq.null $ fst player_order)

nextTurn :: State -> State
nextTurn state@State{..} =
  state { player_order = newPlayerOrder }
  where
    newPlayerOrder = case (player_order, deck) of
      ((Empty, _), _) -> mempty
      ((a :<| players, Loop), _:_) -> (players :|> a, Loop)
      ((a :<| players, Loop), [])  -> (players :|> a, Fixed)
      ((_ :<| players, Fixed), _)  -> (players, Fixed)

listRemove :: Int -> [a] -> (a, [a])
listRemove _ [] = []
listRemove 0 (x:xs) = (x, xs)
listRemove n (x:xs) = second (x:) (listRemove (n - 1) xs)

uncons :: [a] -> (Maybe a, [a])
uncons [] = (Nothing, [])
uncons (x:xs) = (Just x, xs)

getCurrentPlayer :: State -> PlayerId
getCurrentPlayer State{..} =
  fst player_order `Seq.index` 0

playCard :: Card -> State -> State
playCard card@(color, number) state@State{..} =
  state
  { discards = newDiscards
  , played_cards = newPlayedCards
  , number_of_fails = newNumberOfFails
  }
  where
    (newDiscards, newPlayedCards, newNumberOfFails) =
      case (Map.lookup color played_cards, number) of
        (Nothing, 1) ->
          (discards, Map.insert color 1 played_cards, number_of_fails)
        (Just n, n') | n + 1 == n' ->
          (discards, Map.insert color n' played_cards, number_of_fails)
        _ ->
          (card : discards, played_cards, number_of_fails + 1)

discardCard :: Card -> State -> State
discardCard card state@State{..} =
  state
  { discards = card : discards
  , number_of_clues = number_of_clues + 1
  }

currentPlayerPopAndDraw :: Index -> State -> (Card, State)
currentPlayerPopAndDraw i state@State{..} =
  state
    { deck = newDeck
    , hands = newHands'
    }
  where
    currentPlayer = getCurrentPlayer state
    (playedCard, newHands) = Map.alterF go currentPlayer hands
    go (Just hand) = second Just (listRemove i hand)
    go Nothing = error "Impossible"
    (topDeckCard, newDeck) = uncons deck
    newHands' = maybe newHands (\x -> Map.adjust (x:) currentPlayer newHands) topDeckCard

fail :: Either String a
fail = Left

act :: Action -> State -> Either String State
act _ state | isGameOver state = fail "Game is over."

act (GiveClue toWhom _) state@State{..} = do
  when (toWhom == getCurrentPlayer state)
    $ fail "Player cannot give a clue to self."
  when (number_of_clues <= 0)
    $ fail "Cannot give a clue when no clues remain."
  pure $ nextTurn $ state { number_of_clues = number_of_clues - 1 }

act (PlayCard i) state@State{..} = do
  when (i < 0 || i >= length (hands ! getCurrentPlayer state))
    $ fail "Card index out of bounds"
  pure $ nextTurn $ uncurry playCard $ currentPlayerPopAndDraw state

act (Discard i) state@State{..} = do
  when (i < 0 || i >= length (hands ! currentPlayer))
    $ fail "Card index out of bounds"
  when (number_of_clues >= 8)
    $ fail "Cannot discard when at max number of clues"
  pure $ nextTurn $ uncurry discardCard $ currentPlayerPopAndDraw state

act TopDeck state@State{..} = case deck of
  [] -> fail "Cannot topdeck when the deck is empty"
  topDeckCard : newDeck ->
    pure $ nextTurn $ playCard topDeckCard $ state { deck = newDeck }

standardDeck :: Deck
standardDeck =
  [(color, number) | color <- (Rainbow : fmap Colored [minBound .. maxBound])
                   , number <- [1,1,1,2,2,3,3,4,4,5] ]

simpleDeck :: Deck
simpleDeck =
  [(color, number) | color <- (fmap Colored [minBound .. maxBound])
                   , number <- [1,1,1,2,2,3,3,4,4,5] ]

shuffle :: Deck -> IO Deck
shuffle = undefined

-- | Takes a number of cards to deal out and deals to each player that many.
deal :: Int -> State -> State
deal = undefined

type Clue = Either Color CardNumber

clueTrue :: Clue -> Card -> Bool
clueTrue (Left c)  (Colored c', _) = c == c'
clueTrue (Left _)  (Rainbow, _)    = True
clueTrue (Right n) (_, n')         = n == n'

-- | queries (like which cards are red) and views (returns a list of bools)
clueView :: Clue -> Hand -> [Bool]
clueView = fmap . clueTrue
