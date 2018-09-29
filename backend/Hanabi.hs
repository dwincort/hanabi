{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Hanabi where

import Control.Arrow (second, (&&&), (***))
import Control.Monad (when)

import           Data.Foldable
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Data.Sequence   (pattern (:<|), pattern (:|>), pattern Empty, Seq)
import qualified Data.Sequence   as Seq
import           Data.Set        (Set)
import qualified Data.Set        as Set

import qualified System.Random as Rand

-- | The state of the game at any given point
data State = State
  { deck            :: Deck
  -- ^ The draw deck
  , hands           :: Map PlayerId Hand
  -- ^ The players' hands
  , discards        :: [Card]
  -- ^ The discard pile
  , played_cards    :: Map CardColor CardNumber
  -- ^ The successfully played cards
  , number_of_clues :: Int
  -- ^ The number of clues available to give
  , number_of_fails :: Int
  -- ^ The number of failures that have happened so far
  , player_order    :: (Seq PlayerId, Continue)
  -- ^ The current player order.
  -- The head of the sequence represents the current player, with the rest of
  -- the players in sequence after that.  The Continue represents how to proceed
  -- through the sequence: either putting the current player on the end when we
  -- go to the next turn or not.
  } deriving (Eq, Show)

-- | Make a default start state using a sequence of players (in order by their
-- turn) and a (hopefully) shuffled deck
mkStartState :: Seq PlayerId -> Deck -> State
mkStartState players deck = State
  { deck            = deck
  , hands           = Map.fromList $ toList players `zip` repeat []
  , discards        = []
  , played_cards    = mempty
  , number_of_clues = 8
  , number_of_fails = 3
  , player_order    = (players, Loop)
  }

-- | This is used for player order, and it's perhaps somewhat misnamed.
-- Loop means that we will keep looping the player order sequence.
-- Fixed means that we are near the end of the game, and players should not be
-- appended to the end of the order sequence after their turns.
data Continue = Loop | Fixed
  deriving (Eq, Show)

type PlayerId = Int

type Deck = [Card]
type Hand = [(Card, CardView)]

type Card = (CardColor, CardNumber)

type CardView = (Set Color, Maybe CardNumber)

type CardNumber = Int

-- | A card color is not just a color, because cards can also be rainbow.
data CardColor =
    Colored Color
  | Rainbow
  -- | Colorless
  -- ^ These would be cards that can never be identified by color clues
  deriving (Eq, Ord, Show)

data Color = Blue | White | Yellow | Green | Red
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | There are four currently defined actions.
data Action =
    GiveClue PlayerId Clue
  -- ^ @GiveClue i c@ is the action to give the clue @c@ to player @i@.
  | PlayCard Index
  -- ^ @PlayCard i@ is the action for the current player to play their ith card.
  | Discard  Index
  -- ^ @Discard i@ is the action for the current player to discard their ith card.
  | TopDeck
  -- ^ @TopDeck@ is the action to play the top card of the draw deck.
  deriving (Eq, Show)

type Index = Int


-- * State query functions

-- | The current score of the state, i.e. how many cards have been successfully played
score :: State -> Int
score State{..} = sum played_cards

-- | Returns True if the game state cannot accept more actions.
isGameOver :: State -> Bool
isGameOver State{..} =
     number_of_fails <= 0
  || (Seq.null $ fst player_order)

-- | Gets the current player from the state.
getCurrentPlayer :: State -> PlayerId
getCurrentPlayer State{..} =
  fst player_order `Seq.index` 0


-- * Helper functions

-- | A helper function to index and remove the ith element from the list.
-- If i is out of bounds, this function will error.
listRemove :: Int -> [a] -> (a, [a])
listRemove _ []     = error "listRemove failure"
listRemove 0 (x:xs) = (x, xs)
listRemove n (x:xs) = second (x:) (listRemove (n - 1) xs)

-- | This is like the Data.List.uncons function except that it puts the Maybe
-- wrapper only around the unconsed element and not around the tail.  If an
-- empty list is passed, the tail remains the empty list.
uncons :: [a] -> (Maybe a, [a])
uncons []     = (Nothing, [])
uncons (x:xs) = (Just x, xs)


-- * Our exception monad

-- | Failure in the 'Either String' monad.
failWith :: String -> Either String a
failWith = Left

-- | Handle an error, and try again.
repeatOnFail :: Monad m
  => m a               -- ^ A getter for input
  -> (a -> Either b c) -- ^ A computation that may fail
  -> (b -> m x)        -- ^ Do something monadic with the failure value before repeating
  -> m (a,c)           -- The successful input and the result
repeatOnFail ma f handle = ma >>= \a -> case f a of
  Left b  -> repeatOnFail (handle b >> ma) f handle
  Right c -> pure (a, c)

-- * Clues

type Clue = Either Color CardNumber

-- | Update the view within a hand given a clue.
updateHandInfoFromClue :: Clue -> Hand -> Hand
updateHandInfoFromClue clue hand = fmap (fst &&& uncurry (go clue)) hand
  where
    -- go takes a clue, a card, and a view
    -- returns the updated view
    go (Left c) (Colored c', _) (colors, num) | c == c' =
      (Set.insert c colors, num)
    go (Left c)  (Rainbow, _) (colors, num) =
      (Set.insert c colors, num)
    go (Right n) (_, n') (colors, _) | n == n' =
      (colors, Just n)
    go _ _ view = view

-- | A card view of no information.
noCardInfo :: CardView
noCardInfo = (mempty, Nothing)


-- * State update functions

-- | Ends the current player's turn.
--
-- Assumes the game is not over.
-- Affects player_order.
nextTurn :: State -> State
nextTurn state@State{..} =
  state { player_order = newPlayerOrder }
  where
    newPlayerOrder = case (player_order, deck) of
      ((Empty, _), _)              -> (mempty, Fixed)
      ((a :<| players, Loop), _:_) -> (players :|> a, Loop)
      ((a :<| players, Loop), [])  -> (players :|> a, Fixed)
      ((_ :<| players, Fixed), _)  -> (players, Fixed)

-- | Given a card, update the state with an attempt at playing that card.
--
-- Affects discards, played_cards, and number_of_fails.
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
          (card : discards, played_cards, number_of_fails - 1)

-- | Given a card, update the state by discarding the card
--
-- Assumes the card index is in bounds and the number of clues is below max.
-- Affects discards and number_of_clues.
discardCard :: Card -> State -> State
discardCard card state@State{..} =
  state
  { discards = card : discards
  , number_of_clues = number_of_clues + 1
  }

-- | Given an index into the current player's hand, update the state by
-- removing that card from the player's hand and having them draw a new card
-- (or leaving their hand not quite full if the deck is empty).
-- Also returns the card that was removed from the player's hand.
--
-- Assumes the card index is in bounds.
-- Affects deck and hands.
currentPlayerPopAndDraw :: Index -> State -> (Card, State)
currentPlayerPopAndDraw i state@State{..} =
  (poppedCard, state
    { deck = newDeck
    , hands = newHands'
    })
  where
    currentPlayer = getCurrentPlayer state
    (poppedCard, newHands) = Map.alterF go currentPlayer hands
    go (Just hand) = (fst *** Just) (listRemove i hand)
    go Nothing     = error "Impossible"
    (topDeckCard, newDeck) = uncons deck
    newHands' = maybe newHands addCard topDeckCard
    addCard card = Map.adjust ((card,noCardInfo):) currentPlayer newHands

-- | Give a clue to the given player, updating their view of their cards with the
-- new information.
--
-- Assumes there are available clues and that the given player is not the current player.
-- Affects hands and number_of_clues.
giveClue :: PlayerId -> Clue -> State -> State
giveClue receiver clue state@State{..} =
  state
  { hands = Map.adjust (updateHandInfoFromClue clue) receiver hands
  , number_of_clues = number_of_clues - 1
  }

-- | This is the main state update function.
-- Given an action and a state, return the updated state.
-- This is in an exception monad ('Either String') in case the action or state
-- make no sense together.
act :: Action -> State -> Either String State
act _ state | isGameOver state = failWith "Game is over."

act (GiveClue toWhom clue) state@State{..} = do
  when (toWhom == getCurrentPlayer state)
    $ failWith "Player cannot give a clue to self."
  when (number_of_clues <= 0)
    $ failWith "Cannot give a clue when no clues remain."
  pure $ nextTurn $ giveClue toWhom clue state

act (PlayCard i) state@State{..} = do
  when (i < 0 || i >= length (hands ! getCurrentPlayer state))
    $ failWith "Card index out of bounds"
  pure $ nextTurn $ uncurry playCard $ currentPlayerPopAndDraw i state

act (Discard i) state@State{..} = do
  when (i < 0 || i >= length (hands ! getCurrentPlayer state))
    $ failWith "Card index out of bounds"
  when (number_of_clues >= 8)
    $ failWith "Cannot discard when at max number of clues"
  pure $ nextTurn $ uncurry discardCard $ currentPlayerPopAndDraw i state

act TopDeck state@State{..} = case deck of
  [] -> failWith "Cannot topdeck when the deck is empty"
  topDeckCard : newDeck ->
    pure $ nextTurn $ playCard topDeckCard $ state { deck = newDeck }


-- * Deck definitions and functions

-- | A standard 60 card Hanabi deck.
standardDeck :: Deck
standardDeck =
  [(color, number) | color <- (Rainbow : fmap Colored [minBound .. maxBound])
                   , number <- [1,1,1,2,2,3,3,4,4,5] ]

-- | A simple 50 card Hanabi deck with no rainbow cards
simpleDeck :: Deck
simpleDeck =
  [(color, number) | color <- (fmap Colored [minBound .. maxBound])
                   , number <- [1,1,1,2,2,3,3,4,4,5] ]

-- | Randomize the order of the cards in the deck.
shuffle :: [a] -> IO [a]
shuffle xs = go (length xs) xs [] where
  go 0 _ result = return result
  go n lst result = do
    k <- Rand.randomRIO (0,n-1)
    let (x, xs) = listRemove k lst
    go (n-1) xs (x : result)

-- | Takes a number of cards to deal out and deals to each player that many.
-- Assumes that each player has a hand.
deal :: Int -> State -> State
deal n state@State{..} =
  state
  { deck = deck'
  , hands = hands'
  }
  where
    (deck', hands') = deal' n (deck, hands)
    deal' 0 = id
    deal' n = deal' (n-1) . uncurry (Map.mapAccum go)
    go [] h     = ([], h)
    go (x:xs) h = (xs, (x, noCardInfo):h)


-- Some stuff for testing
--
-- mkMyState = do
--   deck <- shuffle standardDeck
--   pure $ deal 5 $ mkStartState (Seq.fromList [1,2]) deck
