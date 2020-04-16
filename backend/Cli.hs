{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Cli where

import Hanabi

import Control.Applicative ((<|>))

import           Data.List       (intercalate, (\\))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust, isNothing, maybe)
import           Data.Sequence   (pattern (:<|), pattern Empty, Seq)
import qualified Data.Sequence   as Seq
import qualified Data.Set        as Set

import qualified System.Console.ANSI      as ANSI
import qualified System.Console.Haskeline as Haskeline

import qualified Text.Parsec      as Parsec
import qualified Text.Parsec.Char as PC

-- Shows only information that is available to all players.
showCommonState :: State -> String
showCommonState State{..} = intercalate "\n" [
    showDeck deck,
    showPlayedCards played_cards,
    showCluesFails number_of_clues number_of_fails,
    showDiscards discards
    ]


-- Shows common information, plus information that is available to the given player.
showStateToPlayer :: PlayerId -> State -> String
showStateToPlayer p (viewState p -> PlayerStateView{..}) = intercalate "\n" [
    "You are player " ++ show p,
    "Deck has " ++ show deck ++ " cards",
    showPlayedCards played_cards,
    showCluesFails number_of_clues number_of_fails,
    showDiscards discards,
    showYourHand my_hand,
    showHands other_hands,
    showPlayerOrder player_order
    ]


showDeck :: Deck -> String
showDeck d = "Deck has " ++ (show $ length d) ++ " cards"

showPlayedCards :: Map CardColor CardNumber -> String
showPlayedCards played_cards
    | Map.null played_cards = "Board: (empty)"
    | otherwise = "Board: " ++ (intercalate " " $ map showCard $ Map.toList played_cards)

shortShowColor :: CardColor -> String
shortShowColor (Colored Blue)   = "B"
shortShowColor (Colored White)  = "W"
shortShowColor (Colored Yellow) = "Y"
shortShowColor (Colored Green)  = "G"
shortShowColor (Colored Red)    = "R"
shortShowColor Rainbow          = "Z"

-- Card as a 2 character string, like W1 (rainbow=Z)
showCard :: Card -> String
showCard (color, number) = shortShowColor color ++ show number

-- A list of short cards separated by spaces (W1 B1)
showCards :: [Card] -> String
showCards = intercalate " " . map showCard

showCluesFails :: Int -> Int -> String
showCluesFails c f = "There are " ++ show c ++ " clues and " ++ show f ++ " bombs remaining."

showDiscards :: [Card] -> String
showDiscards []       = "Discards: (none)"
showDiscards discards = "Discards: " ++ showCards discards

showPlayer :: PlayerId -> String
showPlayer p = "Next player is " ++ (show p) ++ "."

showPlayerOrder :: (Seq PlayerId, Continue) -> String
showPlayerOrder (Empty, Fixed) = "The game is over."
showPlayerOrder (_ :<| Empty, Fixed) = "This is the last turn."
showPlayerOrder (_ :<| np :<| nps, Fixed) = showPlayer np ++ " There are " ++ (show $ 2 + length nps) ++ " turns left."
showPlayerOrder (_ :<| np :<| _, Loop) = showPlayer np
showPlayerOrder (_, Loop) = error "oops"

showHand :: Hand -> String
showHand = showCards . map fst

showHands :: Map PlayerId Hand -> String
showHands hs = intercalate " " [ "Player " ++ (show k) ++ ": " ++ (showHand v)
                                 | (k,v) <- Map.toList hs ]

-- A cardview shows you what has been clued about that card.
-- If you know nothing, it appears as "--"
-- Otherwise we separately render all colors and the number:
-- x3 (3 but no color)
-- G3 (green 3)
-- GW3 (green white 3)
-- GWx (green white, no #)
showCardView :: CardView -> String
showCardView (colors, nums)
    | null colors && isNothing nums = "--"
    | null colors = "x" ++ shownNums
    | otherwise = shownColors ++ shownNums
    where
    shownColors = concat $ map (shortShowColor . Colored) $ Set.toList colors
    shownNums = maybe "x" show nums

showYourHandCard :: Int -> CardView -> String
showYourHandCard i cv = show i ++ ") " ++ showCardView cv

showYourHand :: HandView -> String
showYourHand h = "Your hand: " ++ (intercalate "   " (zipWith showYourHandCard [0..] h))

-- Describe the action in present tense.
showAction :: State -> State -> PlayerId -> Action -> String
showAction _ _ a (GiveClue p c) = "Player " ++ show a ++ " gives a clue to " ++ show p ++ " naming " ++ showClue c ++ "."
showAction s s' a (PlayCard _) = preamble ++ "\n" ++ showPlay s s'
    where
    preamble = "Player " ++ show a ++ " plays a card."
showAction s s' a (Discard _) = "Player " ++ show a ++ " discards a " ++ showCards (s' `diffDiscards` s)
showAction s s' a (TopDeck) = preamble ++ "\n" ++ showPlay s s'
    where
    preamble = "Player " ++ show a ++ " topdecks."

-- Describe what happened in a state where we tried to play a card.
showPlay :: State -> State -> String
showPlay s s' = case s' `diffDiscards` s of
        [] -> "Success! " ++ showCards (s' `diffPlays` s)
        ds -> "Failure: discarded " ++ showCards ds ++ " and ate a bomb!"

-- Describe the clue:
showClue :: Clue -> String
showClue (Left c)  = show c
showClue (Right n) = show n

-- Show history
showHistory :: [String] -> String
showHistory []    = ""
showHistory (a:_) = a

-- Running a game entails shuffling and dealing the deck, then running turns
-- until the game ends.
runGame :: IO ()
runGame = do
    deck <- shuffle standardDeck
    let s = deal 5 $ mkStartState (Seq.fromList [1,2]) deck
    let cs = CliState s []

    cs' <- doWhile (not . isGameOver . state) runTurn cs
    putStrLn "game over!"
    putStrLn $ "Score is " ++ show (score (state cs'))

-- While the condition is true on the state, run the function
-- within the monad to transform the state into a new state.
doWhile :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
doWhile cond fn initialState
    | not (cond initialState) = pure initialState
    | otherwise = fn initialState >>= doWhile cond fn

data CliState = CliState {
    state   :: State, -- game state
    history :: [String] -- actions
}

-- Running a turn entails showing the common state and waiting for them to press enter,
-- then prompting the current user for an action,
-- then parsing it, applying it to the state and returning the new state.
runTurn :: CliState -> IO CliState
runTurn cs = do
    ANSI.clearScreen
    putStrLn $ showHistory (history cs)
    putStrLn $ showCommonState (state cs)
    putStr $ "Give computer to player " ++ (show $ getCurrentPlayer $ state cs) ++ " and press enter..."
    _ <- getLine -- and throw it away

    ANSI.clearScreen
    let pn = getCurrentPlayer (state cs)
    putStrLn $ showHistory (history cs)
    putStrLn $ showStateToPlayer pn (state cs)

    putStr "Enter action: "
    Haskeline.runInputT Haskeline.defaultSettings (readEvalAction cs)

-- read action input from user
readAction :: Haskeline.InputT IO Action
readAction =
  snd <$> repeatOnFail getInput (Parsec.parse action "") onError
  where getInput  = fromJust <$> Haskeline.getInputLine "> "
        onError _ = Haskeline.outputStrLn "Invalid input."

readEvalAction :: CliState -> Haskeline.InputT IO CliState
readEvalAction cs = do
  let s = state cs
  (action, s') <- repeatOnFail readAction (flip act s) Haskeline.outputStrLn
  let actionDescription = showAction s s' (getCurrentPlayer s) action
  pure $ CliState s' (actionDescription : history cs)

actionSpec :: Parsec.Parsec String () Char
actionSpec = PC.oneOf "cpdt"

action :: Parsec.Parsec String () Action
action = do
    as <- actionSpec
    PC.spaces
    case as of
        'c' -> GiveClue <$> num <* PC.spaces <*> clue
        'p' -> PlayCard <$> num
        'd' -> Discard <$> num
        't' -> pure TopDeck
        _   -> error "Impossible"

-- one char, bwygr, indicating a Color
color :: Parsec.Parsec String () Color
color = do
    c <- PC.oneOf "bwygr"
    pure $ case c of
        'b' -> Blue
        'w' -> White
        'y' -> Yellow
        'g' -> Green
        'r' -> Red
        _   -> error "Impossible"

num :: Parsec.Parsec String () Int
num = read <$> Parsec.many1 PC.digit

clue :: Parsec.Parsec String () Clue
clue = Left <$> color
    <|> Right <$> num

class Diff a where
    diff :: a -> a -> a

instance Diff Int where
    diff = (-)

instance (Ord k, Num v, Eq v) => Diff (Map k v) where
    -- remove keys that become zero
    diff = Map.differenceWith (\a b -> case a-b of 0 -> Nothing; c -> Just c)

instance Eq a => Diff [a] where
    -- list difference
    diff = (\\)

stateDiff :: Diff a => (State -> a) -> State -> State -> a
stateDiff f s' s = (f s') `diff` (f s)

diffDiscards :: State -> State -> [Card]
diffDiscards = stateDiff discards

diffPlays :: State -> State -> [Card]
diffPlays s' s = Map.toList $ stateDiff played_cards s' s

{-
d <- shuffle simpleDeck
s = mkStartState (Seq.fromList [1,2]) d
s' = deal 5 s
a = TopDeck
Right s'' = act a s'
putStrLn $ showStateToPlayer 1 s'

showPlayerOrder (Seq.fromList [], Fixed)
showPlayerOrder (Seq.fromList [2,3,1], Fixed)
showPlayerOrder (Seq.fromList [2,3,1], Loop)

runTurn s'

 -}
