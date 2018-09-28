{-# LANGUAGE RecordWildCards, PatternSynonyms #-}

module Cli where

import Hanabi

import Control.Applicative ((<|>))

import Data.Map.Strict (Map, (!))
import Data.Maybe (fromJust, maybe, isNothing)
import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import qualified Data.Set as Set
import Data.Sequence (Seq, pattern (:<|), pattern (:|>), pattern Empty)
import qualified Data.Sequence as Seq

import qualified System.Console.ANSI as ANSI
import qualified System.Console.Haskeline as Haskeline

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Char as PC

-- Shows only information that is available to all players.
showCommonState :: State -> String
showCommonState state = intercalate "\n" [
    showDeck $ deck state,
    showPlayedCards $ played_cards state,
    showCluesFails (number_of_clues state) (number_of_fails state),
    showDiscards $ discards state
    ]


-- Shows common information, plus information that is available to the given player.
showStateToPlayer :: PlayerId -> State -> String
showStateToPlayer p state = intercalate "\n" [
    "You are player " ++ (show p),
    showDeck $ deck state,
    showPlayedCards $ played_cards state,
    showCluesFails (number_of_clues state) (number_of_fails state),
    showDiscards $ discards state,
    showYourHand  $ hands state ! p,
    showHands $ Map.delete p (hands state),
    showPlayerOrder $ player_order state
    ]


showDeck :: Deck -> String
showDeck d = "Deck has " ++ (show $ length d) ++ " cards"

showPlayedCards :: Map CardColor CardNumber -> String
showPlayedCards played_cards 
    | Map.null played_cards = "Board: (empty)"
    | otherwise = "Board: " ++ (intercalate " " $ map showCard $ Map.toList played_cards)

shortShowColor :: CardColor -> String
shortShowColor (Colored Blue) = "B"
shortShowColor (Colored White) = "W"
shortShowColor (Colored Yellow) = "Y"
shortShowColor (Colored Green) = "G"
shortShowColor (Colored Red) = "R"
shortShowColor Rainbow = "Z"

-- Card as a 2 character string, like W1 (rainbow=Z)
showCard :: Card -> String
showCard (color, number) = shortShowColor color ++ show number

-- A list of short cards separated by spaces (W1 B1)
showCards = intercalate " " . map showCard

showCluesFails :: Int -> Int -> String
showCluesFails c f = "There are " ++ show c ++ " clues and " ++ show f ++ " bombs remaining."

showDiscards :: [Card] -> String
showDiscards [] = "Discards: (none)"
showDiscards discards = "Discards: " ++ showCards discards

showPlayer :: PlayerId -> String
showPlayer p = "Next player is " ++ (show p) ++ "."

showPlayerOrder :: (Seq PlayerId, Continue) -> String
showPlayerOrder (Empty, Fixed) = "The game is over."
showPlayerOrder (_ :<| Empty, Fixed) = "This is the last turn."
showPlayerOrder (_ :<| np :<| nps, Fixed) = showPlayer np ++ " There are " ++ (show $ 2 + length nps) ++ " turns left."
showPlayerOrder (Empty, Loop) = error "oops"
showPlayerOrder (_ :<| np :<| _, Loop) = showPlayer np

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


showYourHand :: Hand -> String
showYourHand h = "Your hand: " ++ (intercalate " " (map (showCardView . snd) h))

-- Running a game entails shuffling and dealing the deck, then running turns
-- until the game ends.
runGame :: IO ()
runGame = do
    deck <- shuffle standardDeck
    let state = deal 5 $ mkStartState (Seq.fromList [1,2]) deck

    doWhile (not . isGameOver) runTurn state
    putStrLn "game over!"

-- While the condition is true on the state, run the function
-- within the monad to transform the state into a new state.
doWhile :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
doWhile cond fn initialState 
    | not (cond initialState) = pure initialState
    | otherwise = fn initialState >>= doWhile cond fn

-- Running a turn entails showing the common state and waiting for them to press enter,
-- then prompting the current user for an action,
-- then parsing it, applying it to the state and returning the new state.
runTurn :: State -> IO State
runTurn s = do
    ANSI.clearScreen
    putStrLn $ showCommonState s
    putStr $ "Give computer to player " ++ (show $ getCurrentPlayer s) ++ " and press enter..."
    getLine -- and throw it away

    ANSI.clearScreen
    let pn = getCurrentPlayer s
    putStrLn $ showStateToPlayer pn s

    putStr "Enter action: "
    Haskeline.runInputT Haskeline.defaultSettings (readEvalAction s)

-- read action input from user
readAction :: Haskeline.InputT IO Action
readAction = do
    line <- fromJust <$> Haskeline.getInputLine "> "
    case (Parsec.parse action "" line) of
        Left _ -> do
            Haskeline.outputStrLn "Invalid input."
            readAction
        Right a -> pure a

readEvalAction :: State -> Haskeline.InputT IO State
readEvalAction s = do
    action <- readAction
    case act action s of
        Left m -> do
            Haskeline.outputStrLn m
            readEvalAction s
        Right s' -> pure s'

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

num :: Parsec.Parsec String () Int
num = read <$> Parsec.many1 PC.digit

clue :: Parsec.Parsec String () Clue
clue = Left <$> color 
    <|> Right <$> num

{-
d = simpleDeck
s = mkStartState (Seq.fromList [1,2]) d
s' = deal 5 s
putStrLn $ showStateToPlayer 1 s'

showPlayerOrder (Seq.fromList [], Fixed)
showPlayerOrder (Seq.fromList [2,3,1], Fixed)
showPlayerOrder (Seq.fromList [2,3,1], Loop)

runTurn s'

 -}
