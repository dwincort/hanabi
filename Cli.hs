{-# LANGUAGE RecordWildCards, PatternSynonyms #-}

module Cli where

import Hanabi

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import Data.Sequence (Seq, pattern (:<|), pattern (:|>), pattern Empty)
import qualified Data.Sequence as Seq

import qualified System.Console.ANSI as ANSI

-- Shows only information that is available to all players.
showCommonState :: State -> String
showCommonState state = intercalate "\n" [
    showDeck $ deck state,
    showPlayedCards $ played_cards state,
    showCluesFails (number_of_clues state) (number_of_fails state),
    showDiscards $ discards state,
    showPlayerOrder $ player_order state
    ]


-- Shows common information, plus information that is available to the given player.
showStateToPlayer :: PlayerId -> State -> String
showStateToPlayer p state = intercalate "\n" [
    "You are player " ++ (show p),
    showDeck $ deck state,
    showPlayedCards $ played_cards state,
    showCluesFails (number_of_clues state) (number_of_fails state),
    showDiscards $ discards state,
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
shortShowColor Rainbow = "*"

showCard :: Card -> String
showCard (color, number) = shortShowColor color ++ show number

showCards = intercalate " " . map showCard

showCluesFails :: Int -> Int -> String
showCluesFails c f = "There are " ++ show c ++ " clues and " ++ show f ++ " bombs remaining."

showDiscards :: [Card] -> String
showDiscards [] = "Discards: (none)"
showDiscards discards = "Discards: " ++ showCards discards

showNextPlayer :: PlayerId -> String
showNextPlayer p = "Next player is " ++ (show p) ++ "."

showPlayerOrder :: (Seq PlayerId, Continue) -> String
showPlayerOrder (Empty, Fixed) = "The game is over."
showPlayerOrder (np :<| Empty, Fixed) = showNextPlayer np ++ " This is the last turn."
showPlayerOrder (np :<| nps, Fixed) = showNextPlayer np ++ " There are " ++ (show $ 1 + length nps) ++ " turns left."
showPlayerOrder (Empty, Loop) = error "oops"
showPlayerOrder (np :<| _, Loop) = showNextPlayer np

showHand :: Hand -> String
showHand = showCards . map fst

showHands :: Map PlayerId Hand -> String
showHands hs = intercalate " " [ "Player " ++ (show k) ++ ": " ++ (showHand v)
                                 | (k,v) <- Map.toList hs ]

-- Running a turn entails showing the common state and waiting for them to press enter,
-- then prompting the current user for an action,
-- then parsing it, applying it to the state and returning the new state.
runTurn :: State -> IO State
runTurn s = do
    ANSI.clearScreen
    putStrLn $ showCommonState s
    putStr "Give computer and press enter..."
    getLine -- and throw it away
    
    let pn = getCurrentPlayer s
    putStrLn $ showStateToPlayer pn s

    putStr "Enter action: "
    action <- getLine
    putStrLn $ "You wrote: " ++ action
    pure s


d = simpleDeck
s = mkStartState (Seq.fromList [1,2]) d
s' = deal 5 s
{-
putStrLn $ showStateToPlayer 1 s'

showPlayerOrder (Seq.fromList [], Fixed)
showPlayerOrder (Seq.fromList [2,3,1], Fixed)
showPlayerOrder (Seq.fromList [2,3,1], Loop)

runTurn s'

 -}
