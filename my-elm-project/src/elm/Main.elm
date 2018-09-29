import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import List exposing (concatMap, length, map, map2, map3)
import Set exposing (Set)
import String exposing (fromInt)
import Tuple exposing (first)
import Url.Builder as Url



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type alias Model =
  { state : State
  , hover_clue : Maybe Clue
  }

-- data synced from server:
type alias State =
    -- how many cards left in deck
    { deck : Int
    -- who is playing?
    , me : PlayerId
    -- exact cards in hand (for all players except the one indicated by 'me')
    , hands : Dict PlayerId Hand
    -- info about hands, including my own hand ('me' is a key)
    , views : Dict PlayerId HandView
    -- discards
    , discards : List Card
    -- played cards, the highest of each color (list because colors can't be map indexes)
    , played_cards : List (CardColor, CardNumber)
    -- remaining clues and fails
    , number_of_clues : Int
    , number_of_fails : Int
    -- current player at head of list, and indicator whether these are the last turns of the game
    , player_order: (List PlayerId, Continue)
    }

type alias PlayerId = Int

-- A hand you can see all of (i.e., not yours)
type alias Hand = List Card

-- Summary of all clues which have been given to this hand
-- (includes your hand + all other hands)
type alias HandView = List CardView

type alias Card = { color: CardColor, value: CardNumber }
type alias CardNumber = Int

-- CardView indicates info that has been clued about a card.
-- noCardView means we have no clue about this card.
-- Note: We use List Color rather than Set because Color is not comparable, and elm doesn't support making it comparable :(
type CardView = CardView (List Color) (Maybe CardNumber)
noCardView = CardView [] Nothing

type Color = Blue | White | Yellow | Green | Red
type CardColor = Colored Color | Rainbow
type Continue = Loop | Fixed

type Clue = ClueColor Color | ClueNumber CardNumber


init : () -> (Model, Cmd Msg)
init _ =
  ( Model testState Nothing
  , Cmd.none
  )

initState = State 0 0 Dict.empty Dict.empty [] [] 0 0 ([], Fixed)
testState = State 25 1
    -- other players' hands
    (Dict.fromList [(2, [Card (Colored Red) 1, Card Rainbow 2, Card (Colored Blue) 3])])
    -- all handviews incl 'me'
    (Dict.fromList [
        -- me
        (1, [noCardView, CardView [Red] (Just 1), CardView [] (Just 2), CardView [Blue] Nothing]),
        (2, [noCardView, CardView [Green,Yellow] Nothing, CardView [] (Just 3)])
    ])
    -- discards
    []
    -- played
    [(Colored Yellow, 1), (Colored Green, 2), (Rainbow, 5)]
    8
    3
    ([], Fixed)


-- UPDATE


type Msg =
  NewState (Result String State)
  | HoverClue Clue
  | ClearHover
  | GiveClue Clue


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewState (Ok newstate) -> ( { model | state = newstate }, Cmd.none )
    HoverClue clue -> ( { model | hover_clue = Just clue}, Cmd.none)
    ClearHover -> ( { model | hover_clue = Nothing}, Cmd.none)
    _ -> ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

cardColorClass: CardColor -> String
cardColorClass cc = case cc of
    Rainbow -> "color-rainbow"
    Colored Blue -> "color-blue"
    Colored White -> "color-white"
    Colored Yellow -> "color-yellow"
    Colored Green -> "color-green"
    Colored Red -> "color-red"

cardColorDesc: Color -> String
cardColorDesc c = case c of
    Blue -> "B"
    White -> "W"
    Yellow -> "Y"
    Green -> "G"
    Red -> "R"

-- "x" if empty, all single-char colors otherwise
cardViewColorDesc: List Color -> String
cardViewColorDesc csm = case csm of
    [] -> "x"
    cs -> String.concat (map cardColorDesc cs)

-- "x" if empty, the number otherwise
cardViewValueDesc: Maybe CardNumber -> String
cardViewValueDesc cnm = case cnm of
    Nothing -> "x"
    Just cn -> fromInt cn


nbsp: String
nbsp = "\u{00A0}"

cardViewDesc: CardView -> String
cardViewDesc cv = case cv of
    CardView [] Nothing -> nbsp     -- empty if we know nothing
    CardView cvs mv -> cardViewColorDesc cvs ++ cardViewValueDesc mv

faceUpCard : Bool -> Card -> CardView -> Html Msg
faceUpCard highlighted card cv =
    div [classList [("card", True), (cardColorClass card.color, True), ("highlighted", highlighted)] ]
    [ h1 [] [ text (fromInt card.value) ],
      h3 [] [ text (cardViewDesc cv)]
    ]

faceDownCard: CardView -> Html Msg
faceDownCard cv =
    div [classList [("card", True), ("facedown", True)] ]
    [ h3 [] [text (cardViewDesc cv)]]

hand : Model -> (PlayerId, Hand, HandView) -> Html Msg
hand model (pid, cs, hv) =
    let
        isHighlighted : Card -> Bool
        isHighlighted c = case model.hover_clue of
            Nothing -> False
            Just (ClueColor clue_color) -> Rainbow == c.color || (Colored clue_color) == c.color
            Just (ClueNumber clue_number) -> clue_number == c.value
    in
    div [class "hand"] (
    [ h2 [] [text ("Player " ++ (fromInt pid))] ]
    ++ (map2 (\card cardView -> faceUpCard (isHighlighted card) card cardView) cs hv)
    ++ [otherHandClueButtons pid]
    )

board : List (CardColor, CardNumber) -> Html Msg
board played_cards = div [class "hand board"] (
    [ h2 [] [text ("Board: ")] ]
    ++ (map (\(cc, cn) -> faceUpCard False (Card cc cn) noCardView) played_cards)
    )

tokens : Int -> Int -> Html Msg
tokens clues fails = div [class "tokens"] (
    List.repeat clues clueToken ++ List.repeat fails bombToken
    )

yourHand : Maybe HandView -> Html Msg
yourHand mhv = case mhv of
    Nothing -> div [] []
    Just hv -> div [class "hand"] ([h2 [] [text "Your hand:"]] ++ map faceDownCard hv)

otherHandsAndViews : State -> List (PlayerId, Hand, HandView)
otherHandsAndViews state =
    let
        otherPids = Dict.keys (state.hands)
        lookupHand pid = Maybe.withDefault [] (Dict.get pid (state.hands))
        lookupHandView pid = Maybe.withDefault [] (Dict.get pid (state.views))
    in map (\pid -> (pid, lookupHand pid, lookupHandView pid)) otherPids

clueButtonEvents : Clue -> List (Attribute Msg)
clueButtonEvents clue = [onClick (GiveClue clue), onMouseOver (HoverClue clue), onMouseLeave (ClearHover)]

colorClueButton : PlayerId -> Color -> Html Msg
colorClueButton pid c = button (clueButtonEvents (ClueColor c) ++ [class (cardColorClass (Colored c))]) [text nbsp]

numberClueButton : PlayerId -> CardNumber -> Html Msg
numberClueButton pid v = button (clueButtonEvents (ClueNumber v)) [text (fromInt v)]

otherHandClueButtons : PlayerId -> Html Msg
otherHandClueButtons pid = div [class "clue-buttons"] (
    map (colorClueButton pid) [Blue, White, Yellow, Green, Red]
    ++ [br [] []]
    ++ map (numberClueButton pid) (List.range 1 5))

bombToken : Html Msg
bombToken = div [class "token"] [text "💣"]

clueToken : Html Msg
clueToken = div [class "token"] [text "💥"]

view : Model -> Html Msg
view model =
  div []
    ([ h2 [] [ text ("Hanabi -- " ++ (fromInt (length (first model.state.player_order))) ++ " players") ]
    , br [] []
    , board (model.state.played_cards)
    , tokens model.state.number_of_clues model.state.number_of_fails
    , yourHand (Dict.get model.state.me model.state.views)
    ]
     ++ map (hand model) (otherHandsAndViews model.state))



