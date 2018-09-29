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


init : () -> (Model, Cmd Msg)
init _ =
  ( Model testState
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
    0
    0
    ([], Fixed)


-- UPDATE


type Msg =
  NewState (Result String State)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewState (Ok newstate) -> ( { model | state = newstate }, Cmd.none )
    NewState (Err _) -> ( model, Cmd.none )



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

cardViewValueDesc: Maybe CardNumber -> String
cardViewValueDesc cnm = case cnm of
    Nothing -> "?"
    Just cn -> fromInt cn

cardViewDesc: CardView -> String
cardViewDesc cv = case cv of
    CardView [] Nothing -> "\u{00A0}"     -- empty (nonbreaking space) if we know nothing
    CardView cvs mv -> String.concat (map cardColorDesc cvs) ++ cardViewValueDesc mv

faceUpCard : Card -> CardView -> Html Msg
faceUpCard card cv =
    div [classList [("card", True), (cardColorClass card.color, True)] ]
    [ h1 [] [ text (fromInt card.value) ],
      h3 [] [ text (cardViewDesc cv)]
    ]

faceDownCard: CardView -> Html Msg
faceDownCard cv =
    div [classList [("card", True), ("facedown", True)] ]
    [ h3 [] [text (cardViewDesc cv)]]

hand : (PlayerId, Hand, HandView) -> Html Msg
hand (pid, cs, hv) = div [class "hand"] (
    [ h2 [] [text ("Player " ++ (fromInt pid))] ]
    ++ (map2 faceUpCard cs hv)
    )

board : List (CardColor, CardNumber) -> Html Msg
board played_cards = div [class "hand board"] (
    [ h2 [] [text ("Board: ")] ]
    ++ (map (\(cc, cn) -> faceUpCard (Card cc cn) noCardView) played_cards)
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


view : Model -> Html Msg
view model =
  div []
    ([ h2 [] [ text ("Hanabi -- " ++ (fromInt (length (first model.state.player_order))) ++ " players") ]
    , br [] []
    , board (model.state.played_cards)
    , yourHand (Dict.get model.state.me model.state.views)
    ]
     ++ map hand (otherHandsAndViews model.state))



