import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import List exposing (length)
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

type alias Card = (CardColor, CardNumber)
type alias CardNumber = Int
type alias CardView = (Set Color, Maybe CardNumber)

type Color = Blue | White | Yellow | Green | Red
type CardColor = Colored Color | Rainbow
type Continue = Loop | Fixed


init : () -> (Model, Cmd Msg)
init _ =
  ( Model initState
  , Cmd.none
  )

initState = State 0 0 Dict.empty Dict.empty 0 0 ([], Fixed)


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


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text ("Hanabi -- " ++ (fromInt (length (first model.state.player_order))) ++ " players") ]
    , br [] []
    ]



