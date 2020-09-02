module Main exposing (..)

import Browser
import Html
import Term exposing (Term)
import Term.ANSI exposing (defaultFormat)
import Debug

-- MAIN
main =
  Browser.document
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

-- PORTS

-- MODEL
type alias Model =
  { term : Term Msg
  }

-- INIT
init : () -> (Model, Cmd Msg)
init flags =
  (Model myTerm, Cmd.none)

myTerm = Term.offline (Just { defaultFormat | foreground = Term.ANSI.Green }) UserInput

-- UPDATE

type Msg
  = UserInput String

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UserInput str ->
      let newTerm = Term.receive (str ++ "\n") model.term in -- echo the message
      ( Model newTerm, Cmd.none )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW
view : Model -> Browser.Document Msg
view model =
  { title = "My Term"
  , body =
    [ Html.h1 [] [Html.text "Check out my cool terminal:"]
    , Term.render model.term
    ]
  }