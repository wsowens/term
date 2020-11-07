{-
Copyright 2020 William Owens

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}
module Term exposing (Term, Connection(..), withFormat, offline, new, receive, render)

{-| This module contains the basic term type.
# Definition
@docs Term
@docs Connection

# Creating Terms
@docs withFormat
@docs offline
@docs new

# Using Terms
@docs receive
@docs render
-}
import Array
import Browser
import Browser.Dom as Dom
import Html exposing (div, h1, h2, a, textarea, text)
import Html.Attributes as Attr exposing (class, id)
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Task

import Term.ANSI as ANSI


-- BASIC TERM TYPES
{-| A `Term` is the main data structure exposed by this package. A `Term` can
receive ANSI-encoded messages, convert user input into new Msgs, and quickly
render a chat log. The fields of a `Term` are as follows:
- `status` any current information about the connection
- `log` stores all of the previously received messages (already encoded into
HTML)
- `format` stores the current format that the `Term` will apply to incoming
messages
- `events` stores functions to trigger if the user attempts to submit input
or connect to a new address
Rather than create a `Term` directly, I recommend using the functions in the
section below.
-}
type alias Term msg =
  { status : Maybe Connection
  , log : Array.Array (Html.Html msg)
  , format : Maybe ANSI.Format
  , events : Handlers msg
  }

{-| A `Connection` can be used to information about what a `Term` is connected
to.
-}
type Connection
  = Open String
  | Connecting String
  | Closed

type alias Handlers msg =
  { onConnect: (Html.Attribute msg)
  , onInput: (Html.Attribute msg)
  }

{- Useful when you don't want your term to handle user input.
Admittedly, this is a bit of a hack.
-}
noEvent: Html.Attribute msg
noEvent = class "_nullev"
nullHandlers: Handlers msg
nullHandlers = Handlers noEvent noEvent

-- Helper Functions for creating Terms

{-| Create a formatted Term that does not accept any user input or display a
URL bar. This may be useful if you just want to decorate your website or have a
cool way to display incoming messages.

When selecting a format you can either use the default or build off of it.

    Term.withFormat ANSI.defaultFormat -- creates a default-formatted term

    greenBold = {defaultFormat | foreground = ANSI.Green, bold = True }
    Term.withFormat greenBold -- creates a Term with green, bold text
-}
withFormat: ANSI.Format -> Term msg
withFormat fmt = Term Nothing Array.empty (Just fmt) nullHandlers

{-| Create a Term that has full formatting and user input, but doesn't display
the URL bar. This may be useful if you want to create an interactive terminal
for your user, but you don't need the users to connect to any websites.

`fmt` is an optional ANSI.format (see `withFormat` above). (Note, you can pass
`Nothing` as the format to disable formatting entirely.) `onInput` should be a
`Msg` that accepts some type of String. Once the user types something in the
terminal and presses enter, a Msg of type `onInput` will fire.
-}
offline: Maybe ANSI.Format -> (String -> msg) -> Term msg
offline fmt onInput =
  Term Nothing Array.empty fmt
    (Handlers
      (noEvent)
      (inputHelper onInput)
    )

{-| Create a `Term` with complete configuration.
For details on `fmt` and `onInput`, see `withFormat` above. This function also
accepts another Msg type, `onConnect`. Similar to `onInput`, whatever Msg you
provide for `onConnect` will fire when the user presses enter after typing in
the URL bar.

If `status` is `Nothing`, the URL bar will be hidden entirely. Otherwise, you can
use status to store information about the current connection.
-}
new: Maybe Connection -> Maybe ANSI.Format -> (String -> msg) -> (String -> msg) -> Term msg
new status fmt onConnect onInput =
  Term status Array.empty fmt
    (Handlers
      (connectHelper onConnect)
      (inputHelper onInput)
    )


{-| Send an ANSI-escaped message to the Term. Produces a term with an updated
list of messages and updated format state.
-}
receive : String -> Term msg -> Term msg
receive message term =
  appendBuffer (ANSI.parseEscaped term.format message) term


{- Append the contents of the buffer to the Term's log. Also copy the most
recent format from the buffer to the terminal.
-}
appendBuffer : ANSI.Buffer msg -> Term msg -> Term msg
appendBuffer buf term =
  let new_msgs = Array.fromList buf.nodes in
  { term | format = term.format, log = Array.append term.log new_msgs }




{-| Emits HTML code for a `Term`. This can be used in a typical `view` function
for your application, for example:

    view : Model -> Html.Html msg
    view model =
      Html.div [ ]
        [ Html.h1 [ ] [ Html.text "check out my cool terminal:"]
        , Term.render model.term
        ]
-}
render : Term msg -> Html.Html msg
render term =
  div [ class "term" ]
    [ term.status
      |> Maybe.map (renderStatus term.events.onConnect)
      |> Maybe.withDefault (div [ class "term-element" ] [])
    , div [ class "term-element", id "term-output"] (Array.toList term.log)
    , textarea [ class "term-element", id "term-input", Attr.spellcheck False,
      Attr.placeholder "Type a command here. Press [Enter] to submit.",
      term.events.onInput, Attr.value "" ] []
    ]

renderStatus : Html.Attribute msg -> Connection -> Html.Html msg
renderStatus onConnect status =
  div [ class "term-element"]
      [ div [id "term-url-bar"]
      [ text "Connected to:"
      , Html.input [id "term-url-input", Attr.spellcheck False,
          Attr.placeholder "ws://server-domain.com:port", onConnect ] []
      , statusText status ]
      ]


-- helper function for renderStatus
statusText : Connection -> Html.Html msg
statusText status =
  case status of
    Open _ -> text "[CONNECTED]"
    Connecting _ -> text "[CONNECTING...]"
    Closed ->  text "[CLOSED]"

-- EVENT HANDLER HELPERS

{- This function wraps the user's onConnect event handler.
This function returns a custom HTML.Attribute, like onInput, that
decodes the incoming user message and passes it to the user-provided function.
-}
connectHelper : (String -> msg) ->  Html.Attribute msg
connectHelper onConnect =
  eventDecoder
  |> Decode.andThen checkEnter
  |> Decode.map (\v -> (onConnect v, False) )
  |> Events.stopPropagationOn "keypress"

{- This function wraps the user's onInput event handler. It works like
connectHelper above. However, unlike connectHelper, inputHelper will only
accept "enter" to fire a msg. "enter + shift" will enter a newline into the
user input.
-}
inputHelper : (String -> msg) -> Html.Attribute msg
inputHelper onInput =
  eventDecoder
  |> Decode.andThen checkEnterShift
  |> Decode.map (\v -> ( onInput v, False) )
  |> Events.stopPropagationOn "keypress"


checkEnterShift: Event -> Decoder String
checkEnterShift e =
  if e.key == 13 then
    if e.shift then
      Decode.fail "Shift key pressed with enter"
    else
      Events.targetValue
  else
    Decode.fail "Shift key pressed with enter"


checkEnter: Event -> Decoder String
checkEnter e =
  if e.key == 13 then
    Events.targetValue
  else
    Decode.fail "Shift key pressed with enter"


type alias Event =
  { shift : Bool
  , key : Int
  }

eventDecoder : Decoder Event
eventDecoder =
  Decode.map2 Event
    (Decode.field "shiftKey" Decode.bool)
    (Decode.field "keyCode" Decode.int)
