import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (Decoder, string, at)
import Json.Decode.Pipeline exposing (decode, required, requiredAt)
import Json.Decode as Decode
import Json.Encode as Encode
import WebSocket
import List

main: Program Never Model Msg
main =
  Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

 -- MODEL
type alias Model =
  { chatMessage : List String
  , loginMessage : List String
  , userMessage : String
  , loginName : String
  }

init : (Model, Cmd Msg)
init =
  ( Model [""] [""] "" ""
  , Cmd.none
  )

type alias ChatMessage =
  { command: String
  , content: String
  }

-- UPDATE
type Msg
  = PostChatMessage
  | LoginMessage
  | UpdateUserMessage String
  | NewChatMessage String
  | NewLoginName String

stringToDiv : String -> Html.Html msg
stringToDiv n =
  div [] [ text n ]

--dealWithIncomingStringToChatMsg : String -> (Model, Cmd Msg)
--dealWithIncomingStringToChatMsg d =


--messageDecoder : Decoder ChatMessage
--messageDecoder =
--  toMessage : Decoder ChatMessage
--  toMessage =
--    decode ChatMessage
--      |> Json.Decode.Pipeline.required "command" string
--      |> Json.Decode.Pipeline.required "content" string


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PostChatMessage ->
      let
        message = model.userMessage
      in
        { model | userMessage = "" } ! [WebSocket.send "ws://localhost:3000/" message]
    --PostChatMessage ->
    --  let
    --    user = Encode.object [ ("command", Encode.string "user"), ("content", Encode.string model.userMessage)]
    --  in
    --    { model | userMessage = "" } ! [WebSocket.send "ws://localhost:3000/" (Encode.encode 0 user)]

    LoginMessage ->
      let
        login = Encode.object [ ("command", Encode.string "login"), ("content", Encode.string model.loginName)]
      in
        { model | loginName = "" } ! [WebSocket.send "ws://localhost:3000/" (Encode.encode 0 login)]

    UpdateUserMessage message ->
      { model | userMessage = message } ! []

    NewChatMessage message ->
      --dealWithIncoming message
        { model | chatMessage = jsonToString (Decode.decodeString (Decode.field "content" Decode.string) message) :: model.chatMessage } ! []

    NewLoginName message ->
      {model | loginName = message} ! []
      --{ model | loginMessage = jsonToString (Decode.decodeString (Decode.field "content" Decode.string) message) :: model.loginMessage } ! []


jsonToString : Result String String -> String
jsonToString result =
  case result of
    Ok result -> result
    Err result -> result

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "login..."
            , autofocus True
            , value model.loginName
            , onInput NewLoginName
            ] []
    , button [ onClick LoginMessage ] [ text "Submit" ]
    , div [] []
    , input [ placeholder "message..."
              , autofocus True
              , value model.userMessage
              , onInput UpdateUserMessage
              ] []
      , button [ onClick PostChatMessage ] [ text "Submit" ]
      --, h2 [] [ text ("Login message")]
      --, div [] (List.map showMessage model.loginMessage)
      , h2 [] [ text ("Login and Chat messages")]
      , div [] (List.map showMessage model.chatMessage)
    ]

showMessage : String -> Html msg
showMessage message =
  div [] [text message]

 -- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen "ws://localhost:3000" NewChatMessage
