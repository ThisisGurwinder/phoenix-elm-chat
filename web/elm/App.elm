module App exposing (..)

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (value, placeholder, class)
import Html.Events exposing (onInput, onClick, onSubmit)
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))


type alias Model =
    { newMessage : String
    , messages : List ChatMessage
    , phxSocket : Phoenix.Socket.Socket Msg
    }


type Msg
    = SetNewMessage String
    | JoinChannel
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | SendMessage
    | ReceiveChatMessage JE.Value


type alias ChatMessage =
    { user : String
    , body : String
    }


initialModel : Model
initialModel =
    { newMessage = ""
    , messages = []
    , phxSocket = initPhxSocket
    }


socketServer : String
socketServer =
    "ws://localhost:4000/socket/websocket"


initPhxSocket : Phoenix.Socket.Socket Msg
initPhxSocket =
    Phoenix.Socket.init socketServer
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on "new:msg" "room:lobby" ReceiveChatMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNewMessage string ->
            { model | newMessage = string } ! []

        JoinChannel ->
            let
                channel =
                    Phoenix.Channel.init "room:lobby"

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.join channel model.phxSocket
            in
                ( { model | phxSocket = phxSocket }
                , Cmd.map PhoenixMsg phxCmd
                )

        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.update msg model.phxSocket
            in
                ( { model | phxSocket = phxSocket }
                , Cmd.map PhoenixMsg phxCmd
                )

        SendMessage ->
            let
                payload =
                    (JE.object [ ( "body", JE.string model.newMessage ) ])

                push' =
                    Phoenix.Push.init "new:msg" "room:lobby"
                        |> Phoenix.Push.withPayload payload

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.push push' model.phxSocket
            in
                ( { model
                    | newMessage = ""
                    , phxSocket = phxSocket
                  }
                , Cmd.map PhoenixMsg phxCmd
                )

        ReceiveChatMessage raw ->
            case JD.decodeValue chatMessageDecoder raw of
                Ok chatMessage ->
                    ( { model | messages = chatMessage :: model.messages }
                    , Cmd.none
                    )

                Err error ->
                    ( model, Cmd.none )


chatMessageDecoder : JD.Decoder ChatMessage
chatMessageDecoder =
    JD.object2 ChatMessage
        (JD.oneOf
            [ ("user" := JD.string)
            , JD.succeed "anonymous"
            ]
        )
        ("body" := JD.string)


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick JoinChannel ] [ text "Join lobby" ]
        , div [ class "messages" ]
            (List.map viewMessage model.messages)
        , form [ onSubmit SendMessage ]
            [ input [ placeholder "Message...", onInput SetNewMessage, value model.newMessage ] [] ]
        ]


viewMessage : ChatMessage -> Html Msg
viewMessage message =
    div [ class "message" ]
        [ span [ class "user" ] [ text (message.user ++ ": ") ]
        , span [ class "body" ] [ text message.body ]
        ]


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Phoenix.Socket.listen model.phxSocket PhoenixMsg


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )
