port module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (bool, int, string, float, list, nullable, Decoder, decodeValue)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode exposing (Value)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { clicked : Bool
    , frontdata : Result String FrontData
    }


init : ( Model, Cmd Msg )
init =
    ( { clicked = False
      , frontdata =
            Err "No Convo Yet loaded"
            -- This should be a Maybe (Result String Convo), since we dont' yet
            -- have the data, it's not an "error" yet.
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Click
    | ConversationLoaded (Result String FrontData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( { model | clicked = True }
            , Cmd.none
            )

        ConversationLoaded c ->
            ( { model | frontdata = c }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ showClickState model.clicked
        , br [] []
        , showConversation model.frontdata
        ]


showClickState : Bool -> Html Msg
showClickState clicked =
    case clicked of
        False ->
            button
                [ onClick Click ]
                [ text "Click Here" ]

        True ->
            text "You clicked"


showConversation : Result String FrontData -> Html Msg
showConversation frontdata =
    case frontdata of
        Ok fd ->
            div []
                [ text "Loaded up a conversation fron: "
                , text fd.conversation.contact.display_name
                ]

        Err err ->
            text err



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    conversation (\val -> ConversationLoaded (decodeValue decodeData val))



-- Ports


port conversation : (Value -> msg) -> Sub msg



-- Decoders
--


type alias FrontData =
    { conversation : Conversation
    , contact : Contact
    }


decodeData : Decoder FrontData
decodeData =
    decode FrontData
        |> Json.Decode.Pipeline.required "conversation" decodeConversation
        |> Json.Decode.Pipeline.required "contact" decodeContact


type alias Conversation =
    { id : String
    , link : String
    , message_type : String
    , status : String
    , replied : Bool
    , reacted : Bool
    , num_messages : Int
    , contact : Contact
    , assignee : String
    , inboxes : List String
    , followers : List String
    , tags : List String
    , seen_by : List String
    , subject : String
    , summary : String
    , has_draft : Bool
    }


decodeConversation : Decoder Conversation
decodeConversation =
    decode Conversation
        |> Json.Decode.Pipeline.required "id" string
        |> Json.Decode.Pipeline.required "link" string
        |> Json.Decode.Pipeline.required "message_type" string
        |> Json.Decode.Pipeline.required "status" string
        |> Json.Decode.Pipeline.required "replied" bool
        |> Json.Decode.Pipeline.required "reacted" bool
        |> Json.Decode.Pipeline.required "num_messages" int
        |> Json.Decode.Pipeline.required "contact" decodeContact
        |> Json.Decode.Pipeline.required "assignee" string
        |> Json.Decode.Pipeline.required "inboxes" (list string)
        |> Json.Decode.Pipeline.required "followers" (list string)
        |> Json.Decode.Pipeline.required "tags" (list string)
        |> Json.Decode.Pipeline.required "seen_by" (list string)
        |> Json.Decode.Pipeline.required "subject" string
        |> Json.Decode.Pipeline.required "summary" string
        |> Json.Decode.Pipeline.required "has_draft" bool


type alias Contact =
    { id : String
    , handle : String
    , initials : String
    , display_name : String
    , description : Maybe String
    , avatar : Maybe String
    , source : String
    , role : String
    , num_notes : Int
    }



-- , color : String


decodeContact : Decoder Contact
decodeContact =
    decode Contact
        |> Json.Decode.Pipeline.required "id" string
        |> Json.Decode.Pipeline.required "handle" string
        |> Json.Decode.Pipeline.required "initials" string
        |> Json.Decode.Pipeline.required "display_name" string
        |> Json.Decode.Pipeline.required "description" (nullable string)
        |> Json.Decode.Pipeline.required "avatar" (nullable string)
        |> Json.Decode.Pipeline.required "source" string
        |> Json.Decode.Pipeline.required "role" string
        |> Json.Decode.Pipeline.required "num_notes" int



-- |> Json.Decode.Pipeline.required "color" string
