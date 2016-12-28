port module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (href, target)
import Json.Decode exposing (bool, int, string, float, list, nullable, Decoder, decodeValue)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode exposing (Value)
import RemoteData exposing (RemoteData, WebData)
import Http
import Task exposing (..)


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
    , frontData : RemoteData String FrontData
    , apmDetails : WebData ApmDetails
    }


init : ( Model, Cmd Msg )
init =
    ( { clicked = False
      , frontData =
            RemoteData.NotAsked
      , apmDetails =
            RemoteData.NotAsked
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ConversationLoaded (RemoteData String FrontData)
    | ApmDetailsReceived (WebData ApmDetails)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ConversationLoaded data ->
            ( { model | frontData = data, apmDetails = RemoteData.Loading }
            , case data of
                RemoteData.Success d ->
                    getApmDetails d.contact.handle

                _ ->
                    Cmd.none
            )

        ApmDetailsReceived d ->
            ( { model | apmDetails = d }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ showConversation model.frontData
        , br [] []
        , showApmDetail model.apmDetails
        ]


showConversation : RemoteData String FrontData -> Html Msg
showConversation frontData =
    case frontData of
        RemoteData.NotAsked ->
            text "No Conversation Loaded"

        RemoteData.Loading ->
            text "Loading Conversation"

        RemoteData.Failure err ->
            text err

        RemoteData.Success fd ->
            div []
                [ text "Front says you're viewing a conversation from: "
                , text fd.conversation.contact.display_name
                ]


showApmDetail : WebData ApmDetails -> Html Msg
showApmDetail wd =
    case wd of
        RemoteData.Success apm ->
            let
                showOrg org =
                    div []
                        (List.concat
                            [ [ h2 [] [ a [ href (apmOrgUrl org), target "_blank" ] [ text org.name ] ]
                              , br [] []
                              , text ("Estimated Bill: $" ++ toString org.estimatedBill)
                              , br [] []
                              , br [] []
                              ]
                            , List.map showApp org.apps
                            ]
                        )

                showApp app =
                    div []
                        [ a [ href (apmAppUrl app), target "_blank" ]
                            [ text ("App (" ++ toString app.id ++ ")" ++ app.name)
                            ]
                        ]
            in
                div [] (List.map showOrg apm.orgs)

        RemoteData.Failure err ->
            text (toString err)

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.NotAsked ->
            text "No APM Data requested yet"


apmOrgUrl : ApmOrg -> String
apmOrgUrl org =
    "https://apm.scoutapp.com/admin/org?id=" ++ toString org.id


apmAppUrl : ApmApp -> String
apmAppUrl app =
    "https://apm.scoutapp.com/apps/" ++ toString app.id



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        data val =
            case (decodeValue decodeData val) of
                Err err ->
                    RemoteData.Failure err

                Ok fd ->
                    RemoteData.Success fd
    in
        conversation (ConversationLoaded << data)



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
    , assignee : Maybe String
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
        |> Json.Decode.Pipeline.required "assignee" (nullable string)
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



------------


type alias ApmDetails =
    { orgs : List ApmOrg
    }


type alias ApmOrg =
    { name : String
    , id : Int
    , estimatedBill : Float
    , apps : List ApmApp
    }


type alias ApmApp =
    { name : String
    , id : Int
    }


decodeApmDetails : Decoder ApmDetails
decodeApmDetails =
    decode ApmDetails
        |> Json.Decode.Pipeline.required "orgs" (list decodeOrg)


decodeOrg : Decoder ApmOrg
decodeOrg =
    decode ApmOrg
        |> Json.Decode.Pipeline.required "name" string
        |> Json.Decode.Pipeline.required "id" int
        |> Json.Decode.Pipeline.required "estimated_bill" float
        |> Json.Decode.Pipeline.required "apps" (list decodeApp)


decodeApp : Decoder ApmApp
decodeApp =
    decode ApmApp
        |> Json.Decode.Pipeline.required "name" string
        |> Json.Decode.Pipeline.required "id" int


getApmDetails : String -> Cmd Msg
getApmDetails customerEmail =
    let
        url =
            "https://apm.scoutapp.com/front_plugin/" ++ customerEmail

        request =
            Http.request
                { method = "GET"
                , url = url
                , expect = Http.expectJson decodeApmDetails
                , withCredentials = True
                , body = Http.emptyBody
                , headers = []
                , timeout = Nothing
                }
    in
        Http.send
            (ApmDetailsReceived << RemoteData.fromResult)
            request
