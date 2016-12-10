module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode


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
    , data : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( Model False Nothing
    , Cmd.none
    )



-- UPDATE


type RemoteData
    = RemoteData String


type Msg
    = Click
    | FetchedData RemoteData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( { model | clicked = True }
            , Cmd.none
            )

        FetchedData newData ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ showClickState model.clicked
        , br [] []
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
