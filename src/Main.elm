module Main exposing (Msg(..), main, update)

import Browser
import Html exposing (Html, button, main_, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import Task exposing (Task)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    Response


getPerson : Int -> Task Http.Error Person
getPerson id =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://swapi.dev/api/people/" ++ String.fromInt id ++ "/"
        , body = Http.emptyBody
        , resolver = jsonResolver personDecoder
        , timeout = Nothing
        }


getPlanetName : String -> Task Http.Error String
getPlanetName url =
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = jsonResolver nameDecoder
        , timeout = Nothing
        }


jsonResolver : JD.Decoder a -> Http.Resolver Http.Error a
jsonResolver decoder =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case JD.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (JD.errorToString err))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { vader = Nothing }
    , Task.attempt GotReponse
        (getPerson 4
            |> Task.andThen
                (\person ->
                    Task.map
                        (\planetName ->
                            { vader = Just <| { name = person.name, homePlanet = Just planetName } }
                        )
                        (getPlanetName person.homeworld)
                )
        )
    )



-- UPDATE


type alias Person =
    { name : String
    , homeworld : String
    }


personDecoder : JD.Decoder Person
personDecoder =
    JD.map2 Person
        nameDecoder
        (JD.field "homeworld" JD.string)


nameDecoder : JD.Decoder String
nameDecoder =
    JD.field "name" JD.string


type alias HumanData =
    { name : String
    , homePlanet : Maybe String
    }


type alias Response =
    { vader : Maybe HumanData
    }


type Msg
    = GotReponse (Result Http.Error Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotReponse response ->
            case response of
                Ok r ->
                    ( r, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.vader of
        Just vader ->
            main_
                [ class "ly_cont" ]
                [ p []
                    [ text vader.name
                    ]
                , p []
                    [ text <| Maybe.withDefault "home planet is none." vader.homePlanet
                    ]
                ]

        Nothing ->
            main_ [] [ text "Nothing" ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
