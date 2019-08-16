module Main exposing (main)

import Analysis exposing (Analysis, getAnalysis)
import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, article, footer, h1, header, li, node, p, pre, section, text, ul)
import Html.Attributes exposing (class, href, target)
import Http
import Route exposing (Route)
import Url
import View.LapTimeChart exposing (viewLapTimeChart)
import View.LapTimeChartsByDriver exposing (viewLapTimeChartsByDriver)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = NotFound
    | ErrorPage Http.Error
    | TopPage
    | AnalysisPage Analysis


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    { key = key
    , page = TopPage
    }
        |> goTo (Route.parse url)



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | Loaded (Result Http.Error Page)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            goTo (Route.parse url) model

        Loaded (Ok page) ->
            ( { model | page = page }, Cmd.none )

        Loaded (Err error) ->
            ( { model | page = ErrorPage error }, Cmd.none )


goTo : Maybe Route -> Model -> ( Model, Cmd Msg )
goTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Route.Top ->
            ( { model | page = TopPage }, Cmd.none )

        Just (Route.Analysis raceName) ->
            ( model
            , raceName |> getAnalysis (Result.map AnalysisPage >> Loaded)
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Analysis - F1"
    , body =
        [ siteHeader
        , case model.page of
            NotFound ->
                viewNotFound

            ErrorPage error ->
                viewError error

            TopPage ->
                viewTopPage

            AnalysisPage analysis ->
                viewPostPage analysis
        , siteFooter
        ]
    }


siteHeader : Html Msg
siteHeader =
    Html.header [ class "site-header" ]
        [ h1 []
            [ a [ href "/" ] [ text "Analysis - F1" ]
            ]
        ]


viewNotFound : Html Msg
viewNotFound =
    text "not found"


viewError : Http.Error -> Html Msg
viewError error =
    case error of
        Http.BadBody message ->
            pre [] [ text message ]

        _ ->
            text (Debug.toString error)


viewTopPage : Html Msg
viewTopPage =
    let
        races =
            [ "01_Australia"
            , "02_Bahrain"
            , "03_China"
            , "04_Azerbaijan"
            , "05_Spain"
            , "06_Monaco"
            , "07_Canada"
            , "08_France"
            , "09_Austria"
            , "12_Hungary"
            ]

        viewListItem raceName =
            li []
                [ a [ href raceName ] [ text raceName ]
                ]
    in
    node "main"
        []
        [ ul [] (races |> List.map viewListItem)
        ]


viewPostPage : Analysis -> Html Msg
viewPostPage analysis =
    node "main"
        []
        [ article []
            [ h1 [] [ text analysis.eventName ]
            , section [ class "laptime-charts-by-driver" ]
                [ h1 [] [ text "Lap Time Charts By Driver" ]
                , viewLapTimeChartsByDriver analysis
                ]
            , section [ class "laptime-chart" ]
                [ h1 [] [ text "Lap Time Chart" ]
                , viewLapTimeChart analysis
                ]
            ]
        ]


siteFooter : Html Msg
siteFooter =
    footer [ class "site-footer" ]
        [ p [ class "copyright" ]
            [ text "© 2019 "
            , a [ href "https://y047aka.me", target "_blank" ] [ text "y047aka" ]
            ]
        ]
