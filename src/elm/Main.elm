module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, article, footer, h1, header, li, node, p, pre, section, text, ul)
import Html.Attributes exposing (class, href, target)
import Http
import Route exposing (Route)
import Url


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
    | AnalysisPage String


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
            ( { model | page = AnalysisPage raceName }
            , Cmd.none
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

            AnalysisPage raceName ->
                viewPostPage raceName
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
            ]

        viewListItem =
            \raceName ->
                li []
                    [ a [ href raceName ] [ text raceName ]
                    ]
    in
    node "main"
        []
        [ ul []
            (List.map viewListItem races)
        ]


viewPostPage : String -> Html Msg
viewPostPage raceName =
    node "main"
        []
        [ article []
            [ section []
                [ h1 [] [ text raceName ]
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
