module Main exposing (..)

import Api.Http exposing (getIndexUrl, getSearchUrl, tracker)
import Api.Model exposing (Article, ArticleUrl(..), Index, IndexUrl(..), SearchResponse(..), SearchUrl(..))
import Browser exposing (Document)
import Html exposing (div, h1, input, li, p, text)
import Html.Attributes exposing (class, title)
import Html.Events exposing (onInput)
import Html.Keyed
import Http exposing (Body, Error(..), Expect, Header)
import Task
import Time


url =
    IndexUrl "http://localhost/blog"


mockRequest request =
    let
        exampleArticleUrl =
            ArticleUrl "http://localhost/blog/articles/0"

        exampleArticle =
            { self = exampleArticleUrl
            , title = "Lorem ipsum"
            , created = Time.millisToPosix 0
            , updated = Nothing
            , version = 1
            , body = "Dolor sit amet, consectetuer adipiscing elit. Nullam dictum felis eu pede mollis pretium. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem."
            }
    in
    case request.url of
        "http://localhost/blog" ->
            { featured = exampleArticle, search = SearchUrl "http://localhost/blog/search{?q,after,size}", self = url } |> Task.succeed |> Task.perform IndexReceived

        "http://localhost/blog/search?q=Lorem&after=&size=10" ->
            SearchResponseSome [ { href = exampleArticleUrl, id = "0", title = "Lorem ipsum", snippet = "Dolor sit amet, consectetuer adipiscing elit." } ] |> Task.succeed |> Task.perform SearchResultsReceived

        _ ->
            Http.BadStatus 404 |> Task.succeed |> Task.perform IndexUnavailable


type alias Model =
    { article : Maybe Article
    , search : Maybe SearchUrl
    , query : String
    , results : Maybe SearchResponse
    , error : Maybe Http.Error
    }


type Msg
    = IndexReceived Index
    | IndexUnavailable Http.Error
    | ArticleReceived Article
    | ArticleUnavailable Http.Error
    | QueryUpdated String
    | SearchResultsReceived SearchResponse
    | SearchResultsUnavailable Http.Error


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { article = Nothing, search = Nothing, query = "", results = Nothing, error = Nothing }, url |> getIndexUrl IndexReceived IndexUnavailable |> mockRequest )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IndexReceived index ->
            ( { model | article = Just index.featured, search = Just index.search }, Cmd.none )

        IndexUnavailable err ->
            ( { model | error = Just err }, Cmd.none )

        ArticleReceived article ->
            ( { model | article = Just article }, Cmd.none )

        ArticleUnavailable err ->
            ( { model | article = Nothing, error = Just err }, Cmd.none )

        QueryUpdated query ->
            case model.search of
                Just search ->
                    ( { model | query = query }, search |> getSearchUrl { q = query, size = Just "10", after = Nothing } SearchResultsReceived SearchResultsUnavailable |> tracker "search" |> mockRequest )

                Nothing ->
                    ( { model | query = query }, Cmd.none )

        SearchResultsReceived results ->
            ( { model | results = Just results }, Cmd.none )

        SearchResultsUnavailable err ->
            ( { model | error = Just err }, Cmd.none )


subscriptions _ =
    Sub.none


searchBox { search, query, results } =
    div [ class "search" ]
        (case search of
            Just _ ->
                [ input [ onInput QueryUpdated ]
                    [ text query ]
                , div
                    [ class "results" ]
                    (if query |> String.isEmpty then
                        []

                     else
                        case results of
                            Just (SearchResponseSome hits) ->
                                hits
                                    |> List.map
                                        (\hit ->
                                            li [] [ text hit.title ]
                                        )

                            Just SearchResponseNone ->
                                [ text "No matches" ]

                            Nothing ->
                                [ text "Loading" ]
                    )
                ]

            Nothing ->
                []
        )


view : Model -> Document Msg
view model =
    { title =
        case ( model.error, model.article ) of
            ( Just _, _ ) ->
                "Error"

            ( Nothing, Just article ) ->
                article.title

            ( Nothing, Nothing ) ->
                "Blog"
    , body =
        (case ( model.error, model.article ) of
            ( Just err, _ ) ->
                case err of
                    BadBody msg ->
                        [ text msg ]

                    BadUrl msg ->
                        [ text msg ]

                    Timeout ->
                        [ text "Request timed out" ]

                    NetworkError ->
                        [ text "Network failure" ]

                    BadStatus status ->
                        [ h1 [] [ text (String.fromInt status) ] ]

            ( Nothing, Just article ) ->
                [ h1 [] [ text article.title ]
                , div [ class "year" ]
                    [ article.updated
                        |> Maybe.withDefault article.created
                        |> Time.toYear Time.utc
                        |> String.fromInt
                        |> text
                    ]
                , article.body
                    |> String.split "\n"
                    |> List.filterMap
                        (\line ->
                            if line |> String.isEmpty then
                                Nothing

                            else
                                p [] [ text line ]
                                    |> Just
                        )
                    |> div []
                ]

            ( Nothing, Nothing ) ->
                [ text "Please wait" ]
        )
            ++ [ searchBox model ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
