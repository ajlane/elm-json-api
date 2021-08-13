module Main exposing (..)

import Api.Http exposing (getIndexUrl, getSearchUrl, tracker)
import Api.Model exposing (Article, ArticleUrl(..), Index, IndexUrl(..), SearchResponse(..), SearchUrl(..))
import Browser exposing (Document)
import Html exposing (article, div, h1, input, label, li, p, text)
import Html.Attributes exposing (class, for, name)
import Html.Events exposing (onInput)
import Http exposing (Body, Error(..))
import Mock exposing (request)
import Model exposing (Loadable(..), Model)
import Msg exposing (Msg(..))
import Time


url =
    IndexUrl "http://localhost/blog"


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { article = Waiting
      , search = Waiting
      , query = ""
      , results = Nothing
      }
    , url
        |> getIndexUrl IndexReceived IndexUnavailable
        |> request
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IndexReceived index ->
            ( { model
                | article = Received index.featured
                , search = Received index.search
              }
            , Cmd.none
            )

        IndexUnavailable err ->
            ( { model
                | article = Unavailable err
                , search = Unavailable err
              }
            , Cmd.none
            )

        ArticleReceived article ->
            ( { model
                | article = Received article
              }
            , Cmd.none
            )

        ArticleUnavailable err ->
            ( { model
                | article = Unavailable err
              }
            , Cmd.none
            )

        QueryUpdated "" ->
            ( { model
                | query = ""
                , results = Nothing
              }
            , Cmd.none
            )

        QueryUpdated query ->
            case model.search of
                Waiting ->
                    ( { model
                        | query = ""
                        , results = Nothing
                      }
                    , Cmd.none
                    )

                Unavailable _ ->
                    ( { model
                        | query = ""
                        , results = Nothing
                      }
                    , Cmd.none
                    )

                Received search ->
                    ( { model
                        | query = query
                        , results = Just Waiting
                      }
                    , search
                        |> getSearchUrl { q = query, size = Just "10", after = Nothing } SearchResultsReceived SearchResultsUnavailable
                        |> tracker "search"
                        |> request
                    )

        SearchResultsReceived results ->
            ( { model
                | results = Just <| Received results
              }
            , Cmd.none
            )

        SearchResultsUnavailable err ->
            ( { model
                | results = Just <| Unavailable err
              }
            , Cmd.none
            )


subscriptions _ =
    Sub.none


view : Model -> Document Msg
view model =
    let
        title =
            case model.article of
                Received article ->
                    article.title

                Waiting ->
                    "Blog"

                Unavailable _ ->
                    "Error"

        searchBox =
            div [ class "search" ] <|
                case model.search of
                    Waiting ->
                        []

                    Unavailable _ ->
                        []

                    Received _ ->
                        [ label [ for "search" ] [ text "Search:" ]
                        , input [ name "search", onInput QueryUpdated ]
                            [ text model.query ]
                        ]

        resultsList =
            div [ class "results" ] <|
                case model.results of
                    Just (Received (SearchResponseSome hits)) ->
                        hits
                            |> List.map
                                (\hit ->
                                    li [] [ text hit.title ]
                                )

                    Just (Received SearchResponseNone) ->
                        [ text "No matches" ]

                    Just Waiting ->
                        [ text "Loading" ]

                    Just (Unavailable err) ->
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
                                [ text <| String.fromInt status ++ " response from server" ]

                    Nothing ->
                        []

        content =
            article [] <|
                case model.article of
                    Waiting ->
                        [ text "Please wait" ]

                    Received article ->
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
                                        Just <| p [] [ text line ]
                                )
                            |> div [ class "text" ]
                        ]

                    Unavailable error ->
                        [ div [ class "error" ] <|
                            case error of
                                BadBody msg ->
                                    [ text msg ]

                                BadUrl msg ->
                                    [ text msg ]

                                Timeout ->
                                    [ text "Request timed out" ]

                                NetworkError ->
                                    [ text "Network failure" ]

                                BadStatus status ->
                                    [ h1 [] [ text <| String.fromInt status ] ]
                        ]
    in
    { title = title
    , body =
        [ searchBox
        , resultsList
        , content
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
