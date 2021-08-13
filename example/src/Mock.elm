module Mock exposing (..)

import Api.Http exposing (mock)
import Api.Model exposing (ArticleUrl(..), IndexUrl(..), SearchResponse(..), SearchUrl(..))
import Http exposing (Error(..))
import Msg exposing (Msg(..))
import Time
import Url
import Url.Parser exposing ((</>), (<?>), parse, s)
import Url.Parser.Query as Query


request =
    let
        indexUrl =
            IndexUrl "http://mock.example/blog"

        searchUrl =
            SearchUrl "http://mock.example/blog/search{?q,after,size}"

        exampleArticleUrl =
            ArticleUrl "http://mock.example/blog/articles/0"

        exampleArticle =
            { self = exampleArticleUrl
            , title = "Lorem ipsum"
            , created = Time.millisToPosix 0
            , updated = Nothing
            , version = 1
            , body = "Dolor sit amet, consectetuer adipiscing elit. Nullam dictum felis eu pede mollis pretium.\n\nAenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.\n\nDonec quam felis, ultricies nec, pellentesque eu, pretium quis, sem."
            }

        exampleSearchHit =
            { id = "example"
            , href = exampleArticleUrl
            , title = exampleArticle.title
            , snippet = exampleArticle.body |> String.left 75
            }

        response requestUrl =
            case requestUrl.path of
                "/blog" ->
                    IndexReceived <| { featured = exampleArticle, search = searchUrl, self = indexUrl }

                "/blog/search" ->
                    case requestUrl |> (s "blog" </> s "search" <?> Query.string "q" |> parse) of
                        Just (Just q) ->
                            if exampleArticle.title ++ "\n" ++ exampleArticle.body |> String.toLower |> String.contains (q |> String.toLower) then
                                SearchResultsReceived <| SearchResponseSome [ exampleSearchHit ]

                            else
                                SearchResultsReceived <| SearchResponseNone

                        Just Nothing ->
                            SearchResultsReceived <| SearchResponseNone

                        Nothing ->
                            SearchResultsUnavailable <| Http.BadUrl ((requestUrl |> Url.toString) ++ " is not a valid URL.")

                _ ->
                    IndexUnavailable <| Http.BadStatus 404
    in
    mock response
