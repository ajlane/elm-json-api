module Main exposing (..)

import Api.Article
import Api.IndexUrl
import Browser

url : IndexUrl
url = "http://localhost/blog"

type Model = Maybe Article

type Msg =
    ShowArticle Article

init _ = (Nothing, url |> Api.IndexUrl.get (\i -> ShowArticle i.featured)

main =Article.decode
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
