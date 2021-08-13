module Model exposing (..)

import Api.Model exposing (Article, SearchResponse, SearchUrl)
import Http


type Loadable a
    = Waiting
    | Received a
    | Unavailable Http.Error


type alias Model =
    { article : Loadable Article
    , search : Loadable SearchUrl
    , query : String
    , results : Maybe (Loadable SearchResponse)
    }
