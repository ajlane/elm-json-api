module Api.UpdateArticleResponse exposing (..)

import Api.Article


{-| The response to a request to update an existing article


-}
type UpdateArticleResponse
    = Ok Api.Article.Article
    | Outdated Api.Article.Article
