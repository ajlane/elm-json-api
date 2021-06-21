module Api.NewArticleResponse exposing (..)

import Api.Article


{-| A response to a request to publish a new article.


-}
type NewArticleResponse
    = Ok Api.Article.Article
