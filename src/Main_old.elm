module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, text, h1, h2, p, div, a, ul, li, b)
import Html.Attributes exposing (title, href)
import Url
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string)

-- MAIN

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

-- ROUTE

type Route
  = Topic String
  | Blog Int
  | User String
  | Comment String Int

routeParser : Parser (Route -> a) a
routeParser =
  oneOf
    [ map Topic (s "topic" </> string)
    , map Blog (s "blog" </> int)
    , map User (s "user" </> string)
    , map Comment (s "user" </> string </> s "comment" </> int)
    ]

-- MODEL

type alias Model =
  { key : Nav.Key
  , route : Route
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url, Cmd.none )

-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | route = routeParser url }
      , Cmd.none
      )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "URL Intercepter"
  , body =
    [ text "The current URL is: "
    -- , b [] [ text (Url.toString model.url) ]
    , ul []
      -- Topic String
      -- Blog Int
      -- User String
      -- Comment String Int
      [ viewLink "/topic/pottery"
      , viewLink "/topic/collage"
      , viewLink "/blog/42"
      , viewLink "/blog/123"
      , viewLink "/user/tom"
      , viewLink "/user/will/comment/23"
      , viewLink "/broken-link"
      -- , viewLink "/"
      ]
    , p []
      [ text
        (case model.route of
           Just (Topic topic) -> "This is topic " ++ topic
           Just (Blog blogId) -> "This is blog post #" ++ blogId
           Just (User name) -> "This is user " ++ name
           Just (Comment user commentId) -> "This is comment #" ++ commentId ++ " by " ++ user
           Nothing -> "This link is broken"
        )
      ]
    ]
  }

viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]
