module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, code, div, h1, h2, h3, ul, li, p, text)
import Html.Attributes exposing (title, href)
import Url exposing (Url)
import Url.Parser as P exposing (Parser, (</>), (<?>), s, top)
import Url.Parser.Query as Q
import Http
import Json.Decode as Json

-- MAIN

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChange
    , onUrlRequest = UrlRequest
    }

-- MODEL

type alias LinkItem =
  { description : String
  , url : String
  , color : String
  }

type alias WorkItem =
  { id : String
  , title : String
  , description : String
  , url : String
  , thumb : String
  , color : String
  }

type alias ContentList =
  { links : List LinkItem
  , work : List WorkItem
  }

type MainContent
  = Failure Http.Error
  | Loading
  | Success ContentList

type alias Model =
  { key : Nav.Key
  , route : Maybe Route
  , mainContent : MainContent
  }
  -- { history : List (Maybe Route)
  -- , key : Nav.Key
  -- , content : ContentList
  -- }

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
  ( Model key (P.parse routeParser url) Loading
  , getMainContent
  )
  -- ( Model [ P.parse routeParser url ] key
  -- , Cmd.none
  -- )

-- URL PARSING

type Route
  = Home
  | BlogList (Maybe String)
  | BlogPost Int

routeParser : Parser (Route -> a) a
routeParser =
  P.oneOf
    [ P.map Home top
    , P.map BlogList (s "blog" <?> Q.string "search")
    , P.map BlogPost (s "blog" </> P.int)
    ]

-- UPDATE

type Msg
  = UrlChange Url
  | UrlRequest Browser.UrlRequest
  | HasMainContent (Result Http.Error ContentList)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UrlChange url ->
      ( { model | route = P.parse routeParser url }
      , Cmd.none
      )

    UrlRequest request ->
      case request of
        Browser.Internal url ->
          ( model
          , Nav.pushUrl model.key (Url.toString url)
          )

        Browser.External url ->
          ( model
          , Nav.load url
          )

    HasMainContent result ->
      case result of
        Ok contentList ->
          ( { model | mainContent = Success contentList }
          , Cmd.none
          )

        Err error ->
          ( { model | mainContent = Failure error }
          , Cmd.none
          )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW

view : Model -> Browser.Document Msg
view model =
  Browser.Document "Example Page for elm/url"
    [ div []
        [ h1 [] [ text "Menu" ]
        , ul [] (List.map viewMenuItem ["/", "/blog/", "/blog/42", "/blog/37", "/blog/?search=cats" ])
        , h1 [] [ text "Route" ]
        , div [] [ viewRoute model.route ]
        , h1 [] [ text "Content" ]
        , div [] (viewMainContent model.mainContent)
        ]
    ]

viewMenuItem : String -> Html Msg
viewMenuItem url =
  li [] [ viewHref url url ]

viewHref : String -> String -> Html Msg
viewHref url txt =
  a [ href url ] [ text txt ]

viewRoute : Maybe Route -> Html Msg
viewRoute maybeRoute =
  case maybeRoute of
    Nothing ->
       code [] [ text "Unknown URL" ]

    Just route ->
      code [] [ text (Debug.toString route) ]

viewMainContent : MainContent -> List (Html Msg)
viewMainContent mainContent =
  case mainContent of
    Failure error ->
      [ h3 [] [ text "Content failed to load :<" ]
      , p []
        [ text
            (case error of
               Http.BadUrl str -> "Bad url: " ++ str
               Http.Timeout -> "Request timed out"
               Http.NetworkError -> "Network error"
               Http.BadStatus code -> "Bad status: " ++ String.fromInt code
               Http.BadBody str -> "Request returned bad body: " ++ str
            )
        ]
      ]

    Loading ->
      [ h3 [] [ text "Loading..." ] ]

    Success contentList ->
      [ h2 [] [ text "Links" ]
      , ul [] (List.map listLink contentList.links)
      , h2 [] [ text "Work" ]
      , ul [] (List.map listWork contentList.work)
      ]

listLink : LinkItem -> Html Msg
listLink linkItem =
  viewHref linkItem.url linkItem.description

listWork : WorkItem -> Html Msg
listWork workItem =
  viewHref ("/" ++ workItem.id) workItem.description

-- HTTP

getMainContent : Cmd Msg
getMainContent =
  Http.get
    { url = "./content.json"
    , expect = Http.expectJson HasMainContent contentListDecoder
    }

contentListDecoder : Json.Decoder ContentList
contentListDecoder =
  Json.map2 ContentList
    (Json.field "links"
       (Json.list
          (Json.map3 LinkItem
             (Json.field "description" Json.string)
             (Json.field "url" Json.string)
             (Json.field "color" Json.string)
          )
       )
    )
    (Json.field "work"
       (Json.list
          (Json.map6 WorkItem
             (Json.field "id" Json.string)
             (Json.field "title" Json.string)
             (Json.field "description" Json.string)
             (Json.field "url" Json.string)
             (Json.field "thumb" Json.string)
             (Json.field "color" Json.string)
          )
       )
    )

-- type alias LinkItem =
--   { description : String
--   , url : String
--   , color : String
--   }

-- type alias WorkItem =
--   { id : String
--   , title : String
--   , description : String
--   , url : String
--   , thumb : String
--   , color : String
--   }
