import Html exposing (Html, div, a, h3, text)
import Html.Attributes
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import Markdown.Block as Block

renderer : Markdown.Renderer.Renderer (Html msg)
renderer =
  { heading = heading
  , paragraph = paragraph
  , hardLineBreak = hardLineBreak
  , blockQuote = blockQuote
  , strong = strong
  , emphasis = emphasis
  , codeSpan = codeSpan
  , codeBlock = codeBlock
  , link = link
  , image = image
  , text = text
  , unorderedList = unorderedList
  , orderedList = orderedList
  , thematicBreak = thematicBreak
  , table = table
  , tableHeader = tableHeader
  , tableBody = tableBody
  , tableRow = tableRow
  , tableHeaderCell = tableHeaderCell
  , tableCell = tableCell
  , strikethrough = strikethrough
  , html = Markdown.Html.oneOf
           [ Markdown.Html.tag "vid" (vidUrl altTxt -> vidView vidUrl altTxt)
           |> Markdown.Html.withAttribute "url"
           |> Markdown.Html.withAttribute "alt"
           ]
  }

vidView : String String -> Html Msg
vidView vidUrl altTxt =
  div [ className "vid" ]
    [ a [ href vidUrl ] [ text ("Video!\nAlt text: " ++ altTxt) ] ]
