import Html exposing (div, text, select, option, button, span, br)
import Html.App exposing (beginnerProgram, program)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (value, style)
import String exposing (toInt)
import Array exposing (fromList, toList, Array)
import Time exposing (Time)


main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  { width : Int
  , started : Bool
  , boards : Array (List Int)
  , index : Int
  }


init =
  (Model 4 False (initBoards 4) 0, Cmd.none)


subscriptions model =
  Time.every Time.second Tick


type Msg
  = ChangeWidth String
  | StartStop
  | Tick Time


initBoards width =
  fromList <|
    queenPuzzle width width


getBoard model =
  case Array.get model.index model.boards of
    Just value ->
      fromList value
    Nothing ->
      fromList []


view model =
  div []
    [ div []
      [ button [onClick StartStop]
        [ text (if model.started then "Stop" else "Start") ]
      ]
    , div [] 
      [ select
        [ onInput ChangeWidth ]
        (List.map (\x -> option [value (toString x)] [text (toString x)]) [4..8])
      ]
    , viewBoard <| getBoard model
    ]


isEven int =
  int % 2 == 0


viewBoard board =
  div [] <|
    List.map
      (\j ->
        div [] <|
          List.map
            (\i ->
              span [tileStyle <| isEven (i + j)] <|
                [ text <|
                  if Array.get j board == Just i then
                    "♛"
                  else
                    "."
                ]
            )
            [0..(Array.length board - 1)]
      )
      [0..(Array.length board - 1)]


tileStyle isWhite =
  style
    [ ("color", if isWhite then "black" else "white")
    , ("background-color", if isWhite then "white" else "black")
    , ("width", "50px")
    , ("height", "50px")
    , ("display", "inline-block")
    , ("text-align", "center")
    ]


update msg model =
  case msg of
    ChangeWidth widthString ->
      case toInt widthString of
        Ok width ->
          ({ model
            | width = width
            , boards = initBoards width
            , index = 0
            }
          , Cmd.none)
        Err msg ->
          (model, Cmd.none)

    StartStop ->
      ({ model | started = not model.started }, Cmd.none)
      
    Tick time ->
      if model.started then
        ({ model | index = (model.index + 1) % (Array.length model.boards) }, Cmd.none)
      else
        (model, Cmd.none)


queenPuzzle rows cols =
  if rows <= 0 then
    [[]]
  else
    addQueen (rows - 1) cols


addQueen newRow cols =
  let
    prev = queenPuzzle newRow cols
  in
    List.concatMap
      (\solution ->
        (List.filterMap
          (\newCol ->
            if not (hasConflict newRow newCol solution) then
              Just (List.append solution [newCol])
            else
              Nothing
          )
          [0..(cols - 1)]
        )
      )
      prev


hasConflict newRow newCol solution =
  List.foldl
    (\i conflict ->
      let
        ithSolution =
          case Array.get i (Array.fromList solution) of
            Just value ->
              value
            Nothing ->
              -1
      in
        conflict ||
        (ithSolution == newCol) ||
        (ithSolution + i == newCol + newRow) ||
        (ithSolution - i == newCol - newRow)
    )
    False
    [0..(newRow - 1)]

