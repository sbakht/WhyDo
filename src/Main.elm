port module Main exposing (Model, Msg(..), Task, TaskList, addTask, goView, init, inputView, main, randomView, submitView, taskView, tasksView, update, view)

import Array
import Browser
import Dict exposing (Dict)
import Element exposing (Attr, Element, alignRight, centerX, column, el, fill, fillPortion, height, html, htmlAttribute, link, maximum, mouseOver, none, padding, paddingXY, paragraph, pointer, rgb, rgb255, row, scrollbarY, spacing, spacingXY, text, width)
import Element.Background as Background
import Element.Events as Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (labelHidden, labelLeft)
import Exts.Maybe exposing (catMaybes)
import Html exposing (Html)
import List exposing (take)
import List.Extra exposing (unique)
import Random
import String exposing (fromInt)



---- MODEL ----


type alias Model =
    { tasks : TaskList, adding : String, lastID : Int, random : List Int }


init : ( Model, Cmd Msg )
init =
    ( { tasks = Dict.empty, adding = "", lastID = 1, random = [] }, Cmd.none )


addTask : Task -> TaskList -> TaskList
addTask task list =
    Dict.insert task.id task list


type alias Task =
    { id : Int, text : String, delay : Int }


type alias TaskList =
    Dict Int Task



---- UPDATE ----


type Msg
    = OnTaskKeyPress String
    | AddTask
    | CompleteTask Int
    | Randomize (List Int)
    | GoRandom


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTaskKeyPress string ->
            ( { model | adding = string }, Cmd.none )

        AddTask ->
            ( { model | tasks = addTask (Task (model.lastID + 1) model.adding 0) model.tasks, lastID = model.lastID + 1, adding = "" }, Cmd.none )

        CompleteTask id ->
            let
                tasks =
                    Dict.map
                        (\k task ->
                            if List.member k model.random then
                                procrastinate k task

                            else
                                task
                        )
                    <|
                        Dict.remove id model.tasks

                procrastinate : Int -> Task -> Task
                procrastinate _ task =
                    Task task.id task.text (task.delay + 1)
            in
            ( { model | tasks = tasks }, Random.generate Randomize <| Random.list 3 <| Random.int 0 <| Dict.size tasks - 1 )

        GoRandom ->
            ( model, Random.generate Randomize <| Random.list 10 <| Random.int 0 <| Dict.size model.tasks - 1 )

        Randomize arr ->
            let
                random =
                    getRandomIds <| Debug.log "ids" <| Array.fromList <| Dict.keys model.tasks

                getRandomIds ids =
                    catMaybes <| List.map (\i -> Array.get i ids) (unique arr)
            in
            ( { model | random = random }, Cmd.none )



---- VIEW ----
--view : Model -> Browser.Document Msg


view : Model -> Html Msg
view model =
    Element.layout [] <| column [ centerX ] [ inputView model.adding, submitView, goView, randomView model.random model.tasks, tasksView model.tasks ]


inputView : String -> Element Msg
inputView value =
    Input.text []
        { onChange = OnTaskKeyPress
        , text = value
        , placeholder = Nothing
        , label = labelHidden "Input new todo"
        }


submitView : Element Msg
submitView =
    Input.button []
        { onPress = Just AddTask
        , label = text "Add Todo"
        }


goView : Element Msg
goView =
    Input.button []
        { onPress = Just GoRandom
        , label = text "Randomize"
        }


randomView : List Int -> TaskList -> Element Msg
randomView ids list =
    column [ centerX, paddingXY 0 20 ] (Dict.foldl taskView [] <| Dict.filter (\id _ -> List.member id ids) list)


tasksView : TaskList -> Element Msg
tasksView list =
    column [ centerX, Font.size 12, paddingXY 0 20 ] (Dict.foldl taskView [] list)


taskView : Int -> Task -> List (Element Msg) -> List (Element Msg)
taskView _ task html =
    el [ onClick (CompleteTask task.id) ] (text <| fromInt task.delay ++ " " ++ task.text) :: html



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
