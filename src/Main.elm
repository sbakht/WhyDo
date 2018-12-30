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
import Maybe exposing (withDefault)
import Random
import String exposing (fromInt, toInt)



---- MODEL ----


type alias Model =
    { tasks : TaskList, adding : String, lastID : Int, random : List Int }


init : ( Model, Cmd Msg )
init =
    ( { tasks = Dict.empty, adding = "", lastID = 0, random = [] }, Cmd.none )


addTask : (Task, Int) -> TaskList -> TaskList
addTask (task, depOf) list =
    Dict.update depOf ( Maybe.map (\x -> Task x.id x.text x.delay task.id) ) <| Dict.insert task.id task list


type alias Task =
    { id : Int, text : String, delay : Int, dependency: Int }


type alias TaskList =
    Dict Int Task

mkTask : Int -> String -> Int -> (Task, Int)
mkTask id text delay = case String.split "#" text of
    [words, dep] ->
        (Task id words delay 0, (withDefault 0 <| toInt dep))
    _ ->
        (Task id text delay 0, 0)

---- UPDATE ----


type Msg
    = OnTaskKeyPress String
    | AddTask
    | CompleteTask Int
    | Randomize (List Int)
    | GoRandom

withoutDependency = Dict.filter (\_ task -> task.dependency == 0)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTaskKeyPress string ->
            ( { model | adding = string }, Cmd.none )

        AddTask ->
            if model.adding == "" then
                (model, Cmd.none)
            else
                ( { model | tasks = addTask (mkTask (model.lastID + 1) model.adding 0 ) model.tasks, lastID = model.lastID + 1, adding = "" }, Cmd.none )

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
                    Task task.id task.text (task.delay + 1) task.dependency
            in
            ( { model | tasks = tasks }, Random.generate Randomize <| Random.list 50 <| Random.int 0 <| Dict.size tasks - 1 )

        GoRandom ->
            ( model, Random.generate Randomize <| Random.list 50 <| Random.int 0 <| (\x -> Dict.size x - 1) <| withoutDependency model.tasks )

        Randomize arr ->
            let
                random =
                    getRandomIds <| Array.fromList <| Dict.keys <| withoutDependency model.tasks

                getRandomIds ids =
                    catMaybes <| List.map (\i -> Array.get i ids) (take 3 <| Debug.log "arr" <| unique arr)
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
    column [ centerX, Font.size 12, paddingXY 0 20 ] (Dict.foldl ghostView [] list)


taskView : Int -> Task -> List (Element Msg) -> List (Element Msg)
taskView _ task html =
    el [ onClick (CompleteTask task.id) ] (text <| fromInt task.delay ++ " " ++ task.text) :: html

ghostView : Int -> Task -> List (Element Msg) -> List (Element Msg)
ghostView _ task html =
    el [ onClick (CompleteTask task.id) ] (text <| "ID: " ++ fromInt task.id ++ " " ++ fromInt task.delay ++ " " ++ task.text) :: html


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
