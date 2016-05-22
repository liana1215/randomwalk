module Walker where

import Html                 exposing (..)
import Graphics.Element     exposing (..)
import Graphics.Collage     exposing (..)
import List                 exposing (length, sum)
import Time                 exposing (Time, millisecond, second, every)
import Random               exposing (generate, initialSeed, int, float)
import Window               exposing (dimensions)
import Color                exposing (Color, black, charcoal, lightCharcoal, blue, lightRed, grey)
import String               exposing (toInt)
import Text                 exposing (Style, style, fromString)
import Signal               exposing (map, map2, foldp, merge)


--PORTS
port inputs : Signal UserInputs

type alias Step = 
    (Int, Int)


type alias Point = 
    {x: Int, y: Int}


type alias Model = 
    { point:   Point 
    , n:       Int
    , trials:  Int
    , i:       Int
    , counter: Int
    , test:    Bool
    , results: List Int
    , empiricalMean: Int
    }


type alias UserInputs = 
    { n:      Int
    , trials: Int
    , i:      Int
    }


type Action = NoOp 
            | StepMove Step 
            | UpdateInput UserInputs


seed0 = 
    initialSeed 31415

--MODEL
initModel : Model
initModel = 
    { point   = {x = 0, y = 0}
    , n       = 10
    , trials  = 100
    , i       = 5
    , counter = 0 
    , test    = False
    , results = [] 
    , empiricalMean = 0
    }

--STYLE
bodyStyle : (Int,Int) -> Style
bodyStyle (w,h) =
    { typeface = [ "serif","Times New Roman" ]
    , height   = Just (toFloat (w//25))
    , color    = lightCharcoal
    , bold     = True
    , italic   = False
    , line     = Nothing
    }

outputStyle : (Int,Int) -> Style
outputStyle (w,h) =
    { typeface = [ "serif","Times New Roman" ]
    , height   = Just (toFloat (w//20))
    , color    = blue
    , bold     = True
    , italic   = True
    , line     = Nothing
    }

resultStyle : (Int,Int) -> Style
resultStyle (w,h) =
    { typeface = [ "serif","Times New Roman" ]
    , height   = Just (toFloat (w//5))
    , color    = lightCharcoal
    , bold     = True
    , italic   = False
    , line     = Nothing
    }

--UPDATE
update : Action -> Model -> Model
update action model =
    case action of
        NoOp -> 
            model

        StepMove step ->
            let 
                point = 
                    model.point
                (dx,dy) = 
                    step
            in
                if model.test == False then
                    if point.x == 0 then 
                        if dx < 0 
                            then 
                                { model 
                                    | point = {x = point.x, y = point.y}
                                    , counter = model.counter + 1
                                }
                            else 
                                { model 
                                    | point = {x = point.x + dx, y = point.y}
                                    , counter = model.counter + 1
                                }

                    else if point.x == model.n then
                        if model.trials > 0 
                            then
                                let     
                                    newResults = 
                                        model.results ++ [model.counter]
                                    tempSum = 
                                        sum newResults
                                    mean = 
                                        tempSum // length newResults
                                in 
                                    { model 
                                        | point = {x = model.i, y = 0}
                                        , counter = 0
                                        , test = False
                                        , results = model.results ++[model.counter]
                                        , trials = model.trials - 1
                                        , empiricalMean = mean
                                    }
                            else    
                                { model 
                                    | test = True }
                    else 
                        { model 
                            | point = { x = point.x + dx, y = point.y}
                            , counter = model.counter+1
                        }
                else
                    model

        UpdateInput inputs ->
            { model 
                | point = {x = inputs.i, y = 0}
                , n = inputs.n
                , trials = inputs.trials
                , i = inputs.i
                , counter = 0
                , test = False
                , results = []
                , empiricalMean = 0
            }


genStep : Random.Seed -> (Step, Random.Seed)
genStep s = 
    let 
        (step, seed) = 
            generate (float  0 1) s
    in 
        if step > 0.5 
            then ((1,0), seed)
            else ((-1,0), seed)


clock : Signal Time
clock = 
    every millisecond


randomWalkSeed : Signal (Action, Random.Seed)
randomWalkSeed = 
    map (\(step, seed) -> (StepMove step,seed)) (foldp (\clock ((x,y), seed) -> genStep seed) ((0,0), seed0) clock)


randomWalk : Signal Action
randomWalk = 
    map fst randomWalkSeed


updateInputs : Signal Action
updateInputs =
    map UpdateInput inputs


actions : Signal Action
actions =
    merge updateInputs randomWalk

--VIEW
walker : Model -> Form
walker model= 
    if model.test == False 
        then group [ 
               filled black  <| circle (5)
             , filled lightRed <| circle (4)
             ]       
        else
            toForm (image 40 40 "http://liana1215.github.io/assets/img/daruma.png")


frame : (Int,Int) ->  Form
frame (w,h) =
    group [ 
        traced (dashed charcoal) (segment (0,0) (toFloat(w//3),0))
      , filled grey (rect (toFloat(w//3)) (toFloat(h//10)))
            |> move (toFloat(-w//6),0)
      , Graphics.Collage.text (style (bodyStyle (w,h)) (fromString "Counter"))
            |> move (toFloat(-w//6),toFloat(h//80))
    ]         


bar : (Int,Int) -> Int -> Color -> Element 
bar (w,h) n color =
    flow right [
        opacity 0.5 (collage n (h//20) [filled color (rect (toFloat n) (toFloat (h//20)))])
      , show n
    ]


barGraph : (Int,Int) -> Model -> Element 
barGraph (w,h) model =
    container w (h//10) midLeft (bar (w,h) model.empiricalMean lightRed)


empirical : (Int,Int) -> Model -> Element
empirical (w,h) model = 
    flow down [
        centered (style (outputStyle (w,h)) (fromString "Empirical"))
      , container (w//3) (h//4) middle (centered (style (resultStyle (w,h)) (fromString (toString model.empiricalMean))))
    ]


analytical : (Int,Int) -> Model -> Element
analytical (w,h) model = 
    let 
        analyticalMean = 
            model.n + model.n * model.n - model.i - model.i * model.i
    in
        flow down [
            centered (style (outputStyle (w,h)) (fromString "Analytical"))
          , container (w//3) (h//4) middle (centered (style (resultStyle (w,h))(fromString (toString analyticalMean))))
        ]


view : (Int,Int) -> Model -> Element
view (w,h) model = 
    let 
        point = 
            model.point
    in
        collage w h [ 
            toForm (empirical (w,h) model)
                |> move (toFloat (-w//6), toFloat (h//4))
          , toForm (analytical (w,h) model)
                |> move (toFloat (w//5), toFloat (h//4))
          , frame (w,h) 
                |> move (0, toFloat (h//10))
          , walker model
                |> move ((toFloat point.x),(toFloat point.y)+(toFloat(h//10)))
          , toForm (show model.counter)
                |> move (toFloat(-w//20), toFloat(h//10))
          , toForm (barGraph (w,h) model)
                |> move (toFloat (w//2) , toFloat(h//10))
        ]   


main : Signal Element
main = 
    map2 view dimensions (foldp update initModel actions)
    
