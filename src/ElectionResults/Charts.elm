module ElectionResults.Charts
    ( barChart
    ) where

import Html exposing (Html)
import Html.Attributes as Attr exposing (style, class)

-- Html bar chart
barChart : List (String, String, Float) -> (String -> String) -> Html
barChart data colour =
    let
        bar align label category width = Html.div
            [ style
                [ ("text-align", align)
                , ("font-size", "9.2px")
                , ("white-space", "nowrap")
                , ("padding", "2px")
                , ("flex", toString width)
                , ("background", colour category)
                , ("color", "white")
                ]
            ]
            [ Html.text label ]
        fiftyPercent side = Html.span
            [ style
                [ ("display", "inline-block")
                , ("position", "absolute")
                , ("left", "50%")
                , (side, "0px")
                , ("margin-left", "-1px")
                , ("border-left", "2px solid transparent")
                , ("border-right", "2px solid transparent")
                , ("border-"++side, "5px solid white")
                ]
            ]
            []
        firstBar = Maybe.withDefault [] <| Maybe.map (\(label, category, width) -> [bar "left" label category width]) <| List.head data
        restBars = Maybe.withDefault [] <| Maybe.map bars <| List.tail data
        bars data = case data of
           [] -> []
           [(label, category, width)] -> [bar "right" label category width]
           (label, category, width)::rest -> bar "center" label category width :: bars rest
    in
        Html.div
            [ style
                [ ("margin", "1px")
                , ("display", "flex")
                , ("position", "relative")
                ]
            ]
            <| [fiftyPercent "top", fiftyPercent "bottom"]
            ++ firstBar
            ++ restBars


