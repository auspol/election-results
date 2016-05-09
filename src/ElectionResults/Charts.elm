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
                , ("font-size", "11px")
                , ("white-space", "nowrap")
                , ("padding", "5px")
                , ("flex", toString width)
                , ("background", colour category)
                , ("color", "white")
                ]
            ]
            [ Html.text label ]
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
                ]
            ]
            <| firstBar ++ restBars


