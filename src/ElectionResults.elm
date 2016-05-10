module ElectionResults where

import Http
import Json.Decode as Json
import Task exposing (Task)

import Html exposing (Html)
import Html.Attributes as Attr exposing (style, class)

import ElectionResults.Data exposing (..)
import ElectionResults.Parties exposing (..)
import ElectionResults.API exposing (..)
import ElectionResults.Charts exposing (barChart)

-- Show results
main = Signal.map view results.signal

-- Display the election results
view : Result String (List ElectionResult) -> Html
view result =
    Html.div []
    [ Html.node "meta" [Attr.name "viewport", Attr.content "width=device-width"] []
    , Html.node "link" [Attr.rel "stylesheet", Attr.href "/css/main.css"] []
    , case result of
        Err error -> Html.p [] [Html.text error]
        Ok results ->
            Html.div []
            [ electionResults "Coalition"
                (partyColour Liberal)
                (\result -> isElected Liberal result || isElected National result)
                results
            , electionResults "Labor"
                (partyColour Labor)
                (isElected Labor)
                results
            , electionResults "Greens"
                (partyColour Greens)
                (isElected Greens)
                results
            , electionResults "Other"
                (partyColour Other)
                (isElected Other)
                results
            , electionResults "Undecided"
                "black"
                isUndecided
                results
            ]
    ]

-- Results filtered by category
electionResults : String -> String -> (ElectionResult -> Bool) -> List ElectionResult -> Html
electionResults title color condition results =
    let
        filteredResults = List.filter condition results
    in
        case filteredResults of
            [] -> Html.text ""
            filteredResults ->
                Html.div []
                [ Html.h2
                    [ style
                        [ ("color", color)
                        ]
                    ]
                    [ Html.text title
                    , Html.text " ("
                    , Html.text <| toString <| List.length filteredResults
                    , Html.text ")"
                    ]
                , electionBarCharts filteredResults
                ]

-- Bar charts for each electorate
electionBarCharts : List ElectionResult -> Html
electionBarCharts electionResults =
    let
        fraction votes election = toFloat votes / toFloat (totalVotes election)
        totalVotes election = election.incumbent.votes + election.challenging.votes
        uncountedVotes election = election.enrolments - totalVotes election

        result candidate election = [ (candidate.name, candidate.party, fraction candidate.votes election) ]
        uncounted election = [ ("", "", fraction (uncountedVotes election) election) ]
    in
        Html.div
            [ style
                [ ("display", "flex")
                , ("flex-wrap", "wrap")
                ]
            ]
        <| List.map
            (\election -> Html.div
                [ class "election-result"
                ]
                [ Html.p
                    [ style
                        [ ("text-align", "center")
                        , ("font-size", "10px")
                        , ("margin", "0px")
                        , ("display", "none")
                        ]
                    ]
                    [ Html.text election.electorate ]
                , barChart
                    (result election.incumbent election ++ result election.challenging election)
                    (partyType >> partyColour)
                ]
            )
            electionResults

-- Task for fetching 2013 results
fetchElection2013Results : Task Http.Error (List ElectionResult)
fetchElection2013Results =
    fetchElectionResults "ctggtDcxn71/wB4G29pZ"
    <| electionQuery "17496" "H"

-- API request results
results : Signal.Mailbox (Result String (List ElectionResult))
results =
    Signal.mailbox (Err "Requesting data...")

-- Make  API request
port request : Task () ()
port request =
    requestResult fetchElection2013Results
    `Task.andThen` Signal.send results.address
