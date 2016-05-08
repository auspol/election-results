module ElectionResults where

import String
import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (Task)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Graphics.Element exposing (show)


-- URL for elections data API
apiUrl = "https://api.morph.io/kennib/australian_electoral_commission_federal_election_data/data.json"

-- Show results
main = Signal.map view results.signal


-- Display the election results
view : Result String (List ElectionResult) -> Html
view result =
    case result of
        Err error -> Html.div [] [ Html.text error ]
        Ok electionResults -> electionBarCharts electionResults

-- HTML Bar charts
electionBarCharts : List ElectionResult -> Html
electionBarCharts electionResults =
    let
        fraction votes election = toFloat (votes election) / toFloat (totalVotes election)
        totalVotes election = election.incumbentVotes + election.challengingVotes
        uncountedVotes election = election.enrolments - totalVotes election

        incumbent election = [ (election.incumbent, fraction .incumbentVotes election) ]
        challenging election = [ (election.challenging, fraction .challengingVotes election) ]
        uncounted election = [ ("", fraction uncountedVotes election) ]
    in
        Html.div []
        <| List.map
            (\election -> Html.div []
                [ Html.p [] [ Html.text election.electorate ]
                , barChart <| incumbent election ++ challenging election
                ]
            )
            electionResults

barChart : List (String, Float) -> Html
barChart data =
    let
        bar align label width = Html.div
            [ style
                [ ("text-align", align)
                , ("height", "40px")
                , ("flex", toString width)
                , ("border", "1px solid #333")
                ]
            ]
            [ Html.text label ]
        firstBar = Maybe.withDefault [] <| Maybe.map (\first -> [(uncurry <| bar "left") first]) <| List.head data
        restBars = Maybe.withDefault [] <| Maybe.map bars <| List.tail data
        bars data = case data of
           [] -> []
           [(label, width)] -> [bar "right" label width]
           (label,width)::rest -> bar "center" label width :: bars rest
    in
        Html.div
            [ style
                [ ("margin", "5px 10px 20px 10px")
                , ("display", "flex")
                ]
            ]
            <| firstBar ++ restBars

-- Task for fetching 2013 results
fetchElection2013Results : Task Http.Error (List ElectionResult)
fetchElection2013Results =
    fetchElectionResults
    <| queryString
        [ ("key", "ctggtDcxn71/wB4G29pZ")
        , ("query", electionQuery "17496" "H")
        ]

-- API request results
results : Signal.Mailbox (Result String (List ElectionResult))
results =
    Signal.mailbox (Err "Requesting data...")

-- Make  API request
port request : Task () ()
port request =
    requestResult fetchElection2013Results
    `Task.andThen` Signal.send results.address

-- Turn HTTP requests into results
requestResult : Task Http.Error a -> Task () (Result String a)
requestResult = Task.mapError toString >> Task.toResult

-- HTTP URL query string
queryString : List (String, String) -> String
queryString data =
    (++) "?"
    <| String.join "&"
    <| List.map (\(key, value) -> key ++ "=" ++ value)
    <| data

-- Query the elections API
fetchElectionResults : String -> Task Http.Error (List ElectionResult)
fetchElectionResults query =
    crossOriginGet electionResults
        <| apiUrl ++ query

-- HTTP request across origins
crossOriginGet : Json.Decoder a -> String -> Task Http.Error a
crossOriginGet decoder url =
    Http.get decoder
    <| "https://crossorigin.me/" ++ url

-- SQL query for elections data
electionQuery : String -> String -> String
electionQuery event election =
    """select contest.name as electorate, contest.enrolment as enrolments,
        incumbent_candidate.name as incumbent, incumbent.votes as incumbent_votes,
        challenging_candidate.name as challenging, challenging.votes as challenging_votes
    from event
    join election
        on election.event_id = event.id
    join contest
        on contest.election_id = election.id
        and contest.event_id = event.id
    join two_candidate_preferred as incumbent
        on incumbent.contest_id = contest.id
        and incumbent.incumbent = 1
    join two_candidate_preferred as challenging
        on challenging.contest_id = contest.id
        and challenging.incumbent = 0
    join candidate as incumbent_candidate
        on incumbent_candidate.id = incumbent.candidate_id
    join candidate as challenging_candidate
        on challenging_candidate.id = challenging.candidate_id
    where event.id = '""" ++ event ++ """'
        and election.id = '""" ++ election ++ """'
    group by contest.id
    order by contest.name
    """

-- JSON election result data
type alias ElectionResult =
    { electorate : String
    , incumbent : String
    , challenging : String
    , enrolments : Int
    , incumbentVotes : Int
    , challengingVotes : Int
    }

electionResults : Json.Decoder (List ElectionResult)
electionResults = Json.list electionResult

electionResult : Json.Decoder ElectionResult
electionResult =
    Json.object6
        (\electorate incumbent challenging enrolments incumbentVotes challengingVotes ->
            { electorate = electorate
            , incumbent = incumbent
            , challenging = challenging
            , enrolments = enrolments
            , incumbentVotes = incumbentVotes
            , challengingVotes = challengingVotes
            })
        ("electorate"        := Json.string)
        ("incumbent"         := Json.string)
        ("challenging"       := Json.string)
        ("enrolments"        := Json.int)
        ("incumbent_votes"   := Json.int)
        ("challenging_votes" := Json.int)
