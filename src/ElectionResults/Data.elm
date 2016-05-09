module ElectionResults.Data
    ( ElectionResult
    , Candidate
    , electionResults
    , electionResult
    , candidateResult
    ) where

import Json.Decode as Json exposing ((:=))

-- Election Result data types
type alias ElectionResult =
    { electorate : String
    , incumbent : Candidate
    , challenging : Candidate
    }

type alias Candidate =
    { name : String
    , party : String
    , incumbent : Bool
    , elected : Bool
    , votes : Int
    }

-- JSON election result data
electionResults : Json.Decoder (List ElectionResult)
electionResults = Json.list electionResult

electionResult : Json.Decoder ElectionResult
electionResult =
    Json.object3
        (\electorate incumbent challenging ->
            { electorate = electorate
            , incumbent = incumbent
            , challenging = challenging
            })
        ("electorate"        := Json.string)
        (candidateResult "incumbent")
        (candidateResult "challenging")

candidateResult : String -> Json.Decoder Candidate
candidateResult kind =
    Json.object4
        (\name party elected votes ->
            { name = name
            , party = party
            , incumbent = kind == "incumbent"
            , elected = elected
            , votes = votes
            })
    ((kind)      := Json.string)
    ((kind++"_party")     := Json.string)
    ((kind++"_elected")   := Json.bool)
    ((kind++"_votes")     := Json.int)
