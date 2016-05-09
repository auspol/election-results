module ElectionResults.Data
    ( ElectionResult
    , electionResults
    , electionResult
    ) where

import Json.Decode as Json exposing ((:=))

-- JSON election result data
type alias ElectionResult =
    { electorate : String
    , incumbent : String
    , challenging : String
    , incumbentParty : String
    , challengingParty : String
    , enrolments : Int
    , incumbentVotes : Int
    , challengingVotes : Int
    }

electionResults : Json.Decoder (List ElectionResult)
electionResults = Json.list electionResult

electionResult : Json.Decoder ElectionResult
electionResult =
    Json.object8
        (\electorate incumbent challenging incumbentParty challengingParty enrolments incumbentVotes challengingVotes ->
            { electorate = electorate
            , incumbent = incumbent
            , challenging = challenging
            , incumbentParty = incumbentParty
            , challengingParty = challengingParty
            , enrolments = enrolments
            , incumbentVotes = incumbentVotes
            , challengingVotes = challengingVotes
            })
        ("electorate"        := Json.string)
        ("incumbent"         := Json.string)
        ("challenging"       := Json.string)
        ("incumbent_party"    := Json.string)
        ("challenging_party"  := Json.string)
        ("enrolments"        := Json.int)
        ("incumbent_votes"   := Json.int)
        ("challenging_votes" := Json.int)
