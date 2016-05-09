module ElectionResults.API
    ( requestResult
    , fetchElectionResults
    , electionQuery
    ) where

import Http
import Task exposing (Task)
import String
import Json.Decode as Json

import ElectionResults.Data exposing (..)

-- URL for elections data API
apiUrl = "https://api.morph.io/kennib/australian_electoral_commission_federal_election_data/data.json"

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
fetchElectionResults : String -> String -> Task Http.Error (List ElectionResult)
fetchElectionResults key query =
    crossOriginGet electionResults
        <| apiUrl
        ++ queryString
            [ ("key", key)
            , ("query", query)
            ]

-- HTTP request across origins
crossOriginGet : Json.Decoder a -> String -> Task Http.Error a
crossOriginGet decoder url =
    Http.get decoder
    <| "https://crossorigin.me/" ++ url

-- SQL query for elections data
electionQuery : String -> String -> String
electionQuery event election =
 """select
  contest.name as electorate, contest.enrolment as enrolments,
  incumbent_candidate.name as incumbent,
  incumbent_party.name as incumbent_party,
  incumbent.votes as incumbent_votes,
  incumbent.elected as incumbent_elected,
  challenging_candidate.name as challenging,
  challenging_party.name as challenging_party,
  challenging.votes as challenging_votes,
  challenging.elected as challenging_elected
 from event
 join election
  on election.event_id = event.id
 join contest
  on contest.election_id = election.id
  and contest.event_id = event.id
 join two_candidate_preferred as incumbent
  on incumbent.contest_id = contest.id
  and incumbent.election_id = election.id
  and incumbent.event_id = event.id
  and incumbent.incumbent = 1
 join two_candidate_preferred as challenging
  on challenging.contest_id = contest.id
  and challenging.election_id = election.id
  and challenging.event_id = event.id
  and challenging.incumbent = 0
 join candidate as incumbent_candidate
  on incumbent_candidate.id = incumbent.candidate_id
 join candidate as challenging_candidate
  on challenging_candidate.id = challenging.candidate_id
 join party as incumbent_party
  on incumbent_party.id = incumbent.party_id
 join party as challenging_party
  on challenging_party.id = challenging.party_id
 where event.id = '""" ++ event ++ """'
  and election.id = '""" ++ election ++ """'
 group by contest.id
 order by contest.name"""
