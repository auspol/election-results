module ElectionResults.Parties
    ( MajorParty(..)
    , partyType
    , partyElected
    , isElected
    , isUndecided
    , partyColour
    ) where

import String

import ElectionResults.Data exposing (..)

type MajorParty = Liberal | National | Greens | Labor | Other

-- Official party names to loose party categoriess
partyType : String -> MajorParty
partyType party =
    if String.contains "Liberal" party then
        Liberal
    else if String.contains "National" party then
        National
    else if String.contains "Green" party then
        Greens
    else if String.contains "Labor" party then
        Labor
    else
        Other

-- The party that was elected during an election
partyElected : ElectionResult -> Maybe MajorParty
partyElected election =
    if election.incumbent.elected then
        Just <| partyType election.incumbent.party
    else if election.challenging.elected then
        Just <| partyType election.incumbent.party
    else
        Nothing

-- Was a given party elected?
isElected : MajorParty -> ElectionResult -> Bool
isElected party result =
    case partyElected result of
        Just electedParty -> electedParty == party
        Nothing -> False

-- Is the election undecided?
isUndecided : ElectionResult -> Bool
isUndecided result =
       not result.incumbent.elected
    && not result.challenging.elected

-- Party colours
partyColour : MajorParty -> String
partyColour party =
    case party of
        Liberal  -> "#2222cc"
        National -> "#6666ee"
        Labor    -> "#cc2222"
        Greens   -> "#22cc22"
        Other    -> "#333"
