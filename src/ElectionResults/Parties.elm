module ElectionResults.Parties
    ( MajorParty(..)
    , partyType
    , partyColour
    ) where

import String

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

-- Party colours
partyColour : MajorParty -> String
partyColour party =
    case party of
        Liberal  -> "#2222cc"
        National -> "#6666ee"
        Labor    -> "#cc2222"
        Greens   -> "#22cc22"
        Other    -> "#ccc"
