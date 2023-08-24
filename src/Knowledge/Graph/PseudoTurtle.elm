module Knowledge.Graph.PseudoTurtle exposing (fromString, toNodesEdgesLabels)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, spaces, succeed)


type alias Document =
    { context : Dict String String
    , triples : Triples
    , subject : Maybe String
    }


empty =
    { context = Dict.empty
    , triples = Dict.empty
    , subject = Nothing
    }


type alias Triples =
    Dict String (List ( String, String ))


toNodesEdgesLabels : Document -> { nodes : List String, edges : List ( Int, Int ), labels : Dict ( Int, Int ) String }
toNodesEdgesLabels document =
    let
        subjects =
            Dict.keys document.triples

        index =
            subjects
                |> List.indexedMap (\i n -> ( n, i ))
                |> Dict.fromList

        labeledEdgeList =
            document.triples
                |> Dict.foldl
                    (\subject props edges ->
                        case Dict.get subject index of
                            Nothing ->
                                edges

                            Just i ->
                                List.foldl
                                    (\( predicate, object ) e ->
                                        case Dict.get object index of
                                            Nothing ->
                                                e

                                            Just j ->
                                                ( ( i, j ), predicate ) :: e
                                    )
                                    edges
                                    props
                    )
                    []
    in
    { nodes = subjects
    , edges =
        labeledEdgeList
            |> List.map Tuple.first
    , labels =
        labeledEdgeList
            |> Dict.fromList
    }


fromString : String -> Document
fromString ttl =
    ttl
        |> String.lines
        |> extractSubjects
        |> extractProperties


extractSubjects : List String -> ( List String, Document )
extractSubjects lines =
    lines
        |> List.foldl extractSubject empty
        |> Tuple.pair lines


extractSubject : String -> Document -> Document
extractSubject line document =
    if isSubjectLine line then
        { document | triples = Dict.insert (String.trim line) [] document.triples }

    else
        document


isSubjectLine : String -> Bool
isSubjectLine line =
    -- no indentation means it is a subject
    not (String.isEmpty line) && String.trimLeft line == line


extractProperties : ( List String, Document ) -> Document
extractProperties ( lines, document ) =
    lines
        |> List.foldl extractProperty document


extractProperty : String -> Document -> Document
extractProperty line document =
    let
        parser =
            succeed (\indent source -> ( indent, String.dropLeft (indent - 1) source ))
                |. spaces
                |= Parser.getCol
                |= Parser.getSource

        maybeLineSpec =
            Parser.run parser line
                |> Result.toMaybe
    in
    case maybeLineSpec of
        Nothing ->
            document

        Just ( _, "" ) ->
            -- ignore empty lines
            document

        Just ( 1, source ) ->
            -- subjects were already inserted in the previous phase
            -- but we need to remember which subject we are dealing with
            { document | subject = Just (String.trim source) }

        Just ( indent, source ) ->
            let
                subjects =
                    Dict.keys document.triples

                match subject matchedSubject =
                    if String.endsWith subject source then
                        case matchedSubject of
                            Nothing ->
                                Just subject

                            Just s ->
                                if String.length subject > String.length s then
                                    Just subject

                                else
                                    matchedSubject

                    else
                        matchedSubject
            in
            case List.foldl match Nothing subjects of
                Nothing ->
                    case document.subject of
                        Nothing ->
                            document

                        Just subject ->
                            case source |> String.words |> List.reverse |> List.head of
                                Nothing ->
                                    document

                                Just object ->
                                    let
                                        predicate =
                                            source
                                                |> String.dropRight (String.length object)
                                                |> String.trimRight

                                        triples =
                                            case Dict.get object document.triples of
                                                Nothing ->
                                                    Dict.insert object [] document.triples

                                                _ ->
                                                    document.triples
                                    in
                                    { document | triples = addTriple ( subject, predicate, object ) triples }

                Just object ->
                    let
                        predicate =
                            source
                                |> String.dropRight (String.length object)
                                |> String.trimRight
                    in
                    case document.subject of
                        Nothing ->
                            document

                        Just subject ->
                            { document | triples = addTriple ( subject, predicate, object ) document.triples }


addTriple : ( String, String, String ) -> Triples -> Triples
addTriple ( subject, predicate, object ) triples =
    let
        properties =
            Dict.get subject triples
                |> Maybe.withDefault []
                |> List.append [ ( predicate, object ) ]
    in
    Dict.insert subject properties triples
