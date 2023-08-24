module Main exposing (main)

import Browser
import Html
    exposing
        ( Html
        , article
        , button
        , form
        , input
        , li
        , main_
        , nav
        , ol
        , pre
        , section
        , text
        , textarea
        )
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy)
import Knowledge.Graph
import Style


type Model
    = Import { input : String }
    | Browse
        { lines : List String
        , selected : Maybe String
        , graph : Knowledge.Graph.Model
        }


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init () =
    let
        example =
            """
kennistools
    gebruiken codekracht
    zoals het format
het format
    van de brontekst
de brontekst
    van dit diagram
    lijkt op RDF/Turtle
    lijkt op Markdown
dit diagram
    is een kennisgraaf
een kennisgraaf
    is een codesuperkracht
RDF/Turtle
    is een webstandaard
    is ook een codesuperkracht
Markdown
    is ook een codesuperkracht
"""
    in
    ( Browse
        { graph = Knowledge.Graph.init example
        , lines = String.lines example
        , selected = Nothing
        }
    , Cmd.none
    )


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "Kennisgraaf"
    , body =
        [ main_ (Style.withFont ++ Style.screen)
            (case model of
                Import _ ->
                    [ section (Style.pile ++ Style.panel)
                        [ textarea (onInput Input :: Style.panel ++ Style.input) []
                        , nav []
                            [ button (onClick Update :: Style.button ++ Style.withFont) [ text "Maak de kennisgraaf" ] ]
                        ]
                    ]

                Browse { graph, lines, selected } ->
                    [ section (Style.pile ++ Style.panel)
                        [ lines
                            |> List.map
                                (\l ->
                                    li
                                        (onClick (Selected l)
                                            :: (if Just l == selected then
                                                    [ style "background" "antiquewhite" ]

                                                else
                                                    []
                                               )
                                        )
                                        [ text l ]
                                )
                            |> ol [ style "list-style" "none" ]
                            |> List.singleton
                            |> pre [ style "overflow" "scroll" ]
                        , nav []
                            [ button (onClick New :: Style.button ++ Style.withFont) [ text "Voer een nieuwe graaf in" ] ]
                        ]
                    , article (style "overflow" "scroll" :: Style.output)
                        [ pre []
                            [ lazy Knowledge.Graph.view graph
                                |> Html.map GraphMsg
                            ]
                        ]
                    ]
            )
        ]
    }


type Msg
    = Input String
    | Update
    | New
    | Selected String
    | GraphMsg Knowledge.Graph.Msg


update msg model =
    case model of
        Import { input } ->
            case msg of
                Input text ->
                    ( Import { input = text }, Cmd.none )

                Update ->
                    if String.isEmpty input then
                        init ()

                    else
                        ( Browse
                            { graph = Knowledge.Graph.init input
                            , lines = String.lines input
                            , selected = Nothing
                            }
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        Browse { graph, lines, selected } ->
            case msg of
                New ->
                    ( Import { input = "" }, Cmd.none )

                Selected line ->
                    if String.trimLeft line == line then
                        ( Browse
                            { graph = Knowledge.Graph.goto line graph
                            , selected = Just line
                            , lines = lines
                            }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                GraphMsg m ->
                    ( Browse
                        { graph = Knowledge.Graph.update m graph
                        , selected = selected
                        , lines = lines
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


subscriptions model =
    Sub.none
