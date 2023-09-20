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


type alias Model =
    { lines : List String
    , selected : Maybe String
    , graph : Knowledge.Graph.Model
    , mode : Mode
    }


type Mode
    = Import { input : String }
    | Browse


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
    ( { graph = Knowledge.Graph.init example
      , lines = String.lines example
      , selected = Nothing
      , mode = Browse
      }
    , Cmd.none
    )


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "Kennisgraaf"
    , body =
        [ main_ (Style.withFont ++ Style.screen)
            (case model.mode of
                Import { input } ->
                    [ section (Style.pile ++ Style.panel)
                        [ textarea (onInput Input :: Style.panel ++ Style.input) [ text input ]
                        , nav []
                            [ button (onClick Update :: Style.button ++ Style.withFont) [ text "Maak de kennisgraaf" ] ]
                        ]
                    ]

                Browse ->
                    [ section (Style.pile ++ Style.panel)
                        [ model.lines
                            |> List.map
                                (\l ->
                                    li
                                        (onClick (Selected l)
                                            :: (if Just l == model.selected then
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
                            [ lazy Knowledge.Graph.view model.graph
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
    case model.mode of
        Import { input } ->
            case msg of
                Input text ->
                    ( { model | mode = Import { input = text } }, Cmd.none )

                Update ->
                    if String.isEmpty input then
                        init ()

                    else
                        ( { model
                            | graph = Knowledge.Graph.init input
                            , lines = String.lines input
                            , selected = Nothing
                            , mode = Browse
                          }
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        Browse ->
            case msg of
                New ->
                    ( { model | mode = Import { input = String.join "\n" model.lines } }, Cmd.none )

                Selected line ->
                    if String.trimLeft line == line then
                        ( { model
                            | graph = Knowledge.Graph.goto line model.graph
                            , selected = Just line
                            , mode = Browse
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                GraphMsg m ->
                    ( { model
                        | graph = Knowledge.Graph.update m model.graph
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


subscriptions model =
    Sub.none
