module Knowledge.Graph exposing (Model, Msg, goto, init, update, view)

import Color
import Dagre.Attributes as DA
import Dict exposing (Dict)
import Graph as G
import Html exposing (Html)
import Knowledge.Graph.Drawer exposing (svgDrawEdge, svgDrawNode)
import Knowledge.Graph.Focus exposing (Focus, Target(..))
import Knowledge.Graph.PseudoTurtle
import Knowledge.Graph.Render as R
import Render.StandardDrawers.Attributes as RSDA
import Render.StandardDrawers.Types as RSDT
import TypedSvg as Svg
import TypedSvg.Core exposing (text)


type alias Model =
    { selection : Focus
    , graph : G.Graph String ()
    , labels : Dict ( Int, Int ) String
    }


type Msg
    = SelectEdge ( Int, Int )
    | SelectNode Int


init : String -> Model
init source =
    let
        { nodes, edges, labels } =
            source
                |> Knowledge.Graph.PseudoTurtle.fromString
                |> Knowledge.Graph.PseudoTurtle.toNodesEdgesLabels

        graph =
            G.fromNodeLabelsAndEdgePairs nodes edges
    in
    { selection = [], graph = graph, labels = labels }


goto : String -> Model -> Model
goto line model =
    let
        selection =
            G.fold
                (\n sel ->
                    if n.node.label == line then
                        [ Node n.node.id ]

                    else
                        sel
                )
                model.selection
                model.graph
    in
    { model | selection = selection }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectNode v ->
            case model.selection of
                [] ->
                    { model | selection = [ Node v ] }

                (Node n) :: _ ->
                    if v == n then
                        -- let
                        --     incoming =
                        --         G.alongIncomingEdges (G.get n model.graph)
                        --             |> List.map .id
                        -- in
                        -- { model | selection = incoming }
                        { model | selection = [] }

                    else
                        { model | selection = [ Node v ] }

                _ ->
                    { model | selection = [] }

        SelectEdge ( from, to ) ->
            { model | selection = [ Edge { from = from, to = to } ] }


view : Model -> Html Msg
view { selection, graph, labels } =
    let
        direction =
            DA.TB
    in
    Html.div []
        [ R.draw
            selection
            [ DA.rankDir direction
            , DA.nodeSep 200
            , DA.rankSep 200
            ]
            [ R.nodeDrawer
                (\na ->
                    svgDrawNode
                        [ RSDA.onClick (\n -> SelectNode n.id)
                        , RSDA.label (\n -> n.label)
                        , RSDA.shape (\n -> RSDT.RoundedBox 5)
                        ]
                        { na | width = toFloat <| 10 + 10 * String.length na.node.label }
                )
            , R.edgeDrawer
                (svgDrawEdge
                    [ RSDA.arrowHead RSDT.Vee
                    , RSDA.onClick (\e -> SelectEdge ( e.from, e.to ))
                    , RSDA.strokeWidth (\_ -> 2)
                    , RSDA.label
                        (\e ->
                            Dict.get ( e.from, e.to ) labels
                                |> Maybe.withDefault ""
                        )
                    , RSDA.orientLabelAlongEdge True
                    ]
                )
            , R.style "max-height: 100vh; width: 100%"
            ]
            graph
        ]
