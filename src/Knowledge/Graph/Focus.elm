module Knowledge.Graph.Focus exposing (Focus, Target(..))


type alias Focus =
    List Target


type Target
    = Node Int
    | Edge { from : Int, to : Int }
