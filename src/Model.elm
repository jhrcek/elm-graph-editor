module Model exposing (init, update, Model, Msg(..), EditState(..))

import Graph as G
import Platform.Cmd as Cmd
import String exposing (fromChar)


type alias Gr =
    G.Graph (G.Node String) (G.Edge String)


type alias Model =
    { graph : Gr
    , nextNodeId : Int
    , nextEdgeId : Int
    , editState : EditState
    , charBuffer : String
    , invalidChar : Maybe Char
    }


type EditState
    = Start
    | AddNode
      -- The following 3 constitute adding edge
    | SetFrom
    | SetTo
    | SetLabel
      -- deleting
    | DelNode
    | DelEdge


type Msg
    = ChangeState EditState
    | AddChar Char
    | AddDigit Int
    | InvalidChar Char
    | ConfirmEdit
    | CancelEdit


init =
    ( Model G.empty 0 0 Start "" Nothing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeState st ->
            ( { model | editState = st, invalidChar = Nothing }, Cmd.none )

        AddChar chr ->
            ( { model | charBuffer = model.charBuffer ++ fromChar chr, invalidChar = Nothing }, Cmd.none )

        AddDigit dig ->
            ( { model | invalidChar = Nothing }, Cmd.none )

        InvalidChar chr ->
            ( { model | invalidChar = Just chr }, Cmd.none )

        ConfirmEdit ->
            ( { model | editState = Start, charBuffer = "", invalidChar = Nothing }, Cmd.none )

        CancelEdit ->
            ( { model | editState = Start, charBuffer = "", invalidChar = Nothing }, Cmd.none )
