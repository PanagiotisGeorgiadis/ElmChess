module Components.ChessBoard.Update exposing (..)

import DataModels.Common exposing (PlayerType)


type alias Props p =
    { p
        | playerType : PlayerType
    }


type alias Model =
    {}


initialModel : Props p -> Model
initialModel props =
    {}


type Msg
    = NoOp


type ExtMsg
    = None


update : Props p -> Model -> Msg -> ( Model, Cmd Msg, ExtMsg )
update props model msg =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            , None
            )
