module Utils.Actions exposing (..)

import Task


simpleAction : msg -> Cmd msg
simpleAction msg =
    Task.perform (\_ -> msg) (Task.succeed ())
