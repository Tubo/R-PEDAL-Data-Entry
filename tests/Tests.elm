module Tests exposing (all)

import Expect
import Json.Encode as Encode
import Json.Encode.Extra as EX
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Encoding of the post request"
        [ test "Encode lesion" <|
            \_ ->
                Expect.equal
                    (Encode.encode 0 ((Encode.string << .name) { name = "Hello" }))
                    "\"Hello\""
        ]
