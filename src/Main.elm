module Main exposing (main)

import Browser
import Html as H exposing (Html, button, div, input, label, node, p, span, text)
import Html.Attributes as A exposing (alt, class, name, title)
import Html.Events as Evt
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE


type alias Model =
    { lesionMapUrl : String
    , first : String
    }


init : String -> ( Model, Cmd Msg )
init lesionMapUrl =
    ( Model lesionMapUrl "first", Cmd.none )


type alias Area =
    String


type Msg
    = ClickArea Area


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickArea area ->
            ( { model | first = area }, Cmd.none )


textField : String -> String -> Html Msg
textField label content =
    p [ class "py-2" ]
        [ span [ class "mr-4 w-48 inline-block text-right" ] [ text label ]
        , span [ A.type_ "text", class "w-32" ] [ text content ]
        ]


inputField : String -> String -> Html Msg
inputField label name =
    p [ class "py-2" ]
        [ H.label []
            [ span [ class "mr-4 w-48 inline-block text-right" ] [ text label ]
            , input [ A.type_ "text", A.name name, class "border w-32" ] []
            ]
        ]


choiceField : String -> List (Html Msg) -> Html Msg
choiceField label options =
    p [ class "py-2" ]
        [ H.label [ class "mr-4" ]
            [ span [ class "mr-4 w-48 inline-block text-right" ] [ text label ]
            , H.select [ class "border w-32" ]
                options
            ]
        ]


lesionForm : Model -> Html Msg
lesionForm model =
    div []
        [ div [ class "px-auto" ]
            [ div [ class "relative px-auto", A.style "width" "448", A.style "height" "536px" ]
                [ H.img [ A.src model.lesionMapUrl, A.usemap "#imageMap", class "w-full absolute" ] []
                , S.svg [ SA.viewBox "0 0 652 918", SA.class "absolute" ] lesionMap
                ]
            ]
        , div [ class "px-14 text-left" ]
            [ textField "Lesion location" model.first
            , inputField "Lesion size" "lesion-size"
            , inputField "ADC" "adc"
            , choiceField "PIRADS 2.1 score"
                [ H.option [] [ text "2" ]
                , H.option [] [ text "3" ]
                , H.option [] [ text "4" ]
                , H.option [] [ text "5" ]
                ]
            , choiceField "PIRADS 2.1 upgraded?"
                [ H.option [] [ text "Yes" ]
                , H.option [] [ text "Yes" ]
                , H.option [] [ text "No" ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    H.form [ class "container mx-auto max-w-md" ]
        [ H.h1 [ class "font-serif text-3xl text-center pb-10" ] [ text "R-PEDAL MRI Data Entry" ]
        , H.section [ class "text-center pb-16" ]
            [ div [ class "px-14 text-left" ]
                [ inputField "Patient ID" "patient-id"
                , inputField "MRI date" "mri-date"
                , inputField "PSA level" "psa-level"
                , choiceField "Is an index lesion present?"
                    [ H.option [] [ text "Yes" ]
                    , H.option [] [ text "No" ]
                    ]
                ]
            ]
        , H.section [ class "text-center border m-6" ]
            [ H.h2 [ class "font-serif text-2xl text-center pt-4" ] [ text "Index lesion" ]
            , lesionForm model
            ]
        , H.section [ class "text-center border m-6" ]
            [ H.h2 [ class "font-serif text-2xl text-center pt-4" ] [ text "Secondary lesion (optional)" ]
            , lesionForm model
            ]
        , H.button [ class "block border m-4 mx-auto" ] [ text "Submit!" ]
        ]


type alias Coords =
    List Int


type alias AreaName =
    String


type alias Shape =
    String


lesionNode : AreaName -> Coords -> Shape -> Html Msg
lesionNode name coords shape =
    let
        onClick =
            \n -> SE.onClick <| ClickArea n

        hoverColor =
            "text-transparent hover:text-teal-200 fill-current"
    in
    case shape of
        "poly" ->
            let
                coordsString =
                    String.join ", " <| List.map (\n -> String.fromInt n) coords
            in
            S.polygon
                [ SA.points coordsString
                , onClick name
                , SA.class hoverColor
                ]
                []

        "rect" ->
            case coords of
                [ x1, y1, x2, y2 ] ->
                    let
                        x =
                            String.fromInt x2

                        y =
                            String.fromInt y2

                        w =
                            String.fromInt <| abs <| x2 - x1

                        h =
                            String.fromInt <| abs <| y2 - y1
                    in
                    S.rect
                        [ SA.x x
                        , SA.y y
                        , SA.width w
                        , SA.height h
                        , onClick name
                        , SA.class hoverColor
                        ]
                        []

                _ ->
                    S.text ""

        "circle" ->
            case coords of
                [ x, y, r ] ->
                    let
                        cx =
                            String.fromInt x

                        cy =
                            String.fromInt y

                        cr =
                            String.fromInt r
                    in
                    S.circle
                        [ SA.cx cx
                        , SA.cy cy
                        , SA.r cr
                        , onClick name
                        , SA.class hoverColor
                        ]
                        []

                _ ->
                    S.text ""

        _ ->
            S.text ""


lesionMap : List (Html Msg)
lesionMap =
    [ lesionNode
        "r_base_afs"
        [ 515, 235, 515, 222, 515, 200, 493, 201, 458, 216, 437, 239, 451, 248, 470, 218, 498, 215 ]
        "poly"
    , lesionNode
        "l_base_afs"
        [ 522, 233, 521, 199, 550, 202, 580, 213, 598, 233, 588, 255, 570, 222, 542, 214, 532, 223 ]
        "poly"
    , lesionNode
        "r_base_pza"
        [ 436, 239, 426, 266, 421, 295, 460, 281, 451, 259, 450, 248 ]
        "poly"
    , lesionNode
        "l_base_pza"
        [ 589, 253, 600, 238, 614, 265, 620, 294, 582, 283 ]
        "poly"
    , lesionNode
        "r_base_tza"
        [ 453, 249, 471, 220, 496, 217, 514, 237, 516, 281, 460, 281 ]
        "poly"
    , lesionNode
        "l_base_tza"
        [ 523, 235, 542, 216, 567, 223, 585, 255, 580, 280, 522, 281 ]
        "poly"
    , lesionNode
        "r_base_tzp"
        [ 515, 298, 469, 286 ]
        "rect"
    , lesionNode
        "l_base_tzp"
        [ 567, 298, 521, 286 ]
        "rect"
    , lesionNode
        "r_base_pzpl"
        [ 459, 286, 478, 344, 452, 338, 430, 321, 423, 296 ]
        "poly"
    , lesionNode
        "l_base_pzpl"
        [ 582, 286, 616, 298, 611, 318, 590, 335, 563, 344 ]
        "poly"
    , lesionNode
        "r_base_pzpm"
        [ 474, 318, 483, 344, 512, 346, 489, 335 ]
        "poly"
    , lesionNode
        "l_base_pzpm"
        [ 567, 318, 557, 345, 525, 346, 549, 336 ]
        "poly"
    , lesionNode
        "r_base_cz"
        [ 470, 299, 478, 317, 489, 330, 516, 344, 516, 300 ]
        "poly"
    , lesionNode
        "l_base_cz"
        [ 522, 301, 571, 301, 566, 315, 546, 334, 522, 344 ]
        "poly"
    , lesionNode
        "r_sv"
        [ 504, 112, 484, 110, 452, 115, 429, 130, 414, 151, 418, 172, 443, 174, 474, 153, 504, 127 ]
        "poly"
    , lesionNode
        "l_sv"
        [ 524, 113, 525, 123, 548, 147, 580, 160, 597, 170, 621, 163, 616, 138, 592, 119, 564, 110 ]
        "poly"
    , lesionNode
        "r_mid_afs"
        [ 517, 394, 517, 433, 510, 416, 497, 410, 478, 416, 471, 409, 492, 397 ]
        "poly"
    , lesionNode
        "l_mid_afs"
        [ 521, 394, 520, 430, 530, 415, 545, 409, 559, 415, 567, 410, 544, 399 ]
        "poly"
    , lesionNode
        "r_mid_tza"
        [ 513, 467, 515, 440, 506, 417, 496, 413, 476, 418, 462, 441, 458, 467 ]
        "poly"
    , lesionNode
        "l_mid_tza"
        [ 579, 467, 576, 442, 566, 425, 545, 412, 533, 417, 522, 437, 525, 467 ]
        "poly"
    , lesionNode
        "r_mid_pza"
        [ 454, 468, 456, 440, 474, 417, 467, 412, 444, 437, 432, 469 ]
        "poly"
    , lesionNode
        "l_mid_pza"
        [ 580, 467, 603, 467, 592, 436, 568, 412, 562, 417, 578, 441 ]
        "poly"
    , lesionNode
        "r_mid_pzpl"
        [ 455, 473, 432, 473, 437, 505, 453, 525, 475, 533 ]
        "poly"
    , lesionNode
        "l_mid_pzpl"
        [ 582, 474, 603, 474, 599, 499, 588, 519, 561, 532 ]
        "poly"
    , lesionNode
        "r_mid_tzp"
        [ 461, 473, 508, 473, 500, 488, 483, 495, 467, 488 ]
        "poly"
    , lesionNode
        "l_mid_tzp"
        [ 526, 473, 574, 472, 571, 487, 554, 497, 535, 485 ]
        "poly"
    , lesionNode
        "r_mid_pzpm"
        [ 468, 494, 482, 497, 507, 489, 512, 497, 514, 535, 494, 536, 478, 532 ]
        "poly"
    , lesionNode
        "l_mid_pzpm"
        [ 569, 492, 553, 500, 534, 489, 522, 497, 521, 535, 539, 538, 559, 532 ]
        "poly"
    , lesionNode
        "apex_r-afs"
        [ 516, 581, 516, 619, 506, 606, 497, 608, 495, 596, 482, 591 ]
        "poly"
    , lesionNode
        "apex_l_afs"
        [ 520, 581, 520, 619, 528, 606, 541, 611, 541, 597, 551, 590 ]
        "poly"
    , lesionNode
        "apex_r_pza"
        [ 456, 632, 465, 609, 481, 594, 492, 599, 492, 608, 486, 621 ]
        "poly"
    , lesionNode
        "apex_l_pza"
        [ 581, 631, 552, 623, 544, 608, 545, 600, 555, 596, 570, 610 ]
        "poly"
    , lesionNode
        "apex_r_tza"
        [ 489, 621, 497, 610, 510, 611, 515, 621 ]
        "poly"
    , lesionNode
        "apex_l_tza"
        [ 522, 621, 546, 621, 541, 611, 529, 612 ]
        "poly"
    , lesionNode
        "apex_r_pzpl"
        [ 454, 636, 458, 670, 471, 690, 487, 694, 467, 631 ]
        "poly"
    , lesionNode
        "apex_l_pzpl"
        [ 570, 633, 584, 637, 581, 666, 570, 688, 551, 692 ]
        "poly"
    , lesionNode
        "apex_r_pzpm"
        [ 482, 627, 473, 631, 492, 693, 515, 687, 515, 660, 491, 658, 483, 644 ]
        "poly"
    , lesionNode
        "apex_l_pzpm"
        [ 554, 628, 566, 632, 545, 691, 522, 687, 523, 660, 550, 657, 554, 645 ]
        "poly"
    , lesionNode
        "apex_r_tzp"
        [ 489, 626, 487, 640, 494, 653, 508, 654, 515, 639, 515, 626 ]
        "poly"
    , lesionNode
        "apex_l_tzp"
        [ 550, 628, 550, 641, 545, 654, 526, 656, 521, 639, 521, 628 ]
        "poly"
    , lesionNode
        "urethra"
        [ 520, 737, 11 ]
        "circle"
    ]



---- PROGRAM ----


main : Program String Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
