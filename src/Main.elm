module Main exposing (main)

import Browser
import Html as H exposing (Html, div, input, label, p, span, text)
import Html.Attributes as A exposing (alt, class, name, title)
import Html.Events as Evt
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Encode.Extra as EX
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE


type alias Model =
    { lesionMapUrl : String
    , patientId : String
    , mriDate : String
    , psaLevel : String
    , first : Maybe LesionData
    , second : Maybe LesionData
    , ece : String
    , svi : String
    , comments : String
    , status : Status
    }


type Status
    = Incomplete
    | Ready
    | Sending
    | Success
    | Error String


type Field
    = PatientId
    | MriDate
    | PsaLevel
    | ECE
    | SVI
    | Comments
    | LesionSize LesionNumber
    | ADC LesionNumber
    | Score LesionNumber
    | Upgraded LesionNumber


type alias Area =
    String


type LesionNumber
    = First
    | Second


type alias Location =
    { name : String
    , label : String
    }


type alias LesionData =
    { name : LesionNumber
    , location : ( Maybe Location, Maybe Location )
    , size : String
    , adc : String
    , score : String
    , upgraded : String
    }


initLesionData : LesionNumber -> LesionData
initLesionData n =
    { name = n
    , location = ( Nothing, Nothing )
    , size = ""
    , adc = ""
    , score = "2"
    , upgraded = "NO"
    }


init : String -> ( Model, Cmd Msg )
init lesionMapUrl =
    ( { lesionMapUrl = lesionMapUrl
      , patientId = ""
      , mriDate = ""
      , psaLevel = ""
      , first = Nothing
      , second = Nothing
      , ece = "NO"
      , svi = "NO"
      , comments = ""
      , status = Incomplete
      }
    , Cmd.none
    )



-- CMD


entryEncoder : Model -> Encode.Value
entryEncoder model =
    let
        lesionList list =
            List.filter
                (\e ->
                    case e of
                        Nothing ->
                            False

                        Just _ ->
                            True
                )
                list
    in
    Encode.object
        [ ( "patient_id", Encode.string model.patientId )
        , ( "mri_date", Encode.string model.mriDate )
        , ( "psa_level", Encode.string model.psaLevel )
        , ( "ece", Encode.string model.ece )
        , ( "svi", Encode.string model.svi )
        , ( "comments", Encode.string model.comments )
        , ( "lesions", Encode.list lesionEncoder <| lesionList [ model.first, model.second ] )
        ]


lesionEncoder : Maybe LesionData -> Encode.Value
lesionEncoder maybeLesion =
    case maybeLesion of
        Nothing ->
            Encode.null

        Just lesion ->
            let
                name =
                    case lesion.name of
                        First ->
                            "index"

                        Second ->
                            "additional"
            in
            Encode.object
                [ ( "name", Encode.string name )
                , ( "locations"
                  , Encode.list (EX.maybe (Encode.string << .name))
                        [ Tuple.first lesion.location, Tuple.second lesion.location ]
                  )
                , ( "size", Encode.string lesion.size )
                , ( "adc", Encode.string lesion.adc )
                , ( "score", Encode.string lesion.score )
                , ( "upgraded", Encode.string lesion.upgraded )
                ]


entryDecoder : Decode.Decoder String
entryDecoder =
    Decode.succeed "Success"


postEntry : Model -> Cmd Msg
postEntry model =
    Http.post
        { url = "http://r-pedal-backend.apps.tubo.nz/"
        , body = Http.jsonBody <| entryEncoder model
        , expect = Http.expectJson GotEntry entryDecoder
        }



-- Msg and Update


type Msg
    = ClickArea LesionNumber Area String
    | DeleteLocation LesionNumber
    | ToggleLesionForm LesionNumber
    | UpdateField Field String
    | SubmitEntry
    | GotEntry (Result Http.Error String)


updateLesion : Field -> String -> LesionData -> LesionData
updateLesion dataField data oldLesion =
    case dataField of
        LesionSize _ ->
            { oldLesion | size = data }

        ADC _ ->
            { oldLesion | adc = data }

        Score _ ->
            { oldLesion | score = data }

        Upgraded _ ->
            { oldLesion | upgraded = data }

        _ ->
            oldLesion


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickArea lesionNumber newLocation label ->
            let
                lesion =
                    case lesionNumber of
                        First ->
                            Maybe.withDefault (initLesionData First) model.first

                        Second ->
                            Maybe.withDefault (initLesionData Second) model.second

                loc =
                    case lesion.location of
                        ( Nothing, Nothing ) ->
                            ( Just <| Location newLocation label, Nothing )

                        ( Just firstLocation, Nothing ) ->
                            if firstLocation.name == newLocation then
                                lesion.location

                            else
                                ( Just firstLocation, Just <| Location newLocation label )

                        _ ->
                            lesion.location

                newLesion =
                    { lesion | location = loc }
            in
            case lesionNumber of
                First ->
                    ( { model | first = Just newLesion }, Cmd.none )

                Second ->
                    ( { model | second = Just newLesion }, Cmd.none )

        DeleteLocation First ->
            case model.first of
                Just lesion ->
                    let
                        newLoc =
                            ( Nothing, Nothing )

                        newLesion =
                            { lesion | location = newLoc }
                    in
                    ( { model | first = Just newLesion }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DeleteLocation Second ->
            case model.second of
                Just lesion ->
                    let
                        newLoc =
                            ( Nothing, Nothing )

                        newLesion =
                            { lesion | location = newLoc }
                    in
                    ( { model | second = Just newLesion }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ToggleLesionForm lesionNumber ->
            let
                lesion =
                    case lesionNumber of
                        First ->
                            model.first

                        Second ->
                            model.second
            in
            case ( lesionNumber, lesion ) of
                ( First, Just _ ) ->
                    ( { model | first = Nothing }, Cmd.none )

                ( First, Nothing ) ->
                    ( { model | first = Just (initLesionData First) }, Cmd.none )

                ( Second, Just _ ) ->
                    ( { model | second = Nothing }, Cmd.none )

                ( Second, Nothing ) ->
                    ( { model | second = Just (initLesionData Second) }, Cmd.none )

        UpdateField fieldType data ->
            case fieldType of
                PatientId ->
                    ( { model | patientId = data }, Cmd.none )

                MriDate ->
                    ( { model | mriDate = data }, Cmd.none )

                PsaLevel ->
                    ( { model | psaLevel = data }, Cmd.none )

                ECE ->
                    ( { model | ece = data }, Cmd.none )

                SVI ->
                    ( { model | svi = data }, Cmd.none )

                Comments ->
                    ( { model | comments = data }, Cmd.none )

                LesionSize lesionNumber ->
                    let
                        lesion =
                            case lesionNumber of
                                First ->
                                    model.first

                                Second ->
                                    model.second

                        newLesion =
                            Maybe.map (updateLesion (LesionSize lesionNumber) data) lesion
                    in
                    case lesionNumber of
                        First ->
                            ( { model | first = newLesion }, Cmd.none )

                        Second ->
                            ( { model | second = newLesion }, Cmd.none )

                ADC lesionNumber ->
                    let
                        lesion =
                            case lesionNumber of
                                First ->
                                    model.first

                                Second ->
                                    model.second

                        newLesion =
                            Maybe.map (updateLesion (ADC lesionNumber) data) lesion
                    in
                    case lesionNumber of
                        First ->
                            ( { model | first = newLesion }, Cmd.none )

                        Second ->
                            ( { model | second = newLesion }, Cmd.none )

                Score lesionNumber ->
                    let
                        lesion =
                            case lesionNumber of
                                First ->
                                    model.first

                                Second ->
                                    model.second

                        newLesion =
                            Maybe.map (updateLesion (Score lesionNumber) data) lesion
                    in
                    case lesionNumber of
                        First ->
                            ( { model | first = newLesion }, Cmd.none )

                        Second ->
                            ( { model | second = newLesion }, Cmd.none )

                Upgraded lesionNumber ->
                    let
                        lesion =
                            case lesionNumber of
                                First ->
                                    model.first

                                Second ->
                                    model.second

                        newLesion =
                            Maybe.map (updateLesion (Upgraded lesionNumber) data) lesion
                    in
                    case lesionNumber of
                        First ->
                            ( { model | first = newLesion }, Cmd.none )

                        Second ->
                            ( { model | second = newLesion }, Cmd.none )

        GotEntry result ->
            case result of
                Ok _ ->
                    ( { model | status = Success }, Cmd.none )

                Err _ ->
                    ( { model | status = Error "Double check your fields" }, Cmd.none )

        SubmitEntry ->
            ( { model | status = Sending }, postEntry model )


field : String -> List (Html Msg) -> Html Msg
field label inner =
    p [ class "py-2 text-center" ] <| List.append [ span [ class "mr-8 w-48 inline-block text-right font-medium" ] [ text label ] ] inner


textAreaField : String -> Field -> Html Msg
textAreaField label dataField =
    field label <| [ H.textarea [ class "w-48 border align-top text-sm mr-16", Evt.onInput (UpdateField dataField) ] [] ]


inputField : String -> String -> Field -> Html Msg
inputField label name dataField =
    field label <|
        [ input
            [ A.type_ "text"
            , A.required True
            , A.name name
            , class "border w-48 mr-16"
            , Evt.onInput (UpdateField dataField)
            ]
            []
        ]


dateField : String -> String -> Field -> Html Msg
dateField label name dataField =
    field label <|
        [ input
            [ A.type_ "date"
            , A.name name
            , class "border w-48 mr-16"
            , A.pattern "\\d{4}-\\d{2}-\\d{2}"
            , Evt.onInput (UpdateField dataField)
            , A.required False
            ]
            []
        ]


numberField : String -> String -> Field -> Html Msg
numberField label name dataField =
    field label <|
        [ input
            [ A.type_ "number"
            , A.name name
            , class "border w-48 mr-16"
            , Evt.onInput (UpdateField dataField)
            ]
            []
        ]


choiceField : String -> List (Html Msg) -> Field -> Html Msg
choiceField label options dataField =
    field label [ H.select [ class "border w-48 mr-16", Evt.onInput (UpdateField dataField) ] options ]


type alias Coords =
    List Int


type alias AreaName =
    String


type alias Shape =
    String


lesionNode : LesionNumber -> AreaName -> String -> Coords -> Shape -> Html Msg
lesionNode lesion name label coords shape =
    let
        onClick =
            \n l -> SE.onClick <| ClickArea lesion n l

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
                , onClick name label
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
                        , onClick name label
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
                        , onClick name label
                        , SA.class hoverColor
                        ]
                        []

                _ ->
                    S.text ""

        _ ->
            S.text ""


lesionMap : LesionNumber -> List (Html Msg)
lesionMap lesion =
    [ lesionNode
        lesion
        "r_base_afs"
        "R base AFS"
        [ 515, 235, 515, 222, 515, 200, 493, 201, 458, 216, 437, 239, 451, 248, 470, 218, 498, 215 ]
        "poly"
    , lesionNode
        lesion
        "l_base_afs"
        "L base AFS"
        [ 522, 233, 521, 199, 550, 202, 580, 213, 598, 233, 588, 255, 570, 222, 542, 214, 532, 223 ]
        "poly"
    , lesionNode
        lesion
        "r_base_pza"
        "R base PZa"
        [ 436, 239, 426, 266, 421, 295, 460, 281, 451, 259, 450, 248 ]
        "poly"
    , lesionNode
        lesion
        "l_base_pza"
        "L base PZa"
        [ 589, 253, 600, 238, 614, 265, 620, 294, 582, 283 ]
        "poly"
    , lesionNode
        lesion
        "r_base_tza"
        "R base TZa"
        [ 453, 249, 471, 220, 496, 217, 514, 237, 516, 281, 460, 281 ]
        "poly"
    , lesionNode
        lesion
        "l_base_tza"
        "L base TZa"
        [ 523, 235, 542, 216, 567, 223, 585, 255, 580, 280, 522, 281 ]
        "poly"
    , lesionNode
        lesion
        "r_base_tzp"
        "R base TZa"
        [ 515, 298, 469, 286 ]
        "rect"
    , lesionNode
        lesion
        "l_base_tzp"
        "L base TZa"
        [ 567, 298, 521, 286 ]
        "rect"
    , lesionNode
        lesion
        "r_base_pzpl"
        "R base PZpl"
        [ 459, 286, 478, 344, 452, 338, 430, 321, 423, 296 ]
        "poly"
    , lesionNode
        lesion
        "l_base_pzpl"
        "R base PZpl"
        [ 582, 286, 616, 298, 611, 318, 590, 335, 563, 344 ]
        "poly"
    , lesionNode
        lesion
        "r_base_pzpm"
        "R base PZpm"
        [ 474, 318, 483, 344, 512, 346, 489, 335 ]
        "poly"
    , lesionNode
        lesion
        "l_base_pzpm"
        "L base PZpm"
        [ 567, 318, 557, 345, 525, 346, 549, 336 ]
        "poly"
    , lesionNode
        lesion
        "r_base_cz"
        "R base CZ"
        [ 470, 299, 478, 317, 489, 330, 516, 344, 516, 300 ]
        "poly"
    , lesionNode
        lesion
        "l_base_cz"
        "L base CZ"
        [ 522, 301, 571, 301, 566, 315, 546, 334, 522, 344 ]
        "poly"
    , lesionNode
        lesion
        "r_sv"
        "R SV"
        [ 504, 112, 484, 110, 452, 115, 429, 130, 414, 151, 418, 172, 443, 174, 474, 153, 504, 127 ]
        "poly"
    , lesionNode
        lesion
        "l_sv"
        "L SV"
        [ 524, 113, 525, 123, 548, 147, 580, 160, 597, 170, 621, 163, 616, 138, 592, 119, 564, 110 ]
        "poly"
    , lesionNode
        lesion
        "r_mid_afs"
        "R mid AFS"
        [ 517, 394, 517, 433, 510, 416, 497, 410, 478, 416, 471, 409, 492, 397 ]
        "poly"
    , lesionNode
        lesion
        "l_mid_afs"
        "L mid AFS"
        [ 521, 394, 520, 430, 530, 415, 545, 409, 559, 415, 567, 410, 544, 399 ]
        "poly"
    , lesionNode
        lesion
        "r_mid_tza"
        "R mid TZa"
        [ 513, 467, 515, 440, 506, 417, 496, 413, 476, 418, 462, 441, 458, 467 ]
        "poly"
    , lesionNode
        lesion
        "l_mid_tza"
        "L mid TZa"
        [ 579, 467, 576, 442, 566, 425, 545, 412, 533, 417, 522, 437, 525, 467 ]
        "poly"
    , lesionNode
        lesion
        "r_mid_pza"
        "R mid PZa"
        [ 454, 468, 456, 440, 474, 417, 467, 412, 444, 437, 432, 469 ]
        "poly"
    , lesionNode
        lesion
        "l_mid_pza"
        "L mid PZa"
        [ 580, 467, 603, 467, 592, 436, 568, 412, 562, 417, 578, 441 ]
        "poly"
    , lesionNode
        lesion
        "r_mid_pzpl"
        "R mid PZpl"
        [ 455, 473, 432, 473, 437, 505, 453, 525, 475, 533 ]
        "poly"
    , lesionNode
        lesion
        "l_mid_pzpl"
        "L mid PZpl"
        [ 582, 474, 603, 474, 599, 499, 588, 519, 561, 532 ]
        "poly"
    , lesionNode
        lesion
        "r_mid_tzp"
        "R mid TZp"
        [ 461, 473, 508, 473, 500, 488, 483, 495, 467, 488 ]
        "poly"
    , lesionNode
        lesion
        "l_mid_tzp"
        "L mid TZp"
        [ 526, 473, 574, 472, 571, 487, 554, 497, 535, 485 ]
        "poly"
    , lesionNode
        lesion
        "r_mid_pzpm"
        "R mid PZpm"
        [ 468, 494, 482, 497, 507, 489, 512, 497, 514, 535, 494, 536, 478, 532 ]
        "poly"
    , lesionNode
        lesion
        "l_mid_pzpm"
        "L mid PZpm"
        [ 569, 492, 553, 500, 534, 489, 522, 497, 521, 535, 539, 538, 559, 532 ]
        "poly"
    , lesionNode
        lesion
        "r_apex_afs"
        "R apex AFS"
        [ 516, 581, 516, 619, 506, 606, 497, 608, 495, 596, 482, 591 ]
        "poly"
    , lesionNode
        lesion
        "l_apex_afs"
        "L apex AFS"
        [ 520, 581, 520, 619, 528, 606, 541, 611, 541, 597, 551, 590 ]
        "poly"
    , lesionNode
        lesion
        "r_apex_pza"
        "R apex PZa"
        [ 456, 632, 465, 609, 481, 594, 492, 599, 492, 608, 486, 621 ]
        "poly"
    , lesionNode
        lesion
        "l_apex_pza"
        "L apex PZa"
        [ 581, 631, 552, 623, 544, 608, 545, 600, 555, 596, 570, 610 ]
        "poly"
    , lesionNode
        lesion
        "r_apex_tza"
        "R apex TZa"
        [ 489, 621, 497, 610, 510, 611, 515, 621 ]
        "poly"
    , lesionNode
        lesion
        "l_apex_tza"
        "L apex TZa"
        [ 522, 621, 546, 621, 541, 611, 529, 612 ]
        "poly"
    , lesionNode
        lesion
        "r_apex_pzpl"
        "R apex PZpl"
        [ 454, 636, 458, 670, 471, 690, 487, 694, 467, 631 ]
        "poly"
    , lesionNode
        lesion
        "l_apex_pzpl"
        "L apex PZpl"
        [ 570, 633, 584, 637, 581, 666, 570, 688, 551, 692 ]
        "poly"
    , lesionNode
        lesion
        "r_apex_pzpm"
        "R apex PZpm"
        [ 482, 627, 473, 631, 492, 693, 515, 687, 515, 660, 491, 658, 483, 644 ]
        "poly"
    , lesionNode
        lesion
        "l_apex_pzpm"
        "L apex PZpm"
        [ 554, 628, 566, 632, 545, 691, 522, 687, 523, 660, 550, 657, 554, 645 ]
        "poly"
    , lesionNode
        lesion
        "r_apex_tzp"
        "R apex TZp"
        [ 489, 626, 487, 640, 494, 653, 508, 654, 515, 639, 515, 626 ]
        "poly"
    , lesionNode
        lesion
        "l_apex_tzp"
        "L apex TZp"
        [ 550, 628, 550, 641, 545, 654, 526, 656, 521, 639, 521, 628 ]
        "poly"
    , lesionNode
        lesion
        "urethra"
        "Urethra"
        [ 520, 737, 11 ]
        "circle"
    ]


lesionLocation : LesionData -> Html Msg
lesionLocation lesion =
    let
        locationField =
            \loc ->
                case loc of
                    Just { name, label } ->
                        span [ class "border p-1 mx-1 font-mono text-xs rounded cursor-default inline-block w-18 " ] [ text label ]

                    Nothing ->
                        text ""

        resetButton =
            span
                [ class <|
                    "border rounded-full h-4 w-4 inline-flex justify-center items-center "
                        ++ "text-xs font-mono border-gray-600 text-gray-600 shadow-md m-2 cursor-pointer"
                , Evt.onClick <| DeleteLocation lesion.name
                ]
                [ text "x" ]
    in
    case lesion.location of
        ( Nothing, Nothing ) ->
            field "Lesion location:"
                [ span [ class "inline-block font-mono text-xs w-48 mr-16" ] [ text "None selected" ] ]

        _ ->
            field "Lesion location:"
                [ span [ class "mr-8 w-48" ]
                    [ locationField <| Tuple.first lesion.location
                    , locationField <| Tuple.second lesion.location
                    , resetButton
                    ]
                ]


lesionForm : Model -> LesionNumber -> Html Msg
lesionForm model lesionNumber =
    let
        maybeLesion =
            case lesionNumber of
                First ->
                    model.first

                Second ->
                    model.second

        heading =
            case lesionNumber of
                First ->
                    "Index lesion"

                Second ->
                    "Optional lesion"

        newStyle =
            "text-gray-200 bg-teal-600"

        deleteStyle =
            "text-gray-200 bg-red-600"

        basicButton =
            \n label styleClass ->
                H.button
                    [ class <| "block border p-1 mx-auto my-4 rounded text-sm " ++ styleClass
                    , A.type_ "button"
                    , Evt.onClick <| ToggleLesionForm n
                    ]
                    [ text label ]

        primaryLesionButton =
            case ( model.first, model.second ) of
                ( Nothing, Nothing ) ->
                    basicButton First "Add lesion" newStyle

                ( Nothing, Just _ ) ->
                    basicButton First "Delete lesion" deleteStyle

                ( Just _, Nothing ) ->
                    basicButton First "Delete lesion" deleteStyle

                ( Just _, Just _ ) ->
                    text ""

        secondaryLesionButton =
            case ( model.first, model.second ) of
                ( Just _, Nothing ) ->
                    basicButton Second "Add optional lesion" newStyle

                ( Just _, Just _ ) ->
                    basicButton Second "Delete lesion" deleteStyle

                ( Nothing, _ ) ->
                    text ""

        button =
            case lesionNumber of
                First ->
                    primaryLesionButton

                Second ->
                    secondaryLesionButton
    in
    case maybeLesion of
        Just lesion ->
            H.section [ class "text-center border mt-6 mb-2 transition-all duration-200 ease-in-out hover:border-2 hover:shadow-md" ]
                [ div [ class "px-auto" ]
                    [ div [ class "relative mx-auto", A.style "width" "462px", A.style "height" "551px" ]
                        [ H.img [ A.src model.lesionMapUrl, A.usemap "#imageMap", class "w-full absolute" ] []
                        , S.svg [ SA.viewBox "0 0 652 780", SA.class "absolute" ] <| lesionMap lesion.name
                        , p [ class "absolute top-0 left-0 text-center font-bold font-serif text-2xl pt-4 w-full" ] [ text heading ]
                        ]
                    ]
                , div [ class "px-14 text-left" ]
                    [ lesionLocation lesion
                    , numberField "Lesion size (mm):" "lesion-size" (LesionSize lesion.name)
                    , numberField "ADC:" "adc" (ADC lesion.name)
                    , choiceField "PIRADS 2.1 score:"
                        [ H.option [ A.value "2", A.selected (lesion.score == "2") ] [ text "2" ]
                        , H.option [ A.value "3", A.selected (lesion.score == "3") ] [ text "3" ]
                        , H.option [ A.value "4", A.selected (lesion.score == "4") ] [ text "4" ]
                        , H.option [ A.value "5", A.selected (lesion.score == "5") ] [ text "5" ]
                        ]
                        (Score lesion.name)
                    , choiceField "PIRADS 2.1 upgraded?"
                        [ H.option [ A.value "PZ DCE" ] [ text "Yes - PZ DCE" ]
                        , H.option [ A.value "TZ DWI" ] [ text "Yes - TZ DWI" ]
                        , H.option [ A.selected True, A.value "NO" ] [ text "No" ]
                        ]
                        (Upgraded lesion.name)
                    ]
                , button
                ]

        Nothing ->
            H.section [ class "text-center mt-3 mb-6" ] [ button ]


submitButton : Status -> Html Msg
submitButton status =
    case status of
        Sending ->
            H.button
                [ class "block border border-yellow-600 rounded py-1 px-2 my-4 mx-auto my-1 bg-blue-600 text-gray-200 text-lg shadow-md"
                , A.type_ "button"
                , Evt.onClick SubmitEntry
                ]
                [ text "Sending..." ]

        Success ->
            H.button
                [ class "block border border-green-600 rounded py-1 px-2 my-4 mx-auto my-1 bg-blue-600 text-gray-200 text-lg shadow-md"
                , A.type_ "button"
                , Evt.onClick SubmitEntry
                ]
                [ text "Success" ]

        Error _ ->
            H.button
                [ class "block border border-red-600 rounded py-1 px-2 my-4 mx-auto my-1 bg-red-600 text-gray-200 text-lg shadow-md"
                , A.type_ "button"
                , Evt.onClick SubmitEntry
                ]
                [ text "Error - Check your form" ]

        _ ->
            H.button
                [ class "block border rounded py-1 px-2 my-4 mx-auto my-1 bg-blue-600 text-gray-200 text-lg shadow-md"
                , A.type_ "button"
                , Evt.onClick SubmitEntry
                ]
                [ text "Submit" ]


view : Model -> Html Msg
view model =
    H.form [ class "container mx-auto max-w-lg my-10" ]
        [ H.h1 [ class "font-serif text-3xl text-center pb-10" ] [ text "R-PEDAL MRI Data Entry" ]
        , H.section [ class "text-center pb-4" ]
            [ div [ class "px-14" ]
                [ inputField "Patient ID:" "patient-id" PatientId
                , dateField "MRI date:" "mri-date" MriDate
                , numberField "PSA level:" "psa-level" PsaLevel
                ]
            ]
        , lesionForm model First
        , lesionForm model Second
        , choiceField "ECE: "
            [ H.option [ A.value "true" ] [ text "Yes" ]
            , H.option [ A.value "false", A.selected True ] [ text "No" ]
            ]
            ECE
        , choiceField "SVI: "
            [ H.option [ A.value "NO", A.selected True ] [ text "No" ]
            , H.option [ A.value "LEFT" ] [ text "Yes - left" ]
            , H.option [ A.value "RIGHT" ] [ text "Yes - right" ]
            , H.option [ A.value "BILATERAL" ] [ text "Yes - bilateral" ]
            ]
            SVI
        , textAreaField "Any additional findings?" Comments
        , submitButton model.status
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
