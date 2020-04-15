module Main exposing (main)

import Browser
import Html as H exposing (Html, div, input, label, p, span, text)
import Html.Attributes as A exposing (class, name, title)
import Html.Events as Evt
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE


type alias Model =
    { lesionMapUrl : String
    , patientId : String
    , mriDate : String
    , psaLevel : String
    , lesions : List LesionData
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


type alias Area =
    String


type alias LesionId =
    Int


type alias Label =
    String


type alias Location =
    { name : String
    , label : String
    }


type alias LesionData =
    { id : LesionId
    , location : List Location
    , size : String
    , adc : String
    , score : String
    , upgraded : String
    }


initLesionData : LesionId -> LesionData
initLesionData n =
    { id = n
    , location = []
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
      , lesions = []
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
    Encode.object
        [ ( "patient_id", Encode.string model.patientId )
        , ( "mri_date", Encode.string model.mriDate )
        , ( "psa_level", Encode.string model.psaLevel )
        , ( "ece", Encode.string model.ece )
        , ( "svi", Encode.string model.svi )
        , ( "comments", Encode.string model.comments )
        , ( "lesions", Encode.list lesionEncoder model.lesions )
        ]


lesionEncoder : LesionData -> Encode.Value
lesionEncoder lesion =
    Encode.object
        [ ( "locations", Encode.list (Encode.string << .name) lesion.location )
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
        { url = "https://r-pedal-backend.apps.tubo.nz/"
        , body = Http.jsonBody <| entryEncoder model
        , expect = Http.expectJson GotEntry entryDecoder
        }



-- Msg and Update


type GlobalField
    = PatientId
    | MriDate
    | PsaLevel
    | ECE
    | SVI
    | Comments


type LesionField
    = ClickArea Area String
    | ResetLocation
    | LesionSize String
    | ADC String
    | Score String
    | Upgraded String


type Msg
    = ModifyLesion LesionData LesionField
    | UpdateGlobalField GlobalField String
    | AddLesion
    | DeleteLesion LesionData
    | ResetForm
    | SubmitEntry
    | GotEntry (Result Http.Error String)


updateLesionList : LesionData -> List LesionData -> List LesionData
updateLesionList lesion list =
    List.map
        (\l ->
            if l.id == lesion.id then
                lesion

            else
                l
        )
        list


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModifyLesion lesion field ->
            case field of
                ClickArea newLocation newLabel ->
                    let
                        newLocationList =
                            case lesion.location of
                                [] ->
                                    [ Location newLocation newLabel ]

                                [ a ] ->
                                    if a.name == newLocation then
                                        [ a ]

                                    else
                                        [ a, Location newLocation newLabel ]

                                _ ->
                                    lesion.location

                        newLesion =
                            { lesion | location = newLocationList }
                    in
                    ( { model | lesions = updateLesionList newLesion model.lesions }, Cmd.none )

                ResetLocation ->
                    let
                        newLesion =
                            { lesion | location = [] }
                    in
                    ( { model | lesions = updateLesionList newLesion model.lesions }, Cmd.none )

                LesionSize data ->
                    let
                        newLesion =
                            { lesion | size = data }
                    in
                    ( { model | lesions = updateLesionList newLesion model.lesions }, Cmd.none )

                ADC data ->
                    let
                        newLesion =
                            { lesion | adc = data }
                    in
                    ( { model | lesions = updateLesionList newLesion model.lesions }, Cmd.none )

                Score data ->
                    let
                        newLesion =
                            { lesion | score = data }
                    in
                    ( { model | lesions = updateLesionList newLesion model.lesions }, Cmd.none )

                Upgraded data ->
                    let
                        newLesion =
                            { lesion | upgraded = data }
                    in
                    ( { model | lesions = updateLesionList newLesion model.lesions }, Cmd.none )

        UpdateGlobalField field data ->
            case field of
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

        GotEntry result ->
            case result of
                Ok _ ->
                    ( { model | status = Success }, Cmd.none )

                Err _ ->
                    ( { model | status = Error "Double check your fields" }, Cmd.none )

        SubmitEntry ->
            ( { model | status = Sending }, postEntry model )

        AddLesion ->
            let
                largestId =
                    List.maximum <| List.map .id model.lesions

                id =
                    case largestId of
                        Just largest ->
                            largest + 1

                        Nothing ->
                            1
            in
            ( { model | lesions = model.lesions ++ [ initLesionData id ] }, Cmd.none )

        DeleteLesion lesion ->
            let
                newLesionList =
                    List.filter (\l -> l.id /= lesion.id) model.lesions
            in
            ( { model | lesions = newLesionList }, Cmd.none )

        ResetForm ->
            init model.lesionMapUrl


genericField : String -> List (Html Msg) -> Html Msg
genericField label inner =
    p [ class "py-2 text-center" ] <|
        List.append
            [ span [ class "mr-8 w-48 inline-block text-right font-medium" ]
                [ text label ]
            ]
            inner


textAreaField : String -> String -> (String -> Msg) -> Html Msg
textAreaField label value handler =
    genericField label <|
        [ H.textarea
            [ class "w-48 border align-top text-sm mr-16"
            , Evt.onInput handler
            , A.value value
            ]
            []
        ]


inputField : String -> String -> String -> (String -> Msg) -> Html Msg
inputField label name value handler =
    genericField label <|
        [ input
            [ A.type_ "text"
            , A.required True
            , A.name name
            , A.value value
            , class "border w-48 mr-16"
            , Evt.onInput handler
            ]
            []
        ]


dateField : String -> String -> String -> (String -> Msg) -> Html Msg
dateField label name value handler =
    genericField label <|
        [ input
            [ A.type_ "date"
            , A.name name
            , class "border w-48 mr-16"
            , A.pattern "\\d{4}-\\d{2}-\\d{2}"
            , A.placeholder "YYYY-MM-DD"
            , A.required True
            , A.value value
            , Evt.onInput handler
            ]
            []
        ]


numberField : String -> String -> String -> (String -> Msg) -> Html Msg
numberField label name value handler =
    genericField label <|
        [ input
            [ A.type_ "number"
            , A.name name
            , class "border w-48 mr-16"
            , Evt.onInput handler
            , A.value value
            ]
            []
        ]


choiceField : String -> List (Html Msg) -> (String -> Msg) -> Html Msg
choiceField label options handler =
    genericField label <|
        [ H.select
            [ class "border w-48 mr-16"
            , Evt.onInput handler
            ]
            options
        ]


type alias Coords =
    List Int


type alias AreaName =
    String


type alias Shape =
    String


singleRegionOverlay : LesionData -> AreaName -> String -> Coords -> Shape -> Html Msg
singleRegionOverlay lesion name label coords shape =
    let
        onClick =
            \n l -> SE.onClick <| ModifyLesion lesion (ClickArea n l)

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


lesionMap : LesionData -> List (Html Msg)
lesionMap lesion =
    [ singleRegionOverlay
        lesion
        "r_base_afs"
        "R base AFS"
        [ 515, 235, 515, 222, 515, 200, 493, 201, 458, 216, 437, 239, 451, 248, 470, 218, 498, 215 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_base_afs"
        "L base AFS"
        [ 522, 233, 521, 199, 550, 202, 580, 213, 598, 233, 588, 255, 570, 222, 542, 214, 532, 223 ]
        "poly"
    , singleRegionOverlay
        lesion
        "r_base_pza"
        "R base PZa"
        [ 436, 239, 426, 266, 421, 295, 460, 281, 451, 259, 450, 248 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_base_pza"
        "L base PZa"
        [ 589, 253, 600, 238, 614, 265, 620, 294, 582, 283 ]
        "poly"
    , singleRegionOverlay
        lesion
        "r_base_tza"
        "R base TZa"
        [ 453, 249, 471, 220, 496, 217, 514, 237, 516, 281, 460, 281 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_base_tza"
        "L base TZa"
        [ 523, 235, 542, 216, 567, 223, 585, 255, 580, 280, 522, 281 ]
        "poly"
    , singleRegionOverlay
        lesion
        "r_base_tzp"
        "R base TZa"
        [ 515, 298, 469, 286 ]
        "rect"
    , singleRegionOverlay
        lesion
        "l_base_tzp"
        "L base TZa"
        [ 567, 298, 521, 286 ]
        "rect"
    , singleRegionOverlay
        lesion
        "r_base_pzpl"
        "R base PZpl"
        [ 459, 286, 478, 344, 452, 338, 430, 321, 423, 296 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_base_pzpl"
        "R base PZpl"
        [ 582, 286, 616, 298, 611, 318, 590, 335, 563, 344 ]
        "poly"
    , singleRegionOverlay
        lesion
        "r_base_pzpm"
        "R base PZpm"
        [ 474, 318, 483, 344, 512, 346, 489, 335 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_base_pzpm"
        "L base PZpm"
        [ 567, 318, 557, 345, 525, 346, 549, 336 ]
        "poly"
    , singleRegionOverlay
        lesion
        "r_base_cz"
        "R base CZ"
        [ 470, 299, 478, 317, 489, 330, 516, 344, 516, 300 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_base_cz"
        "L base CZ"
        [ 522, 301, 571, 301, 566, 315, 546, 334, 522, 344 ]
        "poly"
    , singleRegionOverlay
        lesion
        "r_sv"
        "R SV"
        [ 504, 112, 484, 110, 452, 115, 429, 130, 414, 151, 418, 172, 443, 174, 474, 153, 504, 127 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_sv"
        "L SV"
        [ 524, 113, 525, 123, 548, 147, 580, 160, 597, 170, 621, 163, 616, 138, 592, 119, 564, 110 ]
        "poly"
    , singleRegionOverlay
        lesion
        "r_mid_afs"
        "R mid AFS"
        [ 517, 394, 517, 433, 510, 416, 497, 410, 478, 416, 471, 409, 492, 397 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_mid_afs"
        "L mid AFS"
        [ 521, 394, 520, 430, 530, 415, 545, 409, 559, 415, 567, 410, 544, 399 ]
        "poly"
    , singleRegionOverlay
        lesion
        "r_mid_tza"
        "R mid TZa"
        [ 513, 467, 515, 440, 506, 417, 496, 413, 476, 418, 462, 441, 458, 467 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_mid_tza"
        "L mid TZa"
        [ 579, 467, 576, 442, 566, 425, 545, 412, 533, 417, 522, 437, 525, 467 ]
        "poly"
    , singleRegionOverlay
        lesion
        "r_mid_pza"
        "R mid PZa"
        [ 454, 468, 456, 440, 474, 417, 467, 412, 444, 437, 432, 469 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_mid_pza"
        "L mid PZa"
        [ 580, 467, 603, 467, 592, 436, 568, 412, 562, 417, 578, 441 ]
        "poly"
    , singleRegionOverlay
        lesion
        "r_mid_pzpl"
        "R mid PZpl"
        [ 455, 473, 432, 473, 437, 505, 453, 525, 475, 533 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_mid_pzpl"
        "L mid PZpl"
        [ 582, 474, 603, 474, 599, 499, 588, 519, 561, 532 ]
        "poly"
    , singleRegionOverlay
        lesion
        "r_mid_tzp"
        "R mid TZp"
        [ 461, 473, 508, 473, 500, 488, 483, 495, 467, 488 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_mid_tzp"
        "L mid TZp"
        [ 526, 473, 574, 472, 571, 487, 554, 497, 535, 485 ]
        "poly"
    , singleRegionOverlay
        lesion
        "r_mid_pzpm"
        "R mid PZpm"
        [ 468, 494, 482, 497, 507, 489, 512, 497, 514, 535, 494, 536, 478, 532 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_mid_pzpm"
        "L mid PZpm"
        [ 569, 492, 553, 500, 534, 489, 522, 497, 521, 535, 539, 538, 559, 532 ]
        "poly"
    , singleRegionOverlay
        lesion
        "r_apex_afs"
        "R apex AFS"
        [ 516, 581, 516, 619, 506, 606, 497, 608, 495, 596, 482, 591 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_apex_afs"
        "L apex AFS"
        [ 520, 581, 520, 619, 528, 606, 541, 611, 541, 597, 551, 590 ]
        "poly"
    , singleRegionOverlay
        lesion
        "r_apex_pza"
        "R apex PZa"
        [ 456, 632, 465, 609, 481, 594, 492, 599, 492, 608, 486, 621 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_apex_pza"
        "L apex PZa"
        [ 581, 631, 552, 623, 544, 608, 545, 600, 555, 596, 570, 610 ]
        "poly"
    , singleRegionOverlay
        lesion
        "r_apex_tza"
        "R apex TZa"
        [ 489, 621, 497, 610, 510, 611, 515, 621 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_apex_tza"
        "L apex TZa"
        [ 522, 621, 546, 621, 541, 611, 529, 612 ]
        "poly"
    , singleRegionOverlay
        lesion
        "r_apex_pzpl"
        "R apex PZpl"
        [ 454, 636, 458, 670, 471, 690, 487, 694, 467, 631 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_apex_pzpl"
        "L apex PZpl"
        [ 570, 633, 584, 637, 581, 666, 570, 688, 551, 692 ]
        "poly"
    , singleRegionOverlay
        lesion
        "r_apex_pzpm"
        "R apex PZpm"
        [ 482, 627, 473, 631, 492, 693, 515, 687, 515, 660, 491, 658, 483, 644 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_apex_pzpm"
        "L apex PZpm"
        [ 554, 628, 566, 632, 545, 691, 522, 687, 523, 660, 550, 657, 554, 645 ]
        "poly"
    , singleRegionOverlay
        lesion
        "r_apex_tzp"
        "R apex TZp"
        [ 489, 626, 487, 640, 494, 653, 508, 654, 515, 639, 515, 626 ]
        "poly"
    , singleRegionOverlay
        lesion
        "l_apex_tzp"
        "L apex TZp"
        [ 550, 628, 550, 641, 545, 654, 526, 656, 521, 639, 521, 628 ]
        "poly"
    , singleRegionOverlay
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
            \{ name, label } ->
                span [ class "border p-1 mx-1 font-mono text-xs rounded cursor-default inline-block w-18 " ] [ text label ]

        resetIcon =
            span
                [ class <|
                    "border rounded-full h-4 w-4 inline-flex justify-center items-center "
                        ++ "text-xs font-mono border-gray-600 text-gray-600 shadow-md m-2 cursor-pointer"
                , Evt.onClick <| ModifyLesion lesion ResetLocation
                ]
                [ text "x" ]
    in
    case lesion.location of
        [] ->
            genericField "Lesion location:"
                [ span [ class "inline-block font-mono text-xs w-48 mr-16" ] [ text "None selected" ] ]

        location ->
            genericField "Lesion location:"
                [ span [ class "mr-8 w-48" ] <|
                    List.map locationField location
                        ++ [ resetIcon ]
                ]


lesionForm : String -> LesionData -> Html Msg
lesionForm lesionMapUrl lesion =
    let
        title =
            "Lesion #" ++ String.fromInt lesion.id

        deleteButton =
            H.button
                [ class <| "block border-red-600 p-1 mx-auto mt-6 mb-2 rounded text-sm text-white bg-red-600"
                , A.type_ "button"
                , Evt.onClick <| DeleteLesion lesion
                ]
                [ text "Delete lesion" ]
    in
    H.section [ class "text-center border mt-6 pb-4 transition-all duration-200 ease-in-out hover:border-2 hover:shadow-md" ]
        [ div [ class "px-auto" ]
            [ div [ class "relative mx-auto", A.style "width" "462px", A.style "height" "551px" ]
                [ H.img [ A.src lesionMapUrl, A.usemap "#imageMap", class "w-full absolute" ] []
                , S.svg [ SA.viewBox "0 0 652 780", SA.class "absolute" ] <| lesionMap lesion
                , p [ class "absolute top-0 left-0 text-center font-bold font-serif text-2xl pt-4 w-full" ] [ text title ]
                ]
            ]
        , div [ class "px-14 text-left" ]
            [ lesionLocation lesion
            , numberField "Lesion size (mm):" "lesion-size" lesion.size (ModifyLesion lesion << LesionSize)
            , numberField "ADC:" "adc" lesion.adc (ModifyLesion lesion << ADC)
            , choiceField "PIRADS 2.1 score:"
                [ H.option [ A.value "2", A.selected (lesion.score == "2") ] [ text "2" ]
                , H.option [ A.value "3", A.selected (lesion.score == "3") ] [ text "3" ]
                , H.option [ A.value "4", A.selected (lesion.score == "4") ] [ text "4" ]
                , H.option [ A.value "5", A.selected (lesion.score == "5") ] [ text "5" ]
                ]
                (ModifyLesion lesion << Score)
            , choiceField "PIRADS 2.1 upgraded?"
                [ H.option
                    [ A.value "PZ DCE"
                    , A.selected (lesion.upgraded == "PZ DCE")
                    ]
                    [ text "Yes - PZ DCE" ]
                , H.option
                    [ A.value "TZ DWI"
                    , A.selected (lesion.upgraded == "TZ DWI")
                    ]
                    [ text "Yes - TZ DWI" ]
                , H.option
                    [ A.selected (lesion.upgraded == "NO")
                    , A.value "NO"
                    ]
                    [ text "No" ]
                ]
                (ModifyLesion lesion << Upgraded)
            ]
        , deleteButton
        ]


viewLesionForms : String -> List LesionData -> Html Msg
viewLesionForms url lesions =
    div [] <|
        List.map (lesionForm url) lesions


newLesionButton : List LesionData -> Html Msg
newLesionButton lesions =
    let
        lesionNumber =
            List.length lesions

        buttonText =
            if lesionNumber == 0 then
                "Add index lesion"

            else if lesionNumber >= 4 then
                "Limit lesion number reached"

            else
                "Add an additional lesion"
    in
    H.button
        [ class "block mx-auto py-1 px-2 my-8 my-1 "
        , class
            (if lesionNumber >= 4 then
                "text-black text-sm underline cursor-default"

             else
                "bg-green-600 text-gray-200 border rounded text-lg shadow-md"
            )
        , A.type_ "button"
        , A.disabled (lesionNumber >= 4)
        , Evt.onClick AddLesion
        ]
        [ text buttonText ]


submitButton : Status -> Html Msg
submitButton status =
    let
        baseStyle =
            class "block border rounded py-1 px-2 my-4 mx-auto my-1 text-lg shadow-md"
    in
    case status of
        Sending ->
            H.button
                [ baseStyle
                , class "border-yellow-600 bg-yellow-600 text-white"
                , A.type_ "button"
                , Evt.onClick SubmitEntry
                ]
                [ text "Sending..." ]

        Success ->
            H.button
                [ class "border-green-600 bg-green-600 text-gray-200"
                , baseStyle
                , A.type_ "button"
                , Evt.onClick SubmitEntry
                ]
                [ text "Success" ]

        Error _ ->
            H.button
                [ baseStyle
                , class "border-red-600 bg-red-600 text-white"
                , A.type_ "button"
                , Evt.onClick SubmitEntry
                ]
                [ text "Error - Check your form" ]

        _ ->
            H.button
                [ baseStyle
                , class "bg-blue-600 text-white"
                , A.type_ "button"
                , Evt.onClick SubmitEntry
                ]
                [ text "Submit" ]


resetButton : Html Msg
resetButton =
    let
        baseStyle =
            class "block underline py-1 px-2 my-4 mx-auto my-1 text-sm"
    in
    H.button
        [ baseStyle
        , class "text-black"
        , A.type_ "button"
        , Evt.onClick ResetForm
        ]
        [ text "Reset Form" ]


view : Model -> Html Msg
view model =
    H.form [ class "container mx-auto max-w-lg my-10" ]
        [ H.h1 [ class "font-serif text-3xl text-center pb-10" ] [ text "R-PEDAL MRI Data Entry" ]
        , H.section [ class "text-center pb-4" ]
            [ div [ class "px-14" ]
                [ inputField "Patient ID:" "patient-id" model.patientId (UpdateGlobalField PatientId)
                , dateField "MRI date:" "mri-date" model.mriDate (UpdateGlobalField MriDate)
                , numberField "PSA level:" "psa-level" model.psaLevel (UpdateGlobalField PsaLevel)
                ]
            ]
        , viewLesionForms model.lesionMapUrl model.lesions
        , newLesionButton model.lesions
        , choiceField "ECE: "
            [ H.option [ A.value "true" ] [ text "Yes" ]
            , H.option [ A.value "false", A.selected True ] [ text "No" ]
            ]
            (UpdateGlobalField ECE)
        , choiceField "SVI: "
            [ H.option [ A.value "NO", A.selected True ] [ text "No" ]
            , H.option [ A.value "LEFT" ] [ text "Yes - left" ]
            , H.option [ A.value "RIGHT" ] [ text "Yes - right" ]
            , H.option [ A.value "BILATERAL" ] [ text "Yes - bilateral" ]
            ]
            (UpdateGlobalField SVI)
        , textAreaField "Any additional findings?" model.comments (UpdateGlobalField Comments)
        , div [] [ submitButton model.status, resetButton ]
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
