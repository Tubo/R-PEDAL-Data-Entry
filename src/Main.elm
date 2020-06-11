module Main exposing (main)

import Browser
import Html as H exposing (Html, div, input, label, p, span, text)
import Html.Attributes as A exposing (class, name, title)
import Html.Events as Evt
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE


type Model
    = SelectForm LesionUrl
    | MRI LesionUrl MriForm
    | PSMA LesionUrl PsmaForm
    | Pathology LesionUrl PathologyForm


type alias GenericForm a =
    { a
        | status : Status
    }


type alias GenericLesion a =
    { a
        | id : Id
    }


type alias MriForm =
    { patientId : String
    , mriDate : String
    , psaLevel : String
    , ece : String
    , svi : String
    , comments : String
    , lesions : List MriLesion
    , status : Status
    }


type alias MriLesion =
    { id : Id
    , isIndex : Bool
    , location : List Location
    , size : String
    , adc : String
    , score : String
    , upgraded : String
    }


type alias PathologyForm =
    { patientId : String
    , procedureDate : String
    , comments : String
    , specimenType : PathologySpecimen
    , lesions : List PathologyLesion
    , status : Status
    }


type alias PathologyLesion =
    { id : Id
    , isIndex : Bool
    , grade : String
    , size : String
    , greatestPercentage : String
    , positiveCore : String
    , totalCore : String
    , lesionSize : String
    , locSide : String
    , locZone : String
    }


type PathologySpecimen
    = Biopsy
    | Prostatectomy


type alias PsmaForm =
    { patientId : String
    , psmaDate : String
    , comments : String
    , lesions : List PsmaLesion
    , status : Status
    }


type alias PsmaLesion =
    { id : Id
    , isIndex : Bool
    , location : List Location
    , suv : String
    }


type LesionOverlay
    = MriOverlay MriLesion
    | PsmaOverlay PsmaLesion


type alias Location =
    { name : String
    , label : String
    }


type alias LesionUrl =
    String


type alias Area =
    String


type alias Id =
    Int


type alias Label =
    String


type Status
    = Incomplete
    | Ready
    | Sending
    | Success
    | Error String


initMriLesion : Id -> MriLesion
initMriLesion n =
    { id = n
    , isIndex = False
    , location = []
    , size = ""
    , adc = ""
    , score = "2"
    , upgraded = "NO"
    }


initMriForm : MriForm
initMriForm =
    { patientId = ""
    , mriDate = ""
    , psaLevel = ""
    , ece = "NO"
    , svi = "NO"
    , comments = ""
    , lesions = []
    , status = Incomplete
    }


initPathologyLesion : Id -> PathologyLesion
initPathologyLesion id =
    { id = id
    , isIndex = False
    , grade = "NONE"
    , size = ""
    , greatestPercentage = ""
    , positiveCore = ""
    , totalCore = ""
    , lesionSize = ""
    , locSide = "NA"
    , locZone = "NA"
    }


initPathologyForm : PathologyForm
initPathologyForm =
    { patientId = ""
    , procedureDate = ""
    , comments = ""
    , specimenType = Biopsy
    , lesions = []
    , status = Incomplete
    }


initPsmaLesion : Id -> PsmaLesion
initPsmaLesion id =
    { id = id
    , isIndex = False
    , location = []
    , suv = ""
    }


initPsmaForm : PsmaForm
initPsmaForm =
    { patientId = ""
    , psmaDate = ""
    , comments = ""
    , lesions = []
    , status = Incomplete
    }


init : LesionUrl -> ( Model, Cmd Msg )
init lesionUrl =
    ( SelectForm lesionUrl, Cmd.none )



--( SelectForm lesionUrl, Cmd.none )
-- CMD


mriFormEncoder : MriForm -> Encode.Value
mriFormEncoder model =
    Encode.object
        [ ( "patient_id", Encode.string model.patientId )
        , ( "mri_date", Encode.string model.mriDate )
        , ( "psa_level", Encode.string model.psaLevel )
        , ( "ece", Encode.string model.ece )
        , ( "svi", Encode.string model.svi )
        , ( "comments", Encode.string model.comments )
        , ( "lesions", Encode.list mriLesionEncoder model.lesions )
        ]


mriLesionEncoder : MriLesion -> Encode.Value
mriLesionEncoder lesion =
    Encode.object
        [ ( "is_index", Encode.bool lesion.isIndex )
        , ( "locations", Encode.list (Encode.string << .name) lesion.location )
        , ( "size", Encode.string lesion.size )
        , ( "adc", Encode.string lesion.adc )
        , ( "score", Encode.string lesion.score )
        , ( "upgraded", Encode.string lesion.upgraded )
        ]


psmaFormEncoder : PsmaForm -> Encode.Value
psmaFormEncoder form =
    Encode.object
        [ ( "patient_id", Encode.string form.patientId )
        , ( "psma_date", Encode.string form.psmaDate )
        , ( "comments", Encode.string form.comments )
        , ( "lesions", Encode.list psmaLesionEncoder form.lesions )
        ]


psmaLesionEncoder : PsmaLesion -> Encode.Value
psmaLesionEncoder lesion =
    Encode.object
        [ ( "is_index", Encode.bool lesion.isIndex )
        , ( "locations", Encode.list (Encode.string << .name) lesion.location )
        , ( "suv", Encode.string lesion.suv )
        ]


pathologyFormEncoder : PathologyForm -> Encode.Value
pathologyFormEncoder form =
    Encode.object
        [ ( "patient_id", Encode.string form.patientId )
        , ( "procedure_date", Encode.string form.procedureDate )
        , ( "comments", Encode.string form.comments )
        , ( "lesions", Encode.list pathologyLesionEncoder form.lesions )
        ]


pathologyLesionEncoder : PathologyLesion -> Encode.Value
pathologyLesionEncoder lesion =
    let
        lesionSize =
            String.toFloat lesion.size

        positiveCore =
            String.toInt lesion.positiveCore

        totalCore =
            String.toInt lesion.totalCore

        greatestPercentage =
            String.toFloat lesion.greatestPercentage
    in
    Encode.object
        [ ( "is_index", Encode.bool lesion.isIndex )
        , ( "grade", Encode.string lesion.grade )
        , ( "greatest_percentage", maybe Encode.float greatestPercentage )
        , ( "positive_core", maybe Encode.int positiveCore )
        , ( "total_core", maybe Encode.int totalCore )
        , ( "lesion_size", maybe Encode.float lesionSize )
        , ( "loc_side", Encode.string lesion.locSide )
        , ( "loc_zone", Encode.string lesion.locZone )
        ]


entryDecoder : Decode.Decoder String
entryDecoder =
    Decode.succeed "Success"


baseUrl =
    "https://r-pedal-backend.apps.tubo.nz/"


postMriForm : MriForm -> Cmd Msg
postMriForm form =
    Http.post
        { url = baseUrl ++ "mri/"
        , body = Http.jsonBody <| mriFormEncoder form
        , expect = Http.expectJson GotEntry entryDecoder
        }


postPsmaForm : PsmaForm -> Cmd Msg
postPsmaForm form =
    Http.post
        { url = baseUrl ++ "psma/"
        , body = Http.jsonBody <| psmaFormEncoder form
        , expect = Http.expectJson GotEntry entryDecoder
        }


postPathologyForm : PathologyForm -> Cmd Msg
postPathologyForm form =
    Http.post
        { url = baseUrl ++ "pathology/"
        , body = Http.jsonBody <| pathologyFormEncoder form
        , expect = Http.expectJson GotEntry entryDecoder
        }



-- Msg and Update


type MriField
    = MriPatientId String
    | MriDate String
    | PsaLevel String
    | ECE String
    | SVI String
    | MriComments String
    | UpdateMriLesion MriLesion MriLesionField
    | AddMriLesion
    | DeleteMriLesion MriLesion


type MriLesionField
    = ClickMapMri Area String
    | MriLesionSize String
    | ADC String
    | Score String
    | Upgraded String
    | ResetMriLesionLocation


type PsmaField
    = PsmaPatientId String
    | PsmaDate String
    | PsmaComments String
    | UpdatePsmaLesion PsmaLesion PsmaLesionField
    | AddPsmaLesion
    | DeletePsmaLesion PsmaLesion


type PsmaLesionField
    = ClickMapPsma Area String
    | SUV String
    | ResetPsmaLesionLocation


type PathologyField
    = PathologyPatientId String
    | ProcedureDate String
    | PathologyComments String
    | SpecimenType String
    | UpdatePathologyLesion PathologyLesion PathologyLesionField
    | AddPathologyLesion
    | DeletePathologyLesion PathologyLesion


type PathologyLesionField
    = PathologyGrade String
    | PathologyLesionSize String
    | PositiveCore String
    | TotalCore String
    | GreatestPercentage String
    | PathologySide String
    | PathologyZone String


type Pages
    = SelectPage
    | MriPage
    | PsmaPage
    | PathologyPage


type Msg
    = UpdateMriForm MriField
    | UpdatePsmaForm PsmaField
    | UpdatePathologyForm PathologyField
    | ResetForm
    | SubmitEntry
    | GotEntry (Result Http.Error String)
    | GotoPage Pages


updateLesionList : GenericLesion a -> List (GenericLesion a) -> List (GenericLesion a)
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
    let
        lesionMapUrl =
            case model of
                SelectForm lesionUrl ->
                    lesionUrl

                MRI lesionUrl _ ->
                    lesionUrl

                PSMA lesionUrl _ ->
                    lesionUrl

                Pathology lesionUrl _ ->
                    lesionUrl
    in
    case ( model, msg ) of
        ( MRI url form, UpdateMriForm mriField ) ->
            case mriField of
                MriPatientId string ->
                    ( MRI url { form | patientId = string }, Cmd.none )

                MriDate string ->
                    ( MRI url { form | mriDate = string }, Cmd.none )

                PsaLevel string ->
                    ( MRI url { form | psaLevel = string }, Cmd.none )

                ECE string ->
                    ( MRI url { form | ece = string }, Cmd.none )

                SVI string ->
                    ( MRI url { form | svi = string }, Cmd.none )

                MriComments string ->
                    ( MRI url { form | comments = string }, Cmd.none )

                AddMriLesion ->
                    let
                        largestId =
                            List.maximum <| List.map .id form.lesions

                        id =
                            case largestId of
                                Just largest ->
                                    largest + 1

                                Nothing ->
                                    1
                    in
                    ( MRI url
                        { form | lesions = form.lesions ++ [ initMriLesion id ] }
                    , Cmd.none
                    )

                DeleteMriLesion lesion ->
                    let
                        newLesionList =
                            List.filter (\l -> l.id /= lesion.id) form.lesions
                    in
                    ( MRI url
                        { form | lesions = newLesionList }
                    , Cmd.none
                    )

                UpdateMriLesion lesion mriLesionField ->
                    case mriLesionField of
                        ClickMapMri newLocName newLocLabel ->
                            let
                                newLocation =
                                    Location newLocName newLocLabel

                                newLocationList =
                                    if
                                        (List.length lesion.location < 4)
                                            && not (List.member newLocation lesion.location)
                                    then
                                        lesion.location ++ [ Location newLocName newLocLabel ]

                                    else
                                        lesion.location

                                newLesion =
                                    { lesion | location = newLocationList }
                            in
                            ( MRI url { form | lesions = updateLesionList newLesion form.lesions }
                            , Cmd.none
                            )

                        ResetMriLesionLocation ->
                            let
                                newLesion =
                                    { lesion | location = [] }
                            in
                            ( MRI url
                                { form | lesions = updateLesionList newLesion form.lesions }
                            , Cmd.none
                            )

                        MriLesionSize data ->
                            let
                                newLesion =
                                    { lesion | size = data }
                            in
                            ( MRI url
                                { form | lesions = updateLesionList newLesion form.lesions }
                            , Cmd.none
                            )

                        ADC data ->
                            let
                                newLesion =
                                    { lesion | adc = data }
                            in
                            ( MRI url
                                { form | lesions = updateLesionList newLesion form.lesions }
                            , Cmd.none
                            )

                        Score data ->
                            let
                                newLesion =
                                    { lesion | score = data }
                            in
                            ( MRI url
                                { form | lesions = updateLesionList newLesion form.lesions }
                            , Cmd.none
                            )

                        Upgraded data ->
                            let
                                newLesion =
                                    { lesion | upgraded = data }
                            in
                            ( MRI url
                                { form | lesions = updateLesionList newLesion form.lesions }
                            , Cmd.none
                            )

        ( PSMA _ form, UpdatePsmaForm psmaField ) ->
            case psmaField of
                PsmaPatientId string ->
                    ( PSMA lesionMapUrl { form | patientId = string }, Cmd.none )

                PsmaDate string ->
                    ( PSMA lesionMapUrl { form | psmaDate = string }, Cmd.none )

                PsmaComments string ->
                    ( PSMA lesionMapUrl { form | comments = string }, Cmd.none )

                UpdatePsmaLesion lesion psmaLesionField ->
                    case psmaLesionField of
                        ClickMapPsma newLocName newLocLabel ->
                            let
                                newLocation =
                                    Location newLocName newLocLabel

                                newLocationList =
                                    if
                                        (List.length lesion.location < 4)
                                            && not (List.member newLocation lesion.location)
                                    then
                                        lesion.location ++ [ Location newLocName newLocLabel ]

                                    else
                                        lesion.location

                                newLesion =
                                    { lesion | location = newLocationList }
                            in
                            ( PSMA lesionMapUrl { form | lesions = updateLesionList newLesion form.lesions }
                            , Cmd.none
                            )

                        ResetPsmaLesionLocation ->
                            let
                                newLesion =
                                    { lesion | location = [] }
                            in
                            ( PSMA lesionMapUrl
                                { form | lesions = updateLesionList newLesion form.lesions }
                            , Cmd.none
                            )

                        SUV data ->
                            let
                                newLesion =
                                    { lesion | suv = data }
                            in
                            ( PSMA lesionMapUrl
                                { form | lesions = updateLesionList newLesion form.lesions }
                            , Cmd.none
                            )

                AddPsmaLesion ->
                    let
                        largestId =
                            List.maximum <| List.map .id form.lesions

                        id =
                            case largestId of
                                Just largest ->
                                    largest + 1

                                Nothing ->
                                    1
                    in
                    ( PSMA lesionMapUrl
                        { form | lesions = form.lesions ++ [ initPsmaLesion id ] }
                    , Cmd.none
                    )

                DeletePsmaLesion lesion ->
                    let
                        newLesionList =
                            List.filter (\l -> l.id /= lesion.id) form.lesions
                    in
                    ( PSMA lesionMapUrl
                        { form | lesions = newLesionList }
                    , Cmd.none
                    )

        ( Pathology _ form, UpdatePathologyForm pathologyField ) ->
            case pathologyField of
                PathologyPatientId string ->
                    ( Pathology lesionMapUrl { form | patientId = string }, Cmd.none )

                ProcedureDate string ->
                    ( Pathology lesionMapUrl { form | procedureDate = string }, Cmd.none )

                PathologyComments string ->
                    ( Pathology lesionMapUrl { form | comments = string }, Cmd.none )

                SpecimenType specimenString ->
                    let
                        specimen =
                            case specimenString of
                                "Prostatectomy" ->
                                    Prostatectomy

                                _ ->
                                    Biopsy
                    in
                    ( Pathology lesionMapUrl { form | specimenType = specimen, lesions = [] }, Cmd.none )

                UpdatePathologyLesion lesion pathologyLesionField ->
                    let
                        newLesion =
                            case pathologyLesionField of
                                PathologyGrade data ->
                                    { lesion | grade = data }

                                PathologyLesionSize data ->
                                    { lesion | size = data }

                                PositiveCore data ->
                                    { lesion | positiveCore = data }

                                TotalCore data ->
                                    { lesion | totalCore = data }

                                GreatestPercentage data ->
                                    { lesion | greatestPercentage = data }

                                PathologySide data ->
                                    { lesion | locSide = data }

                                PathologyZone data ->
                                    { lesion | locZone = data }
                    in
                    ( Pathology lesionMapUrl
                        { form | lesions = updateLesionList newLesion form.lesions }
                    , Cmd.none
                    )

                AddPathologyLesion ->
                    let
                        largestId =
                            List.maximum <| List.map .id form.lesions

                        id =
                            case largestId of
                                Just largest ->
                                    largest + 1

                                Nothing ->
                                    1
                    in
                    ( Pathology lesionMapUrl
                        { form | lesions = form.lesions ++ [ initPathologyLesion id ] }
                    , Cmd.none
                    )

                DeletePathologyLesion lesion ->
                    let
                        newLesionList =
                            List.filter (\l -> l.id /= lesion.id) form.lesions
                    in
                    ( Pathology lesionMapUrl
                        { form | lesions = newLesionList }
                    , Cmd.none
                    )

        ( page, GotEntry result ) ->
            case page of
                SelectForm _ ->
                    ( model, Cmd.none )

                MRI lesionUrl mriForm ->
                    case result of
                        Ok _ ->
                            ( MRI lesionUrl { mriForm | status = Success }, Cmd.none )

                        Err _ ->
                            ( MRI lesionUrl { mriForm | status = Error "Failed submission" }, Cmd.none )

                PSMA lesionUrl psmaForm ->
                    case result of
                        Ok _ ->
                            ( PSMA lesionUrl { psmaForm | status = Success }, Cmd.none )

                        Err _ ->
                            ( PSMA lesionUrl { psmaForm | status = Error "Failed submission" }, Cmd.none )

                Pathology lesionUrl pathologyForm ->
                    case result of
                        Ok _ ->
                            ( Pathology lesionUrl { pathologyForm | status = Success }, Cmd.none )

                        Err _ ->
                            ( Pathology lesionUrl { pathologyForm | status = Error "Failed submission" }, Cmd.none )

        ( page, SubmitEntry ) ->
            let
                markIndexLesion form =
                    case form.lesions of
                        [] ->
                            form.lesions

                        first :: rest ->
                            { first | isIndex = True } :: rest
            in
            case page of
                SelectForm _ ->
                    ( model, Cmd.none )

                MRI lesionUrl mriForm ->
                    ( MRI lesionUrl { mriForm | status = Sending }
                    , postMriForm { mriForm | lesions = markIndexLesion mriForm }
                    )

                PSMA lesionUrl psmaForm ->
                    ( PSMA lesionUrl { psmaForm | status = Sending }
                    , postPsmaForm { psmaForm | lesions = markIndexLesion psmaForm }
                    )

                Pathology lesionUrl pathologyForm ->
                    ( Pathology lesionUrl { pathologyForm | status = Sending }
                    , postPathologyForm { pathologyForm | lesions = markIndexLesion pathologyForm }
                    )

        ( page, ResetForm ) ->
            case page of
                SelectForm _ ->
                    ( model, Cmd.none )

                MRI lesionUrl _ ->
                    ( MRI lesionUrl initMriForm, Cmd.none )

                PSMA lesionUrl _ ->
                    ( PSMA lesionUrl initPsmaForm, Cmd.none )

                Pathology lesionUrl _ ->
                    ( Pathology lesionUrl initPathologyForm, Cmd.none )

        ( _, GotoPage page ) ->
            case page of
                SelectPage ->
                    ( SelectForm lesionMapUrl, Cmd.none )

                MriPage ->
                    ( MRI lesionMapUrl initMriForm, Cmd.none )

                PsmaPage ->
                    ( PSMA lesionMapUrl initPsmaForm, Cmd.none )

                PathologyPage ->
                    ( Pathology lesionMapUrl initPathologyForm, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


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


choiceField : String -> List ( String, String ) -> (String -> Msg) -> String -> Html Msg
choiceField label options handler selected =
    let
        viewOption =
            \( val, lab ) -> H.option [ A.value val, A.selected (val == selected) ] [ text lab ]
    in
    genericField label <|
        [ H.select
            [ class "border w-48 mr-16"
            , Evt.onInput handler
            ]
          <|
            List.map viewOption options
        ]


type alias Coords =
    List Int


type alias AreaName =
    String


type alias Shape =
    String


singleRegionOverlay : LesionOverlay -> AreaName -> String -> Coords -> Shape -> Html Msg
singleRegionOverlay lesionOverlay name label coords shape =
    let
        mriOrPsmaLoc =
            case lesionOverlay of
                MriOverlay mriLesion ->
                    mriLesion.location

                PsmaOverlay psmaLesion ->
                    psmaLesion.location

        onClick =
            case lesionOverlay of
                MriOverlay mriLesion ->
                    \n l -> SE.onClick <| UpdateMriForm (UpdateMriLesion mriLesion (ClickMapMri n l))

                PsmaOverlay psmaLesion ->
                    \n l -> SE.onClick <| UpdatePsmaForm (UpdatePsmaLesion psmaLesion (ClickMapPsma n l))

        hoverColorClass =
            SA.class "text-transparent hover:text-red-600 fill-current"

        selectedColorClass =
            SA.class <|
                if List.any (\loc -> name == loc.name) mriOrPsmaLoc then
                    "text-red-600"

                else
                    ""
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
                , hoverColorClass
                , selectedColorClass
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
                        , hoverColorClass
                        , selectedColorClass
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
                        , hoverColorClass
                        , selectedColorClass
                        ]
                        []

                _ ->
                    S.text ""

        _ ->
            S.text ""


viewOverlay : LesionOverlay -> List (Html Msg)
viewOverlay lesionOverlay =
    [ singleRegionOverlay
        lesionOverlay
        "r_base_afs"
        "R base AFS"
        [ 515, 235, 515, 222, 515, 200, 493, 201, 458, 216, 437, 239, 451, 248, 470, 218, 498, 215 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_base_afs"
        "L base AFS"
        [ 522, 233, 521, 199, 550, 202, 580, 213, 598, 233, 588, 255, 570, 222, 542, 214, 532, 223 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "r_base_pza"
        "R base PZa"
        [ 436, 239, 426, 266, 421, 295, 460, 281, 451, 259, 450, 248 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_base_pza"
        "L base PZa"
        [ 589, 253, 600, 238, 614, 265, 620, 294, 582, 283 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "r_base_tza"
        "R base TZa"
        [ 453, 249, 471, 220, 496, 217, 514, 237, 516, 281, 460, 281 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_base_tza"
        "L base TZa"
        [ 523, 235, 542, 216, 567, 223, 585, 255, 580, 280, 522, 281 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "r_base_tzp"
        "R base TZa"
        [ 515, 298, 469, 286 ]
        "rect"
    , singleRegionOverlay
        lesionOverlay
        "l_base_tzp"
        "L base TZa"
        [ 567, 298, 521, 286 ]
        "rect"
    , singleRegionOverlay
        lesionOverlay
        "r_base_pzpl"
        "R base PZpl"
        [ 459, 286, 478, 344, 452, 338, 430, 321, 423, 296 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_base_pzpl"
        "L base PZpl"
        [ 582, 286, 616, 298, 611, 318, 590, 335, 563, 344 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "r_base_pzpm"
        "R base PZpm"
        [ 474, 318, 483, 344, 512, 346, 489, 335 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_base_pzpm"
        "L base PZpm"
        [ 567, 318, 557, 345, 525, 346, 549, 336 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "r_base_cz"
        "R base CZ"
        [ 470, 299, 478, 317, 489, 330, 516, 344, 516, 300 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_base_cz"
        "L base CZ"
        [ 522, 301, 571, 301, 566, 315, 546, 334, 522, 344 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "r_sv"
        "R SV"
        [ 504, 112, 484, 110, 452, 115, 429, 130, 414, 151, 418, 172, 443, 174, 474, 153, 504, 127 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_sv"
        "L SV"
        [ 524, 113, 525, 123, 548, 147, 580, 160, 597, 170, 621, 163, 616, 138, 592, 119, 564, 110 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "r_mid_afs"
        "R mid AFS"
        [ 517, 394, 517, 433, 510, 416, 497, 410, 478, 416, 471, 409, 492, 397 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_mid_afs"
        "L mid AFS"
        [ 521, 394, 520, 430, 530, 415, 545, 409, 559, 415, 567, 410, 544, 399 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "r_mid_tza"
        "R mid TZa"
        [ 513, 467, 515, 440, 506, 417, 496, 413, 476, 418, 462, 441, 458, 467 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_mid_tza"
        "L mid TZa"
        [ 579, 467, 576, 442, 566, 425, 545, 412, 533, 417, 522, 437, 525, 467 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "r_mid_pza"
        "R mid PZa"
        [ 454, 468, 456, 440, 474, 417, 467, 412, 444, 437, 432, 469 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_mid_pza"
        "L mid PZa"
        [ 580, 467, 603, 467, 592, 436, 568, 412, 562, 417, 578, 441 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "r_mid_pzpl"
        "R mid PZpl"
        [ 455, 473, 432, 473, 437, 505, 453, 525, 475, 533 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_mid_pzpl"
        "L mid PZpl"
        [ 582, 474, 603, 474, 599, 499, 588, 519, 561, 532 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "r_mid_tzp"
        "R mid TZp"
        [ 461, 473, 508, 473, 500, 488, 483, 495, 467, 488 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_mid_tzp"
        "L mid TZp"
        [ 526, 473, 574, 472, 571, 487, 554, 497, 535, 485 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "r_mid_pzpm"
        "R mid PZpm"
        [ 468, 494, 482, 497, 507, 489, 512, 497, 514, 535, 494, 536, 478, 532 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_mid_pzpm"
        "L mid PZpm"
        [ 569, 492, 553, 500, 534, 489, 522, 497, 521, 535, 539, 538, 559, 532 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "r_apex_afs"
        "R apex AFS"
        [ 516, 581, 516, 619, 506, 606, 497, 608, 495, 596, 482, 591 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_apex_afs"
        "L apex AFS"
        [ 520, 581, 520, 619, 528, 606, 541, 611, 541, 597, 551, 590 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "r_apex_pza"
        "R apex PZa"
        [ 456, 632, 465, 609, 481, 594, 492, 599, 492, 608, 486, 621 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_apex_pza"
        "L apex PZa"
        [ 581, 631, 552, 623, 544, 608, 545, 600, 555, 596, 570, 610 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "r_apex_tza"
        "R apex TZa"
        [ 489, 621, 497, 610, 510, 611, 515, 621 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_apex_tza"
        "L apex TZa"
        [ 522, 621, 546, 621, 541, 611, 529, 612 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "r_apex_pzpl"
        "R apex PZpl"
        [ 454, 636, 458, 670, 471, 690, 487, 694, 467, 631 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_apex_pzpl"
        "L apex PZpl"
        [ 570, 633, 584, 637, 581, 666, 570, 688, 551, 692 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "r_apex_pzpm"
        "R apex PZpm"
        [ 482, 627, 473, 631, 492, 693, 515, 687, 515, 660, 491, 658, 483, 644 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_apex_pzpm"
        "L apex PZpm"
        [ 554, 628, 566, 632, 545, 691, 522, 687, 523, 660, 550, 657, 554, 645 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "r_apex_tzp"
        "R apex TZp"
        [ 489, 626, 487, 640, 494, 653, 508, 654, 515, 639, 515, 626 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "l_apex_tzp"
        "L apex TZp"
        [ 550, 628, 550, 641, 545, 654, 526, 656, 521, 639, 521, 628 ]
        "poly"
    , singleRegionOverlay
        lesionOverlay
        "urethra"
        "Urethra"
        [ 520, 737, 11 ]
        "circle"
    ]


viewLocationField : LesionOverlay -> Html Msg
viewLocationField lesionOverlay =
    let
        ( locations, handler ) =
            case lesionOverlay of
                MriOverlay mriLesion ->
                    ( mriLesion.location
                    , Evt.onClick (UpdateMriForm (UpdateMriLesion mriLesion ResetMriLesionLocation))
                    )

                PsmaOverlay psmaLesion ->
                    ( psmaLesion.location
                    , Evt.onClick (UpdatePsmaForm (UpdatePsmaLesion psmaLesion ResetPsmaLesionLocation))
                    )

        locationField =
            \{ name, label } ->
                span [ class "border p-1 mx-1 font-mono text-xs text-center rounded cursor-default inline-block w-12 " ] [ text label ]

        resetIcon =
            span
                [ class <|
                    "border rounded-full h-4 w-4 inline-flex justify-center items-center "
                        ++ "text-xs font-mono border-gray-600 text-gray-600 shadow-md m-2 cursor-pointer"
                , handler
                ]
                [ text "x" ]
    in
    case locations of
        [] ->
            genericField "Lesion location:"
                [ span [ class "inline-block font-mono text-xs text-left w-64 mr-0" ] [ text "None selected" ] ]

        locationList ->
            genericField "Lesion location:"
                [ span [ class "inline-block w-64 pl-0 text-left" ] <|
                    List.map locationField locationList
                        ++ [ resetIcon ]
                ]


viewMriLesion : LesionUrl -> MriLesion -> Html Msg
viewMriLesion lesionMapUrl lesion =
    let
        title =
            "Lesion #" ++ String.fromInt lesion.id

        deleteButton =
            H.button
                [ class <| "block border-red-600 p-1 mx-auto mt-6 mb-2 rounded text-sm text-white bg-red-600 shadow"
                , A.type_ "button"
                , Evt.onClick <| UpdateMriForm (DeleteMriLesion lesion)
                ]
                [ text "Delete lesion" ]
    in
    H.section [ class "text-center border mt-6 pb-4 transition-all duration-200 ease-in-out hover:border-2 hover:shadow-md" ]
        [ div [ class "px-auto" ]
            [ div [ class "relative mx-auto", A.style "width" "462px", A.style "height" "551px" ]
                [ H.img [ A.src lesionMapUrl, A.usemap "#imageMap", class "w-full absolute" ] []
                , S.svg [ SA.viewBox "0 0 652 780", SA.class "absolute" ] <|
                    viewOverlay <|
                        MriOverlay lesion
                , p [ class "absolute top-0 left-0 text-center font-bold font-serif text-2xl pt-4 w-full" ] [ text title ]
                ]
            ]
        , div [ class "px-14 text-left" ]
            [ viewLocationField <| MriOverlay lesion
            , numberField "Lesion size (mm):"
                "lesion-size"
                lesion.size
                (UpdateMriForm << UpdateMriLesion lesion << MriLesionSize)
            , numberField "ADC:"
                "adc"
                lesion.adc
                (UpdateMriForm << UpdateMriLesion lesion << ADC)
            , choiceField "PIRADS 2.1 score:"
                [ ( "2", "2" )
                , ( "3", "3" )
                , ( "4", "4" )
                , ( "5", "5" )
                ]
                (UpdateMriForm << UpdateMriLesion lesion << Score)
                lesion.score
            , choiceField "PIRADS 2.1 upgraded?"
                [ ( "PZ DCE", "PZ DCE" )
                , ( "TZ DWI", "TZ DWI" )
                , ( "NO", "No" )
                ]
                (UpdateMriForm << UpdateMriLesion lesion << Upgraded)
                lesion.upgraded
            ]
        , deleteButton
        ]


viewMriLesions : String -> List MriLesion -> Html Msg
viewMriLesions url lesions =
    div [] <|
        List.map (viewMriLesion url) lesions


viewPsmaLesion : LesionUrl -> PsmaLesion -> Html Msg
viewPsmaLesion lesionMapUrl lesion =
    let
        title =
            "Lesion #" ++ String.fromInt lesion.id

        deleteButton =
            H.button
                [ class <| "block border-red-600 p-1 mx-auto mt-6 mb-2 rounded text-sm text-white bg-red-600 shadow"
                , A.type_ "button"
                , Evt.onClick <| UpdatePsmaForm (DeletePsmaLesion lesion)
                ]
                [ text "Delete lesion" ]
    in
    H.section [ class "text-center border mt-6 pb-4 transition-all duration-200 ease-in-out hover:border-2 hover:shadow-md" ]
        [ div [ class "px-auto" ]
            [ div [ class "relative mx-auto", A.style "width" "462px", A.style "height" "551px" ]
                [ H.img [ A.src lesionMapUrl, A.usemap "#imageMap", class "w-full absolute" ] []
                , S.svg [ SA.viewBox "0 0 652 780", SA.class "absolute" ] <|
                    viewOverlay <|
                        PsmaOverlay lesion
                , p [ class "absolute top-0 left-0 text-center font-bold font-serif text-2xl pt-4 w-full" ] [ text title ]
                ]
            ]
        , div [ class "px-14 text-left" ]
            [ viewLocationField <| PsmaOverlay lesion
            , numberField "SUV:"
                "suv"
                lesion.suv
                (UpdatePsmaForm << UpdatePsmaLesion lesion << SUV)
            ]
        , deleteButton
        ]


viewPathologyLesion : PathologySpecimen -> PathologyLesion -> Html Msg
viewPathologyLesion specimen lesion =
    let
        title =
            "Lesion #" ++ String.fromInt lesion.id

        biopsyFields =
            div []
                [ numberField "Positive core number"
                    "positive-core-number"
                    lesion.positiveCore
                    (UpdatePathologyForm << UpdatePathologyLesion lesion << PositiveCore)
                , numberField "Total core number"
                    "total-core-number"
                    lesion.totalCore
                    (UpdatePathologyForm << UpdatePathologyLesion lesion << TotalCore)
                , numberField "Greatest percentage of cancer (0-100%)"
                    "greatest-cancer-percentage"
                    lesion.greatestPercentage
                    (UpdatePathologyForm << UpdatePathologyLesion lesion << GreatestPercentage)
                ]

        prostatectomyFields =
            div []
                [ numberField "Lesion size (mm)"
                    "pathology-size"
                    lesion.size
                    (UpdatePathologyForm << UpdatePathologyLesion lesion << PathologyLesionSize)
                ]

        specimenSpecificFields =
            case specimen of
                Prostatectomy ->
                    prostatectomyFields

                Biopsy ->
                    biopsyFields

        deleteButton =
            H.button
                [ class <| "block border-red-600 p-1 mx-auto mt-6 mb-2 rounded text-sm text-white bg-red-600 shadow"
                , A.type_ "button"
                , Evt.onClick <| UpdatePathologyForm (DeletePathologyLesion lesion)
                ]
                [ text "Delete lesion" ]
    in
    H.section [ class "text-center border mt-6 pb-4 transition-all duration-200 ease-in-out hover:border-2" ]
        [ p [ class "px-auto text-center font-bold font-serif text-2xl py-4 w-full" ] [ text title ]
        , div [ class "px-14 text-left" ]
            [ choiceField "Side"
                [ ( "NA", "Not stated" ), ( "RIGHT", "Right" ), ( "LEFT", "Left" ) ]
                (UpdatePathologyForm << UpdatePathologyLesion lesion << PathologySide)
                lesion.locSide
            , choiceField "Zone"
                [ ( "NA", "Not stated" ), ( "BASE", "Base" ), ( "MID", "Mid" ), ( "APEX", "Apex" ) ]
                (UpdatePathologyForm << UpdatePathologyLesion lesion << PathologyZone)
                lesion.locZone
            , choiceField "Lesion grade:"
                [ ( "NONE", "No malignancy" )
                , ( "ISUP 1", "ISUP 1" )
                , ( "ISUP 2", "ISUP 2" )
                , ( "ISUP 3", "ISUP 3" )
                , ( "ISUP 4", "ISUP 4" )
                , ( "ISUP 5", "ISUP 5" )
                ]
                (UpdatePathologyForm << UpdatePathologyLesion lesion << PathologyGrade)
                lesion.grade
            , specimenSpecificFields
            ]
        , deleteButton
        ]


viewPsmaLesions : String -> List PsmaLesion -> Html Msg
viewPsmaLesions url lesions =
    div [] <|
        List.map (viewPsmaLesion url) lesions


viewPathologyLesions : PathologySpecimen -> List PathologyLesion -> Html Msg
viewPathologyLesions specimenType lesions =
    div [] <|
        List.map (viewPathologyLesion specimenType) lesions


newLesionButton : List a -> Pages -> Int -> Html Msg
newLesionButton lesions page limit =
    let
        lesionNumber =
            List.length lesions

        buttonText =
            if lesionNumber == 0 then
                "Add index lesion"

            else if lesionNumber < limit then
                "Add an additional lesion"

            else
                "Lesion number limit reached"

        handler =
            case page of
                MriPage ->
                    Evt.onClick <| UpdateMriForm AddMriLesion

                PsmaPage ->
                    Evt.onClick <| UpdatePsmaForm AddPsmaLesion

                PathologyPage ->
                    Evt.onClick <| UpdatePathologyForm AddPathologyLesion

                SelectPage ->
                    class ""
    in
    H.button
        [ class "block mx-auto py-1 px-2 my-8 my-1 "
        , class
            (if lesionNumber >= limit then
                "text-black text-sm underline cursor-default"

             else
                "bg-green-600 text-gray-200 border rounded text-lg shadow-md"
            )
        , A.type_ "button"
        , A.disabled (lesionNumber >= limit)
        , handler
        ]
        [ text buttonText ]


submitButton : GenericForm a -> Html Msg
submitButton form =
    let
        baseStyle =
            class "block border rounded py-1 px-2 my-4 mx-auto my-1 text-lg shadow-md"
    in
    case form.status of
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
            div []
                [ H.button
                    [ baseStyle
                    , class "border-red-600 bg-red-600 text-white"
                    , A.type_ "button"
                    , Evt.onClick SubmitEntry
                    ]
                    [ text "Error - Click to retry" ]
                , H.p [ class "text-xs text-center text-red-600" ] [ text "please check your form" ]
                ]

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


toMenuButton : Html Msg
toMenuButton =
    let
        baseStyle =
            class "block border rounded shadow py-1 px-2 my-4 mx-auto my-1 text-sm text-gray-600"
    in
    H.button
        [ baseStyle
        , class "text-black"
        , A.type_ "button"
        , Evt.onClick <| GotoPage SelectPage
        ]
        [ text "Select a different form" ]


formFooter : GenericForm a -> Html Msg
formFooter form =
    div []
        [ submitButton form
        , resetButton
        , toMenuButton
        ]


viewMriForm : LesionUrl -> MriForm -> Html Msg
viewMriForm lesionMapUrl form =
    H.form [ class "container mx-auto max-w-lg my-10" ]
        [ H.h1 [ class "font-serif text-3xl text-center pb-10" ] [ text "R-PEDAL MRI Data Entry" ]
        , H.section [ class "text-center pb-4" ]
            [ div [ class "px-14" ]
                [ inputField "Patient ID:" "patient-id" form.patientId (UpdateMriForm << MriPatientId)
                , dateField "MRI date:" "mri-date" form.mriDate (UpdateMriForm << MriDate)
                , numberField "PSA level:" "psa-level" form.psaLevel (UpdateMriForm << PsaLevel)
                ]
            ]
        , H.hr [] []
        , H.p [ class "font-serif text-sm text-center pt-6" ]
            [ text "The first entry will be considered as the index lesion" ]
        , viewMriLesions lesionMapUrl form.lesions
        , newLesionButton form.lesions MriPage 2
        , choiceField "ECE: "
            [ ( "false", "No" )
            , ( "true", "Yes" )
            ]
            (UpdateMriForm << ECE)
            form.ece
        , choiceField "SVI: "
            [ ( "NO", "No" )
            , ( "LEFT", "Yes - left" )
            , ( "RIGHT", "Yes - right" )
            , ( "BILATERAL", "Yes - bilateral" )
            ]
            (UpdateMriForm << SVI)
            form.svi
        , textAreaField "Any additional findings?" form.comments (UpdateMriForm << MriComments)
        , formFooter form
        ]


viewPsmaForm : LesionUrl -> PsmaForm -> Html Msg
viewPsmaForm lesionMapUrl form =
    H.form [ class "container mx-auto max-w-lg my-10" ]
        [ H.h1 [ class "font-serif text-3xl text-center pb-10" ] [ text "R-PEDAL PSMA Data Entry" ]
        , H.section [ class "text-center pb-4" ]
            [ div [ class "px-14" ]
                [ inputField "Patient ID:" "patient-id" form.patientId (UpdatePsmaForm << PsmaPatientId)
                , dateField "PSMA date:" "psma-date" form.psmaDate (UpdatePsmaForm << PsmaDate)
                ]
            ]
        , H.hr [] []
        , H.p [ class "font-serif text-sm text-center pt-6" ]
            [ text "The first entry will be considered as the index lesion" ]
        , viewPsmaLesions lesionMapUrl form.lesions
        , newLesionButton form.lesions PsmaPage 3
        , textAreaField "Any additional findings?" form.comments (UpdatePsmaForm << PsmaComments)
        , formFooter form
        ]


viewPathologyForm : PathologyForm -> Html Msg
viewPathologyForm form =
    let
        specimenType =
            case form.specimenType of
                Prostatectomy ->
                    "Prostatectomy"

                Biopsy ->
                    "Biopsy"
    in
    H.form [ class "container mx-auto max-w-lg my-10" ]
        [ H.h1 [ class "font-serif text-3xl text-center pb-10" ] [ text "R-PEDAL Pathology Data Entry" ]
        , H.section [ class "text-center pb-4" ]
            [ div [ class "px-14" ]
                [ inputField "Patient ID:" "patient-id" form.patientId (UpdatePathologyForm << PathologyPatientId)
                , dateField "Procedure date:" "procedure-date" form.procedureDate (UpdatePathologyForm << ProcedureDate)
                , choiceField "Specimen type:"
                    [ ( "Biopsy", "Biopsy" )
                    , ( "Prostatectomy", "Prostatectomy" )
                    ]
                    (UpdatePathologyForm << SpecimenType)
                    specimenType
                ]
            ]
        , H.p [ class "font-serif text-sm text-center pt-6" ]
            [ text "The first entry will be considered as the index lesion" ]
        , viewPathologyLesions form.specimenType form.lesions
        , newLesionButton form.lesions PathologyPage 3
        , textAreaField "Any additional findings?" form.comments (UpdatePathologyForm << PathologyComments)
        , formFooter form
        ]


view : Model -> Html Msg
view model =
    case model of
        SelectForm _ ->
            H.div [ class "flex justify-around w-full my-20" ]
                [ H.div [ class "border-4 border-solid rounded text-2xl p-8 cursor-pointer", Evt.onClick (GotoPage MriPage) ] [ text "MRI Form" ]
                , H.div [ class "border-4 border-solid rounded text-2xl p-8 cursor-pointer", Evt.onClick (GotoPage PsmaPage) ] [ text "PSMA Form" ]
                , H.div [ class "border-4 border-solid rounded text-2xl p-8 cursor-pointer", Evt.onClick (GotoPage PathologyPage) ] [ text "Pathology Form" ]
                ]

        MRI lesionUrl mriForm ->
            viewMriForm lesionUrl mriForm

        PSMA lesionUrl psmaForm ->
            viewPsmaForm lesionUrl psmaForm

        Pathology _ pathologyForm ->
            viewPathologyForm pathologyForm



---- PROGRAM ----


main : Program String Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
