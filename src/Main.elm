module Main exposing (main)

import Browser
import Html as H exposing (Html, button, div, input, label, node, p, span, text)
import Html.Attributes as A exposing (alt, class, coords, name, shape, title)
import Html.Events exposing (onClick)


type alias Model =
    { lesionMapUrl : String }


init : String -> ( Model, Cmd Msg )
init lesionMapUrl =
    ( Model lesionMapUrl, Cmd.none )


initialModel : Model
initialModel =
    Model ""


type alias Area =
    String


type Msg
    = ClickArea Area


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickArea area ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    H.form []
        [ H.h1 [] [ text "R-PEDAL MRI Data Entry" ]
        , H.section []
            [ p []
                [ label []
                    [ span [] [ text "Patient ID" ]
                    , input [ A.type_ "text", A.name "patient-id", class "border" ] []
                    ]
                ]
            , p []
                [ label []
                    [ span [] [ text "MRI date" ]
                    , input [ A.type_ "text", A.name "mri-date", class "border" ] []
                    ]
                ]
            , p []
                [ label []
                    [ span [] [ text "PSA level" ]
                    , input [ A.type_ "number", A.name "psa-level", class "border" ] []
                    ]
                ]
            , p []
                [ label []
                    [ span [] [ text "Is an index lesion present?" ]
                    , H.select []
                        [ H.option [] [ text "Yes" ]
                        , H.option [] [ text "No" ]
                        ]
                    ]
                ]
            ]
        , H.section []
            [ H.h2 [] [ text "Index lesion" ]
            , div []
                [ H.img [ A.src model.lesionMapUrl, A.usemap "#imageMap", class "" ] []
                , lesionMap
                ]
            , p []
                [ label []
                    [ span [] [ text "Lesion size" ]
                    , input [ A.type_ "number", A.name "lesion-size", class "border" ] []
                    ]
                ]
            , p []
                [ label []
                    [ span [] [ text "ADC" ]
                    , input [ A.type_ "text", A.name "adc", class "border" ] []
                    ]
                ]
            , p []
                [ label []
                    [ span [] [ text "PIRADS 2.1 score" ]
                    , H.select []
                        [ H.option [] [ text "2" ]
                        , H.option [] [ text "3" ]
                        , H.option [] [ text "4" ]
                        , H.option [] [ text "5" ]
                        ]
                    ]
                ]
            , p []
                [ label []
                    [ span [] [ text "PIRADS 2.1 upgraded?" ]
                    , H.select []
                        [ H.option [] [ text "Yes" ]
                        , H.option [] [ text "Yes" ]
                        , H.option [] [ text "No" ]
                        ]
                    ]
                ]
            ]
        ]


lesionMap : Html Msg
lesionMap =
    node "map"
        [ name "imageMap", A.id "imageMap" ]
        [ node "area"
            [ alt "r_base_afs"
            , coords "515,235,515,222,515,200,493,201,458,216,437,239,451,248,470,218,498,215"
            , shape "poly"
            , title "r_base_afs"
            , onClick <| ClickArea "r_base_afs"
            ]
            []
        , node "area"
            [ alt "l_base_afs"
            , coords "522,233,521,199,550,202,580,213,598,233,588,255,570,222,542,214,532,223"
            , shape "poly"
            , title "l_base_afs"
            , onClick <| ClickArea "l_base_afs"
            ]
            []
        , node "area"
            [ alt "r_base_pza"
            , coords "436,239,426,266,421,295,460,281,451,259,450,248"
            , shape "poly"
            , title "r_base_pza"
            , onClick <| ClickArea "r_base_pza"
            ]
            []
        , node "area"
            [ alt "l_base_pza"
            , coords "589,253,600,238,614,265,620,294,582,283"
            , shape "poly"
            , title "l_base_pza"
            , onClick <| ClickArea "l_base_pza"
            ]
            []
        , node "area"
            [ alt "r_base_tza"
            , coords "453,249,471,220,496,217,514,237,516,281,460,281"
            , shape "poly"
            , title "r_base_tza"
            , onClick <| ClickArea "r_base_tza"
            ]
            []
        , node "area"
            [ alt "l_base_tza"
            , coords "523,235,542,216,567,223,585,255,580,280,522,281"
            , shape "poly"
            , title "l_base_tza"
            , onClick <| ClickArea "l_base_tza"
            ]
            []
        , node "area"
            [ alt "r_base_tzp"
            , coords "515,298,469,286"
            , shape "rect"
            , title "r_base_tzp"
            , onClick <| ClickArea "r_base_tzp"
            ]
            []
        , node "area"
            [ alt "l_base_tzp"
            , coords "567,298,521,286"
            , shape "rect"
            , title "l_base_tzp"
            , onClick <| ClickArea "l_base_tzp"
            ]
            []
        , node "area"
            [ alt "r_base_pzpl"
            , coords "459,286,478,344,452,338,430,321,423,296"
            , shape "poly"
            , title "r_base_pzpl"
            , onClick <| ClickArea "r_base_pzpl"
            ]
            []
        , node "area"
            [ alt "l_base_pzpl"
            , coords "582,286,616,298,611,318,590,335,563,344"
            , shape "poly"
            , title "l_base_pzpl"
            , onClick <| ClickArea "l_base_pzpl"
            ]
            []
        , node "area"
            [ alt "r_base_pzpm"
            , coords "474,318,483,344,512,346,489,335"
            , shape "poly"
            , title "r_base_pzpm"
            , onClick <| ClickArea "r_base_pzpm"
            ]
            []
        , node "area"
            [ alt "l_base_pzpm"
            , coords "567,318,557,345,525,346,549,336"
            , shape "poly"
            , title "l_base_pzpm"
            , onClick <| ClickArea "l_base_pzpm"
            ]
            []
        , node "area"
            [ alt "r_base_cz"
            , coords "470,299,478,317,489,330,516,344,516,300"
            , shape "poly"
            , title "r_base_cz"
            , onClick <| ClickArea "r_base_cz"
            ]
            []
        , node "area"
            [ alt "l_base_cz"
            , coords "522,301,571,301,566,315,546,334,522,344"
            , shape "poly"
            , title "l_base_cz"
            , onClick <| ClickArea "l_base_cz"
            ]
            []
        , node "area"
            [ alt "r_sv"
            , coords "504,112,484,110,452,115,429,130,414,151,418,172,443,174,474,153,504,127"
            , shape "poly"
            , title "r_sv"
            , onClick <| ClickArea "r_sv"
            ]
            []
        , node "area"
            [ alt "l_sv"
            , coords "524,113,525,123,548,147,580,160,597,170,621,163,616,138,592,119,564,110"
            , shape "poly"
            , title "l_sv"
            , onClick <| ClickArea "l_sv"
            ]
            []
        , node "area"
            [ alt "r_mid_afs"
            , coords "517,394,517,433,510,416,497,410,478,416,471,409,492,397"
            , shape "poly"
            , title "r_mid_afs"
            , onClick <| ClickArea "r_mid_afs"
            ]
            []
        , node "area"
            [ alt "l_mid_afs"
            , coords "521,394,520,430,530,415,545,409,559,415,567,410,544,399"
            , shape "poly"
            , title "l_mid_afs"
            , onClick <| ClickArea "l_mid_afs"
            ]
            []
        , node "area"
            [ alt "r_mid_tza"
            , coords "513,467,515,440,506,417,496,413,476,418,462,441,458,467"
            , shape "poly"
            , title "r_mid_tza"
            , onClick <| ClickArea "r_mid_tza"
            ]
            []
        , node "area"
            [ alt "l_mid_tza"
            , coords "579,467,576,442,566,425,545,412,533,417,522,437,525,467"
            , shape "poly"
            , title "l_mid_tza"
            , onClick <| ClickArea "l_mid_tza"
            ]
            []
        , node "area"
            [ alt "r_mid_pza"
            , coords "454,468,456,440,474,417,467,412,444,437,432,469"
            , shape "poly"
            , title "r_mid_pza"
            , onClick <| ClickArea "r_mid_pza"
            ]
            []
        , node "area"
            [ alt "l_mid_pza"
            , coords "580,467,603,467,592,436,568,412,562,417,578,441"
            , shape "poly"
            , title "l_mid_pza"
            , onClick <| ClickArea "l_mid_pza"
            ]
            []
        , node "area"
            [ alt "r_mid_pzpl"
            , coords "455,473,432,473,437,505,453,525,475,533"
            , shape "poly"
            , title "r_mid_pzpl"
            , onClick <| ClickArea "r_mid_pzpl"
            ]
            []
        , node "area"
            [ alt "l_mid_pzpl"
            , coords "582,474,603,474,599,499,588,519,561,532"
            , shape "poly"
            , title "l_mid_pzpl"
            , onClick <| ClickArea "l_mid_pzpl"
            ]
            []
        , node "area"
            [ alt "r_mid_tzp"
            , coords "461,473,508,473,500,488,483,495,467,488"
            , shape "poly"
            , title "r_mid_tzp"
            , onClick <| ClickArea "r_mid_tzp"
            ]
            []
        , node "area"
            [ alt "l_mid_tzp"
            , coords "526,473,574,472,571,487,554,497,535,485"
            , shape "poly"
            , title "l_mid_tzp"
            , onClick <| ClickArea "l_mid_tzp"
            ]
            []
        , node "area"
            [ alt "r_mid_pzpm"
            , coords "468,494,482,497,507,489,512,497,514,535,494,536,478,532"
            , shape "poly"
            , title "r_mid_pzpm"
            , onClick <| ClickArea "r_mid_pzpm"
            ]
            []
        , node "area"
            [ alt "l_mid_pzpm"
            , coords "569,492,553,500,534,489,522,497,521,535,539,538,559,532"
            , shape "poly"
            , title "l_mid_pzpm"
            , onClick <| ClickArea "l_mid_pzpm"
            ]
            []
        , node "area"
            [ alt "apex_r-afs"
            , coords "516,581,516,619,506,606,497,608,495,596,482,591"
            , shape "poly"
            , title "apex_r_afs"
            , onClick <| ClickArea "apex_r_afs"
            ]
            []
        , node "area"
            [ alt "apex_l_afs"
            , coords "520,581,520,619,528,606,541,611,541,597,551,590"
            , shape "poly"
            , title "apex_l_afs"
            , onClick <| ClickArea "apex_l_afs"
            ]
            []
        , node "area"
            [ alt "apex_r_pza"
            , coords "456,632,465,609,481,594,492,599,492,608,486,621"
            , shape "poly"
            , title "apex_r_pza"
            , onClick <| ClickArea "apex_r_pza"
            ]
            []
        , node "area"
            [ alt "apex_l_pza"
            , coords "581,631,552,623,544,608,545,600,555,596,570,610"
            , shape "poly"
            , title "apex_l_pza"
            , onClick <| ClickArea "apex_l_pza"
            ]
            []
        , node "area"
            [ alt "apex_r_tza"
            , coords "489,621,497,610,510,611,515,621"
            , shape "poly"
            , title "apex_r_tza"
            , onClick <| ClickArea "apex_r_tza"
            ]
            []
        , node "area"
            [ alt "apex_l_tza"
            , coords "522,621,546,621,541,611,529,612"
            , shape "poly"
            , title "apex_l_tza"
            , onClick <| ClickArea "apex_l_tza"
            ]
            []
        , node "area"
            [ alt "apex_r_pzpl"
            , coords "454,636,458,670,471,690,487,694,467,631"
            , shape "poly"
            , title "apex_r_pzpl"
            , onClick <| ClickArea "apex_r_pzpl"
            ]
            []
        , node "area"
            [ alt "apex_l_pzpl"
            , coords "570,633,584,637,581,666,570,688,551,692"
            , shape "poly"
            , title "apex_l_pzpl"
            , onClick <| ClickArea "apex_l_pzpl"
            ]
            []
        , node "area"
            [ alt "apex_r_pzpm"
            , coords "482,627,473,631,492,693,515,687,515,660,491,658,483,644"
            , shape "poly"
            , title "apex_r_pzpm"
            , onClick <| ClickArea "apex_r_pzpm"
            ]
            []
        , node "area"
            [ alt "apex_l_pzpm"
            , coords "554,628,566,632,545,691,522,687,523,660,550,657,554,645"
            , shape "poly"
            , title "apex_l_pzpm"
            , onClick <| ClickArea "apex_l_pzpm"
            ]
            []
        , node "area"
            [ alt "apex_r_tzp"
            , coords "489,626,487,640,494,653,508,654,515,639,515,626"
            , shape "poly"
            , title "apex_r_tzp"
            , onClick <| ClickArea "apex_r_tzp"
            ]
            []
        , node "area"
            [ alt "apex_l_tzp"
            , coords "550,628,550,641,545,654,526,656,521,639,521,628"
            , shape "poly"
            , title "apex_l_tzp"
            , onClick <| ClickArea "apex_l_tzp"
            ]
            []
        , node "area"
            [ alt "urethra"
            , coords "520,737,11"
            , shape "circle"
            , title "urethra"
            , onClick <| ClickArea "urethra"
            ]
            []
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
