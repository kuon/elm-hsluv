module Tests exposing (hsluvTests)

import Array exposing (Array)
import Expect exposing (FloatingPointTolerance(..), equal)
import HSLuv exposing (..)
import HSLuv.Manipulate exposing (..)
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import SnapshotRev4 exposing (referenceString)
import Test exposing (..)


type alias Reference =
    { hex : String
    , lch : ( Float, Float, Float )
    , luv : ( Float, Float, Float )
    , rgb : ( Float, Float, Float )
    , xyz : ( Float, Float, Float )
    , hpluv : ( Float, Float, Float )
    , hsluv : ( Float, Float, Float )
    }


hsluvTests : Test
hsluvTests =
    Test.describe "HSLuv"
        [ Test.describe "references" referenceTests
        , Test.describe "manipulate" manipulateTests
        ]


referencesDecoder : Decoder (List Reference)
referencesDecoder =
    let
        toRef ( hex, ref ) =
            { ref | hex = hex }
    in
    Decode.keyValuePairs referenceDecoder
        |> Decode.map (List.map toRef)


referenceDecoder : Decoder Reference
referenceDecoder =
    let
        decodeArray =
            Decode.array Decode.float

        lch =
            Decode.field "lch" decodeArray

        luv =
            Decode.field "luv" decodeArray

        rgb =
            Decode.field "rgb" decodeArray

        xyz =
            Decode.field "xyz" decodeArray

        hpluv =
            Decode.field "hpluv" decodeArray

        hsluv =
            Decode.field "hsluv" decodeArray

        toTuple components =
            case
                ( Array.get 0 components
                , Array.get 1 components
                , Array.get 2 components
                )
            of
                ( Just a, Just b, Just c ) ->
                    ( a, b, c )

                _ ->
                    ( 0, 0, 0 )

        toRef lch_ luv_ rgb_ xyz_ hpluv_ hsluv_ =
            { hex = ""
            , lch = toTuple lch_
            , luv = toTuple luv_
            , rgb = toTuple rgb_
            , xyz = toTuple xyz_
            , hpluv = toTuple hpluv_
            , hsluv = toTuple hsluv_
            }
    in
    Decode.map6 toRef lch luv rgb xyz hpluv hsluv


referenceTests : List Test
referenceTests =
    case Decode.decodeString referencesDecoder referenceString of
        Ok refs ->
            List.indexedMap
                (\i items ->
                    Test.describe ("chunk " ++ String.fromInt i) <| List.map referenceTest items
                )
                (List.Extra.greedyGroupsOf 100 refs)

        Err _ ->
            [ test "failed decoding" <|
                \_ -> Expect.fail "reference decoding failed"
            ]


referenceTest : Reference -> Test
referenceTest ref =
    Test.describe ref.hex
        [ Test.describe "lchToLuv"
            (tupleTests (lchToLuv ref.lch) ref.luv)
        , Test.describe "luvToLch"
            (tupleTests (luvToLch ref.luv) ref.lch)
        , Test.describe "xyzToRgb"
            (tupleTests (xyzToRgb ref.xyz) ref.rgb)
        , Test.describe "rgbToXyz"
            (tupleTests (rgbToXyz ref.rgb) ref.xyz)
        , Test.describe "xyzToLuv"
            (tupleTests (xyzToLuv ref.xyz) ref.luv)
        , Test.describe "luvToXyz"
            (tupleTests (luvToXyz ref.luv) ref.xyz)
        , Test.describe "hsluvToLch"
            (tupleTests (hsluvToLch ref.hsluv) ref.lch)
        , Test.describe "lchToHsluv"
            (tupleTests (lchToHsluv ref.lch) ref.hsluv)
        , Test.describe "hpluvToLch"
            (tupleTests (hpluvToLch ref.hpluv) ref.lch)
        , Test.describe "lchToHpluv"
            (tupleTests (lchToHpluv ref.lch) ref.hpluv)
        , Test.describe "hsluvToRgb"
            (tupleTests (hsluvToRgb ref.hsluv) ref.rgb)
        , Test.describe "hpluvToRgb"
            (tupleTests (hpluvToRgb ref.hpluv) ref.rgb)
        , Test.describe "rgbToHsluv"
            (tupleTests (rgbToHsluv ref.rgb) ref.hsluv)
        , Test.describe "rgbToHpluv"
            (tupleTests (rgbToHpluv ref.rgb) ref.hpluv)
        , Test.describe "hsluv"
            (colorTests ref.hsluv ref.rgb)
        ]


tupleTests : ( Float, Float, Float ) -> ( Float, Float, Float ) -> List Test
tupleTests ( a0, a1, a2 ) ( b0, b1, b2 ) =
    let
        check =
            Expect.within (Absolute 0.000000001)
    in
    [ test "0" <| \_ -> check a0 b0
    , test "1" <| \_ -> check a1 b1
    , test "2" <| \_ -> check a2 b2
    ]


colorTests : ( Float, Float, Float ) -> ( Float, Float, Float ) -> List Test
colorTests ( a0, a1, a2 ) ( b0, b1, b2 ) =
    let
        color =
            hsluv360 { hue = a0, saturation = a1, lightness = a2, alpha = 0.5 }

        { red, green, blue, alpha } =
            toRgba color

        check a b =
            Expect.within (Absolute 0.000000001) a b
    in
    [ test "r" <| \_ -> check red b0
    , test "g" <| \_ -> check green b1
    , test "b" <| \_ -> check blue b2
    , test "a" <| \_ -> check alpha 0.5
    ]


manipulateTests : List Test
manipulateTests =
    let
        check =
            Expect.within (Absolute 0.000000001)

        color =
            hsluv360 { hue = 100, saturation = 25, lightness = 86, alpha = 0.5 }
    in
    [ test "set red" <|
        \_ ->
            let
                newColor =
                    setRed 0.4 color

                { red, green, blue, alpha } =
                    toRgba newColor
            in
            check red 0.4
    , test "set green" <|
        \_ ->
            let
                newColor =
                    setGreen 0.3 color

                { red, green, blue, alpha } =
                    toRgba newColor
            in
            check green 0.3
    , test "set blue" <|
        \_ ->
            let
                newColor =
                    setBlue 0.8 color

                { red, green, blue, alpha } =
                    toRgba newColor
            in
            check blue 0.8
    , test "set alpha" <|
        \_ ->
            let
                newColor =
                    setAlpha 0.7 color

                { red, green, blue, alpha } =
                    toRgba newColor
            in
            check alpha 0.7
    , test "set hue" <|
        \_ ->
            let
                newColor =
                    setHue 0.25 color

                { hue, saturation, lightness, alpha } =
                    toHsluv newColor
            in
            check hue 0.25
    , test "set saturation" <|
        \_ ->
            let
                newColor =
                    setSaturation 0.9 color

                { hue, saturation, lightness, alpha } =
                    toHsluv newColor
            in
            check saturation 0.9
    , test "set lightness" <|
        \_ ->
            let
                newColor =
                    setLightness 0.2 color

                { hue, saturation, lightness, alpha } =
                    toHsluv newColor
            in
            check lightness 0.2
    , test "mult red" <|
        \_ ->
            let
                newColor =
                    multRed 0.5 color

                { red, green, blue, alpha } =
                    toRgba newColor
            in
            check red 0.409821226702
    , test "mult green" <|
        \_ ->
            let
                newColor =
                    multGreen 1.1 color

                { red, green, blue, alpha } =
                    toRgba newColor
            in
            check green 0.9467682225141888
    , test "mult blue" <|
        \_ ->
            let
                newColor =
                    multBlue 0.2 color

                { red, green, blue, alpha } =
                    toRgba newColor
            in
            check blue 0.145375922330956
    , test "mult alpha" <|
        \_ ->
            let
                newColor =
                    multAlpha 1.2 color

                { red, green, blue, alpha } =
                    toRgba newColor
            in
            check alpha 0.6
    , test "mult hue" <|
        \_ ->
            let
                newColor =
                    multHue 0.72 color

                { hue, saturation, lightness, alpha } =
                    toHsluv newColor
            in
            check hue 0.2
    , test "mult saturation" <|
        \_ ->
            let
                newColor =
                    multSaturation 2 color

                { hue, saturation, lightness, alpha } =
                    toHsluv newColor
            in
            check saturation 0.5
    , test "mult lightness" <|
        \_ ->
            let
                newColor =
                    multLightness 0.6 color

                { hue, saturation, lightness, alpha } =
                    toHsluv newColor
            in
            check lightness 0.516
    ]
