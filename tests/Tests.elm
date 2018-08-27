module Tests exposing (hsluvTests)

import Array exposing (Array)
import Expect exposing (FloatingPointTolerance(..), equal)
import HSLuv exposing (..)
import HSLuv.Color exposing (Color)
import Json.Decode as Decode exposing (Decoder)
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
    Test.describe "HSLuv references" referenceTests


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
            List.map referenceTest refs

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
            toRgb255 color

        check a b =
            Expect.equal a (round (255 * b))
    in
    [ test "r" <| \_ -> check red b0
    , test "g" <| \_ -> check green b1
    , test "b" <| \_ -> check blue b2
    , test "a" <| \_ -> Expect.within (Absolute 0.000000001) 0.5 alpha
    ]
