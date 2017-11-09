module Tests exposing (hsluvTests)

import Test exposing (..)
import Expect exposing (equal, FloatingPointTolerance(..))
import HSLuv exposing (..)
import SnapshotRev4 exposing (referenceString)
import Json.Decode as Decode exposing (Decoder)
import Array exposing (Array)


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

        toRef lch luv rgb xyz hpluv hsluv =
            { hex = ""
            , lch = toTuple lch
            , luv = toTuple luv
            , rgb = toTuple rgb
            , xyz = toTuple xyz
            , hpluv = toTuple hpluv
            , hsluv = toTuple hsluv
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
        [ Test.describe "hsluvToRgb"
            (tupleTests (hsluvToRgb (ref.hsluv)) ref.rgb)
        , Test.describe "hpluvToRgb"
            (tupleTests (hpluvToRgb (ref.hpluv)) ref.rgb)
        , Test.describe "rgbToHsluv"
            (tupleTests (rgbToHsluv (ref.rgb)) ref.hsluv)
        , Test.describe "rgbToHpluv"
            (tupleTests (rgbToHpluv (ref.rgb)) ref.hpluv)
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
