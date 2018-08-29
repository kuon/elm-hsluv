module HSLuv.Manipulate exposing
    ( setRed, setGreen, setBlue, setHue, setLightness, setSaturation
    , setAlpha
    , multRed, multGreen, multBlue, multHue, multLightness
    , multSaturation, multAlpha
    , mapRed, mapGreen, mapBlue, mapHue, mapLightness
    , mapSaturation, mapAlpha
    )

{-| Manipulate colors

This module let you manipulate the `Color` type.


# Set components

@docs setRed, setGreen, setBlue, setHue, setLightness, setSaturation
@docs setAlpha


# Multiply components

@docs multRed, multGreen, multBlue, multHue, multLightness
@docs multSaturation, multAlpha


# Map components

@docs mapRed, mapGreen, mapBlue, mapHue, mapLightness
@docs mapSaturation, mapAlpha

-}

import HSLuv exposing (..)
import HSLuv.Color exposing (Color(..))


{-| Set the red channel of the color. Requires a value in the 0->1 range.
-}
setRed : Float -> Color -> Color
setRed newRed =
    mapRed (always newRed)


{-| Set the green channel of the color. Requires a value in the 0->1 range.
-}
setGreen : Float -> Color -> Color
setGreen newGreen =
    mapGreen (always newGreen)


{-| Set the blue channel of the color. Requires a value in the 0->1 range.
-}
setBlue : Float -> Color -> Color
setBlue newBlue =
    mapBlue (always newBlue)


{-| Set the hue of the color. Requires a value in the 0->1 range.
-}
setHue : Float -> Color -> Color
setHue newHue =
    mapHue (always newHue)


{-| Set the saturation of the color. Requires a value in the 0->1 range.
-}
setSaturation : Float -> Color -> Color
setSaturation newSaturation =
    mapSaturation (always newSaturation)


{-| Set the lightness of the color. Requires a value in the 0->1 range.
-}
setLightness : Float -> Color -> Color
setLightness newLightness =
    mapLightness (always newLightness)


{-| Set the alpha of the color. Requires a value in the 0->1 range.
-}
setAlpha : Float -> Color -> Color
setAlpha newAlpha =
    mapAlpha (always newAlpha)


{-| Multiply the red channel by the give factor. The result is clamped in the
0->1 range.
-}
multRed : Float -> Color -> Color
multRed factor =
    mapRed ((*) factor)


{-| Multiply the green channel by the give factor. The result is clamped in the
0->1 range.
-}
multGreen : Float -> Color -> Color
multGreen factor =
    mapGreen ((*) factor)


{-| Multiply the blue channel by the give factor. The result is clamped in the
0->1 range.
-}
multBlue : Float -> Color -> Color
multBlue factor =
    mapBlue ((*) factor)


{-| Multiply the hue by the give factor. The result is clamped in the
0->1 range.
-}
multHue : Float -> Color -> Color
multHue factor =
    mapHue ((*) factor)


{-| Multiply the saturation by the give factor. The result is clamped in the
0->1 range.
-}
multSaturation : Float -> Color -> Color
multSaturation factor =
    mapSaturation ((*) factor)


{-| Multiply the lightness by the give factor. The result is clamped in the
0->1 range.
-}
multLightness : Float -> Color -> Color
multLightness factor =
    mapLightness ((*) factor)


{-| Multiply the alpha by the give factor. The result is clamped in the
0->1 range.
-}
multAlpha : Float -> Color -> Color
multAlpha factor =
    mapAlpha ((*) factor)


{-| Map the red channel with the given function. The result is clamped in the
0->1 range.
-}
mapRed : (Float -> Float) -> Color -> Color
mapRed func c =
    let
        { red, green, blue, alpha } =
            toRgb c
    in
    rgb
        { red = clamp 0 1 (func red)
        , green = green
        , blue = blue
        , alpha = alpha
        }


{-| Map the green channel with the given function. The result is clamped in the
0->1 range.
-}
mapGreen : (Float -> Float) -> Color -> Color
mapGreen func c =
    let
        { red, green, blue, alpha } =
            toRgb c
    in
    rgb
        { red = red
        , green = clamp 0 1 (func green)
        , blue = blue
        , alpha = alpha
        }


{-| Map the blue channel with the given function. The result is clamped in the
0->1 range.
-}
mapBlue : (Float -> Float) -> Color -> Color
mapBlue func c =
    let
        { red, green, blue, alpha } =
            toRgb c
    in
    rgb
        { red = red
        , green = green
        , blue = clamp 0 1 (func blue)
        , alpha = alpha
        }


{-| Map the hue with the given function. The result is clamped in the
0->1 range.
-}
mapHue : (Float -> Float) -> Color -> Color
mapHue func c =
    let
        { hue, saturation, lightness, alpha } =
            toHsluv c
    in
    hsluv
        { hue = clamp 0 1 (func hue)
        , saturation = saturation
        , lightness = lightness
        , alpha = alpha
        }


{-| Map the saturation with the given function. The result is clamped in the
0->1 range.
-}
mapSaturation : (Float -> Float) -> Color -> Color
mapSaturation func c =
    let
        { hue, saturation, lightness, alpha } =
            toHsluv c
    in
    hsluv
        { hue = hue
        , saturation = clamp 0 1 (func saturation)
        , lightness = lightness
        , alpha = alpha
        }


{-| Map the lightness with the given function. The result is clamped in the
0->1 range.
-}
mapLightness : (Float -> Float) -> Color -> Color
mapLightness func c =
    let
        { hue, saturation, lightness, alpha } =
            toHsluv c
    in
    hsluv
        { hue = hue
        , saturation = saturation
        , lightness = clamp 0 1 (func lightness)
        , alpha = alpha
        }


{-| Map the alpha with the given function. The result is clamped in the
0->1 range.
-}
mapAlpha : (Float -> Float) -> Color -> Color
mapAlpha func color =
    case color of
        RGB c ->
            RGB { c | alpha = clamp 0 1 (func c.alpha) }

        HSLuv c ->
            HSLuv { c | alpha = clamp 0 1 (func c.alpha) }
