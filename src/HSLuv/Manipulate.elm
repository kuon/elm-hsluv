module HSLuv.Manipulate exposing
    ( setRed, setGreen, setBlue, setHue, setLightness, setSaturation
    , setAlpha
    , multRed, multGreen, multBlue, multHue, multLightness
    , multSaturation, multAlpha
    , mapRed, mapGreen, mapBlue, mapHue, mapLightness
    , mapSaturation, mapAlpha
    )

{-| Manipulate colors

This module let you manipulate the `HSLuv` type.


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
import HSLuv.Color exposing (HSLuv(..))


{-| Set the red channel of the color. Requires a value in the 0->1 range.
-}
setRed : Float -> HSLuv -> HSLuv
setRed newRed =
    mapRed (always newRed)


{-| Set the green channel of the color. Requires a value in the 0->1 range.
-}
setGreen : Float -> HSLuv -> HSLuv
setGreen newGreen =
    mapGreen (always newGreen)


{-| Set the blue channel of the color. Requires a value in the 0->1 range.
-}
setBlue : Float -> HSLuv -> HSLuv
setBlue newBlue =
    mapBlue (always newBlue)


{-| Set the hue of the color. Requires a value in the 0->1 range.
-}
setHue : Float -> HSLuv -> HSLuv
setHue newHue =
    mapHue (always newHue)


{-| Set the saturation of the color. Requires a value in the 0->1 range.
-}
setSaturation : Float -> HSLuv -> HSLuv
setSaturation newSaturation =
    mapSaturation (always newSaturation)


{-| Set the lightness of the color. Requires a value in the 0->1 range.
-}
setLightness : Float -> HSLuv -> HSLuv
setLightness newLightness =
    mapLightness (always newLightness)


{-| Set the alpha of the color. Requires a value in the 0->1 range.
-}
setAlpha : Float -> HSLuv -> HSLuv
setAlpha newAlpha =
    mapAlpha (always newAlpha)


{-| Multiply the red channel by the give factor. The result is clamped in the
0->1 range.
-}
multRed : Float -> HSLuv -> HSLuv
multRed factor =
    mapRed ((*) factor)


{-| Multiply the green channel by the give factor. The result is clamped in the
0->1 range.
-}
multGreen : Float -> HSLuv -> HSLuv
multGreen factor =
    mapGreen ((*) factor)


{-| Multiply the blue channel by the give factor. The result is clamped in the
0->1 range.
-}
multBlue : Float -> HSLuv -> HSLuv
multBlue factor =
    mapBlue ((*) factor)


{-| Multiply the hue by the give factor. The result is clamped in the
0->1 range.
-}
multHue : Float -> HSLuv -> HSLuv
multHue factor =
    mapHue ((*) factor)


{-| Multiply the saturation by the give factor. The result is clamped in the
0->1 range.
-}
multSaturation : Float -> HSLuv -> HSLuv
multSaturation factor =
    mapSaturation ((*) factor)


{-| Multiply the lightness by the give factor. The result is clamped in the
0->1 range.
-}
multLightness : Float -> HSLuv -> HSLuv
multLightness factor =
    mapLightness ((*) factor)


{-| Multiply the alpha by the give factor. The result is clamped in the
0->1 range.
-}
multAlpha : Float -> HSLuv -> HSLuv
multAlpha factor =
    mapAlpha ((*) factor)


{-| Map the red channel with the given function. The result is clamped in the
0->1 range.
-}
mapRed : (Float -> Float) -> HSLuv -> HSLuv
mapRed func c =
    let
        { red, green, blue, alpha } =
            toRgba c
    in
    rgba
        { red = clamp 0 1 (func red)
        , green = green
        , blue = blue
        , alpha = alpha
        }


{-| Map the green channel with the given function. The result is clamped in the
0->1 range.
-}
mapGreen : (Float -> Float) -> HSLuv -> HSLuv
mapGreen func c =
    let
        { red, green, blue, alpha } =
            toRgba c
    in
    rgba
        { red = red
        , green = clamp 0 1 (func green)
        , blue = blue
        , alpha = alpha
        }


{-| Map the blue channel with the given function. The result is clamped in the
0->1 range.
-}
mapBlue : (Float -> Float) -> HSLuv -> HSLuv
mapBlue func c =
    let
        { red, green, blue, alpha } =
            toRgba c
    in
    rgba
        { red = red
        , green = green
        , blue = clamp 0 1 (func blue)
        , alpha = alpha
        }


{-| Map the hue with the given function. The result is clamped in the
0->1 range.
-}
mapHue : (Float -> Float) -> HSLuv -> HSLuv
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
mapSaturation : (Float -> Float) -> HSLuv -> HSLuv
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
mapLightness : (Float -> Float) -> HSLuv -> HSLuv
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
mapAlpha : (Float -> Float) -> HSLuv -> HSLuv
mapAlpha func (HSLuv c) =
    HSLuv { c | alpha = clamp 0 1 (func c.alpha) }
