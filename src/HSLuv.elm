module HSLuv
    exposing
        ( hsluvToRgb
        , hpluvToRgb
        , rgbToHsluv
        , rgbToHpluv
        )

{-| Convert color between HSLuv and RGB spaces


# Low level functions

@docs hsluvToRgb, hpluvToRgb, rgbToHsluv, rgbToHpluv

-}

import Color exposing (Color)


{-| `hsluvToRgb` convert HSLuv components to RGB
-}
hsluvToRgb : ( Float, Float, Float ) -> ( Float, Float, Float )
hsluvToRgb ( h, s, l ) =
    ( 0, 0, 0 )


{-| `hpluvToRgb` convert HSLuv components to RGB
-}
hpluvToRgb : ( Float, Float, Float ) -> ( Float, Float, Float )
hpluvToRgb ( h, s, l ) =
    ( 0, 0, 0 )


{-| `rgbToHsluv` convert RGB components to HSLuv
-}
rgbToHsluv : ( Float, Float, Float ) -> ( Float, Float, Float )
rgbToHsluv ( r, g, b ) =
    ( 0, 0, 0 )


{-| `rgbToHpluv` convert RGB components to HSLuv
-}
rgbToHpluv : ( Float, Float, Float ) -> ( Float, Float, Float )
rgbToHpluv ( r, g, b ) =
    ( 0, 0, 0 )
