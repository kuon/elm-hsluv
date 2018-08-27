module HSLuv.Color exposing (Color(..))


type Color
    = RGB { red : Float, green : Float, blue : Float, alpha : Float }
    | HSLuv { hue : Float, saturation : Float, lightness : Float, alpha : Float }
