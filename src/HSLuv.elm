module HSLuv exposing
    ( hsluv, hsluv360, rgb, rgb255
    , toHsluv, toHsluv360, toRgb, toRgb255
    , hsluvToRgb, hpluvToRgb, rgbToHsluv, rgbToHpluv
    , lchToLuv, luvToLch, xyzToRgb, rgbToXyz, xyzToLuv, luvToXyz
    , hsluvToLch, lchToHsluv, hpluvToLch, lchToHpluv
    )

{-| Convert color between HSLuv and RGB spaces


# Create colors (constructors)

@docs hsluv, hsluv360, rgb, rgb255


# Convert colors (extractor)

@docs toHsluv, toHsluv360, toRgb, toRgb255


# Low level functions

@docs hsluvToRgb, hpluvToRgb, rgbToHsluv, rgbToHpluv


# Low level intermediate conversion functions

@docs lchToLuv, luvToLch, xyzToRgb, rgbToXyz, xyzToLuv, luvToXyz
@docs hsluvToLch, lchToHsluv, hpluvToLch, lchToHpluv

-}

import HSLuv.Color exposing (Color(..))


{-| `hsluv` create a Color with normalized HSLuv
components (0->1) and an alpha channel
-}
hsluv :
    { hue : Float
    , saturation : Float
    , lightness : Float
    , alpha : Float
    }
    -> Color
hsluv c =
    HSLuv
        { hue = c.hue * 360
        , saturation = c.saturation * 100
        , lightness = c.lightness * 100
        , alpha = c.alpha
        }


{-| `hsluv360` create a Color with HSLuv
components and an alpha channel

    - hue range is 0->360
    - saturation range is 0->100
    - lightness range is 0->100
    - alpha range is 0->1

-}
hsluv360 :
    { hue : Float
    , saturation : Float
    , lightness : Float
    , alpha : Float
    }
    -> Color
hsluv360 =
    HSLuv


{-| `rgb` create a Color with normalized RGB components (0->1)
and an alpha channel
-}
rgb : { red : Float, green : Float, blue : Float, alpha : Float } -> Color
rgb =
    RGB


{-| `rgb255` create a Color with RGB components (0->255) and an alpha channel (0->1)
-}
rgb255 : { red : Int, green : Int, blue : Int, alpha : Float } -> Color
rgb255 c =
    rgb
        { red = toFloat c.red / 255
        , green = toFloat c.green / 255
        , blue = toFloat c.blue / 255
        , alpha = c.alpha
        }


{-| `toHsluv` extract the normalized components of a color in the HSLuv format
-}
toHsluv :
    Color
    ->
        { hue : Float
        , saturation : Float
        , lightness : Float
        , alpha : Float
        }
toHsluv color =
    case color of
        HSLuv c ->
            { hue = c.hue / 360
            , saturation = c.saturation / 100
            , lightness = c.lightness / 100
            , alpha = c.alpha
            }

        RGB c ->
            let
                ( h, s, l ) =
                    rgbToHsluv ( c.red, c.green, c.blue )
            in
            { hue = h / 360
            , saturation = s / 100
            , lightness = l / 100
            , alpha = c.alpha
            }


{-| `toHsluv360` extract the components of a color in the HSLuv format

    - hue range is 0->360
    - saturation range is 0->100
    - lightness range is 0->100
    - alpha range is 0->1

-}
toHsluv360 :
    Color
    ->
        { hue : Float
        , saturation : Float
        , lightness : Float
        , alpha : Float
        }
toHsluv360 color =
    case color of
        HSLuv c ->
            c

        RGB c ->
            let
                ( h, s, l ) =
                    rgbToHsluv ( c.red, c.green, c.blue )
            in
            { hue = h, saturation = s, lightness = l, alpha = c.alpha }


{-| `toRgb` extract the normalized components of a color in the RGBA format
-}
toRgb :
    Color
    ->
        { red : Float
        , green : Float
        , blue : Float
        , alpha : Float
        }
toRgb color =
    case color of
        HSLuv c ->
            let
                ( r, g, b ) =
                    hsluvToRgb ( c.hue, c.saturation, c.lightness )
            in
            { red = r
            , green = g
            , blue = b
            , alpha = c.alpha
            }

        RGB c ->
            c


{-| `toRgb255` extract the components of a color in the RGBA format (0->255)
-}
toRgb255 :
    Color
    ->
        { red : Int
        , green : Int
        , blue : Int
        , alpha : Float
        }
toRgb255 color =
    case color of
        HSLuv c ->
            let
                ( r, g, b ) =
                    hsluvToRgb ( c.hue, c.saturation, c.lightness )
            in
            { red = r * 255 |> round
            , green = g * 255 |> round
            , blue = b * 255 |> round
            , alpha = c.alpha
            }

        RGB c ->
            { red = c.red * 255 |> round
            , green = c.green * 255 |> round
            , blue = c.blue * 255 |> round
            , alpha = c.alpha
            }


type alias Vec2 =
    ( Float, Float )


type alias Vec3 =
    ( Float, Float, Float )


type alias Bounds =
    List Vec2


{-| `hsluvToRgb` convert HSLuv components to RGB
-}
hsluvToRgb : Vec3 -> Vec3
hsluvToRgb =
    hsluvToLch
        >> lchToLuv
        >> luvToXyz
        >> xyzToRgb


{-| `hpluvToRgb` convert HSLuv components to RGB
-}
hpluvToRgb : Vec3 -> Vec3
hpluvToRgb =
    hpluvToLch
        >> lchToLuv
        >> luvToXyz
        >> xyzToRgb


{-| `rgbToHsluv` convert RGB components to HSLuv
-}
rgbToHsluv : Vec3 -> Vec3
rgbToHsluv =
    rgbToXyz
        >> xyzToLuv
        >> luvToLch
        >> lchToHsluv


{-| `rgbToHpluv` convert RGB components to HSLuv
-}
rgbToHpluv : Vec3 -> Vec3
rgbToHpluv =
    rgbToXyz
        >> xyzToLuv
        >> luvToLch
        >> lchToHpluv


{-| `lchToLuv` convert LCH components to LUV
-}
lchToLuv : Vec3 -> Vec3
lchToLuv ( l, c, h ) =
    let
        hRad =
            h / 360.0 * 2.0 * pi
    in
    ( l, cos hRad * c, sin hRad * c )


{-| `luvToLch` convert LUV components to LCH
-}
luvToLch : Vec3 -> Vec3
luvToLch ( l, u, v ) =
    let
        c =
            sqrt (u * u + v * v)

        h =
            if c < minF then
                0

            else
                atan2 v u * 180.0 / pi

        h_ =
            if h < 0 then
                360 + h

            else
                h
    in
    ( l, c, h_ )


{-| `xyzToRgb` convert XYZ components to RGB
-}
xyzToRgb : Vec3 -> Vec3
xyzToRgb xyz =
    let
        ( m1, m2, m3 ) =
            m

        ( a, b, c ) =
            ( dotProduct m1 xyz, dotProduct m2 xyz, dotProduct m3 xyz )
    in
    ( fromLinear a, fromLinear b, fromLinear c )


{-| `rgbToXyz` convert RGB components to XYZ
-}
rgbToXyz : Vec3 -> Vec3
rgbToXyz ( r, g, b ) =
    let
        ( m1, m2, m3 ) =
            mInv

        rgb_ =
            ( toLinear r, toLinear g, toLinear b )
    in
    ( dotProduct m1 rgb_, dotProduct m2 rgb_, dotProduct m3 rgb_ )


{-| `xyzToLuv` convert XYZ components to LUV
-}
xyzToLuv : Vec3 -> Vec3
xyzToLuv ( x, y, z ) =
    let
        l =
            f y
    in
    if l == 0 || (x == 0 && y == 0 && z == 0) then
        ( 0, 0, 0 )

    else
        let
            varU =
                (4.0 * x) / (x + (15.0 * y) + (3.0 * z))

            varV =
                (9.0 * y) / (x + (15.0 * y) + (3.0 * z))

            u =
                13.0 * l * (varU - refU)

            v =
                13.0 * l * (varV - refV)
        in
        ( l, u, v )


{-| `luvToXyz` convert LUV components to XYZ
-}
luvToXyz : Vec3 -> Vec3
luvToXyz ( l, u, v ) =
    if l == 0 then
        ( 0, 0, 0 )

    else
        let
            varY =
                fInv l

            varU =
                u / (13.0 * l) + refU

            varV =
                v / (13.0 * l) + refV

            y =
                varY * refY

            x =
                0.0 - (9.0 * y * varU) / ((varU - 4.0) * varV - varU * varV)

            z =
                (9.0 * y - (15.0 * varV * y) - (varV * x)) / (3.0 * varV)
        in
        ( x, y, z )


{-| `hsluvToLch` convert HSLuv components to LCH
-}
hsluvToLch : Vec3 -> Vec3
hsluvToLch ( h, s, l ) =
    if l > maxF then
        ( 100.0, 0, h )

    else if l < minF then
        ( 0.0, 0.0, h )

    else
        ( l, maxSafeChromaForLH l h / 100.0 * s, h )


{-| `lchToHsluv` convert LCH components to HSLuv
-}
lchToHsluv : Vec3 -> Vec3
lchToHsluv ( l, c, h ) =
    if l > maxF then
        ( h, 0, 100.0 )

    else if l < minF then
        ( h, 0.0, 0.0 )

    else
        let
            maxChroma =
                maxSafeChromaForLH l h
        in
        ( h, c / maxChroma * 100.0, l )


{-| `hpluvToLch` convert HPLuv components to LCH
-}
hpluvToLch : Vec3 -> Vec3
hpluvToLch ( h, s, l ) =
    if l > maxF then
        ( 100.0, 0, h )

    else if l < minF then
        ( 0.0, 0.0, h )

    else
        ( l, maxSafeChromaForL l / 100.0 * s, h )


{-| `lchToHpluv` convert LCH components to HPLuv
-}
lchToHpluv : Vec3 -> Vec3
lchToHpluv ( l, c, h ) =
    if l > maxF then
        ( h, 0.0, 100.0 )

    else if l < minF then
        ( h, 0.0, 0.0 )

    else
        ( h, c / maxSafeChromaForL l * 100.0, l )


getBounds : Float -> Bounds
getBounds l =
    let
        sub =
            ((l + 16.0) ^ 3.0) / 1560896.0

        sub_ =
            if sub > epsilon then
                sub

            else
                l / kappa

        compute ( m1, m2, m3 ) t =
            let
                top1 =
                    (284517.0 * m1 - 94839.0 * m3) * sub_

                top2 =
                    (838422.0 * m3 + 769860.0 * m2 + 731718.0 * m1)
                        * l
                        * sub_
                        - (769860.0 * t * l)

                bottom =
                    (632260.0 * m3 - 126452.0 * m2) * sub_ + 126452.0 * t
            in
            ( top1 / bottom, top2 / bottom )

        ( m1_, m2_, m3_ ) =
            m
    in
    [ compute m1_ 0
    , compute m1_ 1
    , compute m2_ 0
    , compute m2_ 1
    , compute m3_ 0
    , compute m3_ 1
    ]


maxSafeChromaForL : Float -> Float
maxSafeChromaForL l =
    let
        fold bound val =
            let
                length =
                    distanceLineFromOrigin bound
            in
            if length >= 0 then
                min val length

            else
                val
    in
    List.foldl fold 1.7976931348623157e308 (getBounds l)


maxSafeChromaForLH : Float -> Float -> Float
maxSafeChromaForLH l h =
    let
        hRad =
            h / 360.0 * pi * 2.0

        fold bound val =
            let
                length =
                    lengthOfRayUntilIntersect hRad bound
            in
            if length >= 0 then
                min val length

            else
                val
    in
    List.foldl fold 1.7976931348623157e308 (getBounds l)


distanceLineFromOrigin : Vec2 -> Float
distanceLineFromOrigin ( slope, intercept ) =
    abs intercept / sqrt (slope ^ 2 + 1)


lengthOfRayUntilIntersect : Float -> Vec2 -> Float
lengthOfRayUntilIntersect theta ( slope, intercept ) =
    intercept / (sin theta - slope * cos theta)


dotProduct : Vec3 -> Vec3 -> Float
dotProduct ( a0, a1, a2 ) ( b0, b1, b2 ) =
    a0 * b0 + a1 * b1 + a2 * b2


minF : Float
minF =
    0.00000001


maxF : Float
maxF =
    99.9999999


m : ( Vec3, Vec3, Vec3 )
m =
    ( ( 3.240969941904521, -1.537383177570093, -0.498610760293 )
    , ( -0.96924363628087, 1.87596750150772, 0.041555057407175 )
    , ( 0.055630079696993, -0.20397695888897, 1.056971514242878 )
    )


mInv : ( Vec3, Vec3, Vec3 )
mInv =
    ( ( 0.41239079926595, 0.35758433938387, 0.18048078840183 )
    , ( 0.21263900587151, 0.71516867876775, 0.072192315360733 )
    , ( 0.019330818715591, 0.11919477979462, 0.95053215224966 )
    )


refY : Float
refY =
    1.0


refU : Float
refU =
    0.19783000664283


refV : Float
refV =
    0.46831999493879


kappa : Float
kappa =
    903.2962962


epsilon : Float
epsilon =
    0.0088564516


f : Float -> Float
f t =
    if t > epsilon then
        116.0 * ((t / refY) ^ (1.0 / 3.0)) - 16.0

    else
        t / refY * kappa


fInv : Float -> Float
fInv t =
    if t > 8 then
        refY * ((t + 16.0) / 116.0) ^ 3.0

    else
        refY * t / kappa


toLinear : Float -> Float
toLinear c =
    if c > 0.04045 then
        ((c + 0.055) / 1.055) ^ 2.4

    else
        c / 12.92


fromLinear : Float -> Float
fromLinear c =
    if c <= 0.0031308 then
        12.92 * c

    else
        1.055 * (c ^ (1.0 / 2.4)) - 0.055
