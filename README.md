# HSLuv implementation in pure Elm

[![Build Status](https://travis-ci.org/kuon/elm-hsluv.svg?branch=master)](https://travis-ci.org/kuon/elm-hsluv)

Convert between HSLuv and RGB.


HSLuv references: <http://www.hsluv.org/>

The `Color` type is provided by the [new color package](
https://package.elm-lang.org/packages/avh4/elm-color/latest/Color) from `avh4`.

**Note:** If you plan to manipulate HSLuv colors, store them with the provided
`HSLuv` type and convert them to `Color` at the last possible time as the
conversion can be expensive. If you just use `HSLuv` as constructor, you may
write an helper like:
`color h s l = HSLuv.hsluv360 { hue = h, saturation = s, lightness = l, alpha = 1} |> HSLuv.toColor`


## Basic Usage

```elm
-- create an HSLuv color with a Color
c = HSLuv.color myColor

-- create an HSLuv color with components
c = HSLuv.hsluv360 { hue = 150, saturation = 100, lightness = 50, alpha = 1 }

-- turn a color into normalized components
{ red, green, saturation, lightness } = HSLuv.rgba c

-- turn and HSLuv color into a Color
color = HSLuv.toColor c
```

## Compatibility

- Latest version is targeted at elm `0.19`.
- Version **1.0.1** is the last version compatible with elm `0.18`.
