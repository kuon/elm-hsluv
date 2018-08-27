# HSLuv implementation in pure Elm

[![Build Status](https://travis-ci.org/kuon/elm-hsluv.svg?branch=master)](https://travis-ci.org/kuon/elm-hsluv)

Convert between HSLuv and RGB.


HSLuv references: <http://www.hsluv.org/>


## Basic Usage

```elm
-- create a Color in the HSLuv color space
c = HSLuv.hsluv360 { hue = 150, saturation = 100, lightness = 50, alpha = 1 }

-- turn a color into normalized components
{ red, green, saturation, lightness } = HSLuv.rgb c
```

## Compatibility

- Latest version is targeted at elm `0.19`.
- Version **1.0.1** is the last version compatible with elm `0.18`.
