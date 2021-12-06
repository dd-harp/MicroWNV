# MicroWNV

<!-- badges: start -->
[![R-CMD-check](https://github.com/dd-harp/MicroWNV/workflows/R-CMD-check/badge.svg)](https://github.com/dd-harp/MicroWNV/actions)
[![codecov](https://codecov.io/gh/dd-harp/MicroWNV/branch/main/graph/badge.svg)](https://codecov.io/gh/dd-harp/MicroWNV)
<!-- badges: end -->

## Software design

This is intended to quickly get a prototype running. Therefore we will eschew some
elegance for speed. The model object will be an environment with a class attribute.
All interior structure will be named lists. Each function will be passed the entire
model object.

It should eventually be turned into a package.
