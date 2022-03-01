# MicroWNV

<!-- badges: start -->
[![R-CMD-check](https://github.com/dd-harp/MicroWNV/workflows/R-CMD-check/badge.svg)](https://github.com/dd-harp/MicroWNV/actions)
[![codecov](https://codecov.io/gh/dd-harp/MicroWNV/branch/main/graph/badge.svg?token=3IPFQEBJ5P)](https://codecov.io/gh/dd-harp/MicroWNV)
<!-- badges: end -->

## Introduction

**Micro-WNV** is an extension of [**Micro-MoB**](https://github.com/dd-harp/MicroMoB) for West Nile virus (WNV)
models. It depends on the `MicroMoB` package which should be installed first.
It follows the same design philosophy as that software package, and is a true extension
in the sense that it cannot be used independently, as it depends on "core" algorithms
common across most mosquito-borne pathogen transmission simulations implemented in `MicroMoB`.

The `MicroWNV` package adds two additional components, _fledge_ for fledgling (immature)
bird populations, and _bird_ for adult bird populations. In certain cases some S3 methods
have been overridden to take into account the peculiarities of WNV models, and can be
found in the source code (and references) clearly marked.

## Installation

```
remotes::install_github('dd-harp/MicroWNV')
library(MicroWNV)
```

## Contributing

Thank you for your interest in **Micro-MoB**! If you have a bug to report, please
open an [issue on GitHub](https://github.com/dd-harp/MicroMoB/issues). If you would like
to open a pull request or have further questions, please see our guide to
contributing to the project at `vignette("Contributing")`.
