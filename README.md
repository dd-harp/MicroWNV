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

### Components

The model is broken into components, for humans, mosquitoes, and birds (and some others).
Each component has an _interface_, which are methods which must be defined for that
component. A component's interface is stored in file, for example, R/humans_interface.R
shows the user what methods must be defined for any human model. Other components (e.g. the bloodmeal)
will call generic methods not knowing what specific code is implementing them, and so
they must return values consistent with their definition.

We call a specific implementation of a component a _model_.
Specific implementations are found in files that replace _\_interface_ with the
model name, for example R/humans_SIR.R.
