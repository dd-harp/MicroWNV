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

## Components

The model is broken into components, for humans, mosquitoes, and birds (and some others).
Each component has an _interface_, which are methods which must be defined for that
component. A component's interface is stored in file, for example, R/humans_interface.R
shows the user what methods must be defined for any human model. Other components (e.g. the bloodmeal)
will call generic methods not knowing what specific code is implementing them, and so
they must return values consistent with their definition.

We call a specific implementation of a component a _model_.
Specific implementations are found in files that replace _\_interface_ with the
model name, for example R/humans_SIR.R. Their accompanying test files are located in
tests/testthat. If you are creating a new model, please remember to test it
adequately.

We list the components which require interfaces below and specific models
to implement them.

### Mosquitoes

The mosquito component is responsible for all dynamics which update adult mosquito
populations. The interface is defined in R/mosquito_interface.R.

#### Ross-Macdonald (RM)

Simple model

### Aquatic

The aquatic component is responsible for all dynamics which update immature (aquatic
stage) mosquito populations. The interface is defined in R/aquatic_interface.R.

### Birds

The bird component is responsible for all dynamics which update adult bird populations.
The interface is defined in R/birds_interface.R.

### Fledge

The fledge component is responsible for all dynamics which update immature (fledgling)
bird populations. The interface is defined in R/fledge_interface.R.

### Humans

The human component updates human populations. The interface is defined in R/humans_interface.R.
