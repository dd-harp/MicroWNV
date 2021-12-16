# interface for aquatic (immature) mosquito populations: any model of immature mosquitoes must implement these functions

# get emergents

#' @title Compute number of newly emerging adults
#' @param model an object from [MicroWNV::make_microWNV]
#' @export
compute_emergents <- function(model) {
  UseMethod("compute_emergents", model$aqua)
}


# add oviposition

#' @title Add eggs from oviposition to aquatic model
#' @param model an object from [MicroWNV::make_microWNV]
#' @param eggs a vector of length `p` giving eggs for each place
#' @export
add_oviposit <- function(model, eggs) {
  UseMethod("add_oviposit", model$aqua)
}


# compute oviposition (eggs/patch/day)

#' @title Compute number of eggs laid from oviposition for each patch
#' @param model an object from [MicroWNV::make_microWNV]
#' @export
compute_oviposit <- function(model) {
  UseMethod("compute_oviposit", model$aqua)
}
