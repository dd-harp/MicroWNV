# interface for birds: any model of birds must implement these functions

#' @title Update bird population
#' @param model an object from [MicroWNV::make_microWNV]
#' @export
step_birds <- function(model) {
  UseMethod("step_birds", model$bird)
}

#' @title Compute net infectiousness of birds
#' @description This is normally computed as the prevalence of disease in each place
#' multiplied by the transmission efficiency from birds to mosquitoes.
#' @param model an object from [MicroWNV::make_microWNV]
#' @return a vector of length `p`
#' @export
compute_xB <- function(model) {
  UseMethod("compute_xB", model$bird)
}

#' @title Compute total bird population
#' @description Compute the total bird population in each place
#' @param model an object from [MicroWNV::make_microWNV]
#' @return a vector of length `p`
#' @export
compute_B_pop <- function(model) {
  UseMethod("compute_B_pop", model$bird)
}
