# interface for birds: any model of birds must implement these functions

#' @title Update bird population
#' @description This method dispatches on the type of `model$bird`.
#' @param model an object from [MicroWNV::make_microWNV]
#' @export
step_birds <- function(model) {
  UseMethod("step_birds", model$bird)
}

#' @title Compute net infectiousness of birds
#' @description This is normally computed as the prevalence of disease in each place
#' multiplied by the transmission efficiency from birds to mosquitoes.
#' This method dispatches on the type of `model$bird`
#' @param model an object from [MicroWNV::make_microWNV]
#' @return a vector of length `p`
#' @export
compute_xB <- function(model) {
  UseMethod("compute_xB", model$bird)
}

#' @title Compute total bird population
#' @description Compute the total bird population in each place
#' This method dispatches on the type of `model$bird`
#' @param model an object from [MicroWNV::make_microWNV]
#' @return a vector of length `p`
#' @export
compute_B_pop <- function(model) {
  UseMethod("compute_B_pop", model$bird)
}


#' @title Compute available birds
#' @description Compute the bird population in each place weighted by home range
#' and biting weight. This method dispatches on the type of `model$bird`.
#' @param model an object from [MicroWNV::make_microWNV]
#' @return a vector of length `p`
#' @export
compute_WB <- function(model) {
  UseMethod("compute_WB", model$bird)
}
