# interface for birds: any model of birds must implement these functions

# update (step)

#' @title Update bird population
#' @description This method dispatches on the type of `model$bird`.
#' @param model an object from [MicroMoB::make_MicroMoB]
#' @export
step_birds <- function(model) {
  UseMethod("step_birds", model$bird)
}

# calculate values for biting

#' @title Compute available birds (\eqn{W_{B}})
#' @description Compute the bird population in each place weighted by home range
#' and biting weight. This method dispatches on the type of `model$bird`.
#' @param model an object from [MicroMoB::make_MicroMoB]
#' @return a vector of length `p`
#' @export
compute_WB <- function(model) {
  UseMethod("compute_WB", model$bird)
}

#' @title Compute bird biting weights (\eqn{w_{f_{B}}})
#' @description This method dispatches on the type of `model$bird`.
#' @param model an object from [MicroMoB::make_MicroMoB]
#' @return a vector of length `p`
#' @export
compute_wfB <- function(model) {
  UseMethod("compute_wfB", model$bird)
}

#' @title Compute net infectiousness of birds (\eqn{x_{B}})
#' @description This is normally computed as the prevalence of disease in each place
#' multiplied by the transmission efficiency from birds to mosquitoes.
#' This method dispatches on the type of `model$bird`
#' @param model an object from [MicroMoB::make_MicroMoB]
#' @return a vector of length `p`
#' @export
compute_xB <- function(model) {
  UseMethod("compute_xB", model$bird)
}

#' @title Compute total bird population (\eqn{B_{pop}})
#' @description Compute the total bird population in each place
#' This method dispatches on the type of `model$bird`
#' @param model an object from [MicroMoB::make_MicroMoB]
#' @return a vector of length `p`
#' @export
compute_B_pop <- function(model) {
  UseMethod("compute_B_pop", model$bird)
}

#' @title Compute time at risk matrix (\eqn{\Psi})
#' @description Compute the home range (time at risk) matrix for bird populations.
#' This method dispatches on the type of `model$bird`.
#' @param model an object from [MicroMoB::make_MicroMoB]
#' @return a matrix with `n` rows and `p` columns
#' @export
compute_PsiB <- function(model) {
  UseMethod("compute_PsiB", model$bird)
}
