# interface for humans: any model of humans must implement these functions

#' @title Compute available humans
#' @description This is normally computed as \deqn{W = \Psi^{\intercal} \cdot w_{f} H}
#' @param model an object from [MicroWNV::make_microWNV]
#' @return a vector of length `p`
#' @export
compute_W <- function(model) {
  UseMethod("compute_W", model$human)
}


#' @title Compute net infectiousness of humans
#' @description In a Ross-Macdonald style transmission model, this is computed as
#' \deqn{x = c X}
#' @param model an object from [MicroWNV::make_microWNV]
#' @return a vector of length `n`
#' @export
compute_x <- function(model) {
  UseMethod("compute_x", model$human)
}
