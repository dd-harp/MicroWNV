# interface for birds: any model of birds must implement these functions

#' @title Update bird population
#' @param model an object from [MicroWNV::make_microWNV]
#' @export
step_birds <- function(model) {
  UseMethod("step_birds", model$bird)
}




