# interface for mosquitoes: any model of mosquitoes must implement these functions

#' @title Update mosquito population
#' @param model an object from [MicroWNV::make_microWNV]
#' @export
step_mosquitoes <- function(model) {
  UseMethod("step_mosquitoes", model$mosquito)
}
