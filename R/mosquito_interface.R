# interface for mosquitoes: any model of mosquitoes must implement these functions

#' @title Update mosquito population
#' @description This method dispatches on the type of `model$mosquito`
#' @param model an object from [MicroWNV::make_microWNV]
#' @export
step_mosquitoes <- function(model) {
  UseMethod("step_mosquitoes", model$mosquito)
}
