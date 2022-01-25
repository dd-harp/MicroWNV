# interface for fledglings: any model of fledglings must implement these functions

# step (update)

#' @title Update fledgling populations
#' @description This method dispatches on the type of `model$fledge`
#' @param model an object from [MicroMoB::make_MicroMoB]
#' @export
step_fledge <- function(model) {
  UseMethod("step_fledge", model$fledge)
}

# get fledglings

#' @title Compute number of new fledglings
#' @description This method dispatches on the type of `model$fledge`
#' @param model an object from [MicroMoB::make_MicroMoB]
#' @export
compute_fledge <- function(model) {
  UseMethod("compute_fledge", model$fledge)
}
