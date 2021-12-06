
#' @title Make a model object
#' @description The model object is a hashed [environment]. By default it contains
#' a single list
#' @param tmax number of days to simulate
#' @export
make_microWNV <- function(tmax) {
  stopifnot(is.finite(tmax))
  stopifnot(tmax > 0)
  object <- structure(new.env(hash = TRUE), class = "microWNV")
  object$global <- list(tmax = as.integer(tmax), tnow = 1L)
  return(object)
}


