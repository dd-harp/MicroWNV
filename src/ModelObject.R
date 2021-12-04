
#' @title Make a model object
#' @description The model object is a hashed [environment]. By default it contains
#' a single list
#' @export
make_microWNV <- function() {
  object <- structure(new.env(hash = TRUE), class = "microWNV")
  object$global <- list()
  return(object)
}


