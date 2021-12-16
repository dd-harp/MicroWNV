# interface for fledglings: any model of fledglings must implement these functions

# step (update)

#' @title Update fledgling populations
#' @param model an object from [MicroWNV::make_microWNV]
#' @export
step_fledge <- function(model) {
  UseMethod("step_fledge", model$fledge)
}

# get fledglings

#' @title Compute number of new fledglings
#' @param model an object from [MicroWNV::make_microWNV]
#' @export
compute_fledge <- function(model) {
  UseMethod("compute_fledge", model$fledge)
}


# add clutch

#' @title Add clutch (eggs) to fledgling model
#' @param model an object from [MicroWNV::make_microWNV]
#' @param eggs a vector of length `p` giving eggs for each place
#' @export
add_clutch <- function(model, eggs) {
  UseMethod("add_clutch", model$fledge)
}


# compute clutch (eggs/patch/day)

#' @title Compute egg clutches from birds
#' @param model an object from [MicroWNV::make_microWNV]
#' @param N total number of birds in each patch
#' @export
compute_clutch <- function(model, N) {
  UseMethod("compute_clutch", model$fledge)
}
