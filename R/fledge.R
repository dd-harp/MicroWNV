#' @title Setup fledglings with trace (forced) births
#' @description This requires the birds to be set up prior to being called.
#' @param model an object from [MicroWNV::make_microWNV]
#' @param trace either a vector of length equal to `p` or a matrix with `p` rows
#' and `tmax` columns, vectors will be converted to the appropriate matrix internally.
#' @param stochastic should the model update deterministically or stochastically?
#' @export
setup_fledge_trace <- function(model, trace, stochastic) {
  stopifnot(inherits(model, "microWNV"))
  if (inherits(trace, "matrix")) {
    stopifnot(nrow(trace) == model$global$p)
    stopifnot(ncol(trace) == model$global$tmax)
  } else {
    stopifnot(length(trace) == model$global$p)
    trace <- replicate(n = model$global$tmax, expr = trace)
  }

  fledge_class <- c("trace")
  if (stochastic) {
    fledge_class <- c(fledge_class, "trace_stochastic")
  } else {
    fledge_class <- c(fledge_class, "trace_deterministic")
  }

  model$fledge <- structure(list(), class = fledge_class)
  model$fledge$trace <- trace

}



# get fledglings

#' @title Compute number of new fledglings
#' @param model an object from [MicroWNV::make_microWNV]
#' @export
compute_fledge <- function(model) {
  UseMethod("compute_fledge", model$fledge)
}

#' @title Compute number of new fledglings from trace
#' @inheritParams compute_fledge
#' @details see [MicroWNV::compute_fledge.trace_deterministic] and [MicroWNV::compute_fledge.trace_stochastic]
#' @export
compute_fledge.trace <- function(model) {
  NextMethod()
}

#' @title Update SIRS bird population (deterministic)
#' @description Return the column of the trace matrix for this day.
#' @inheritParams compute_fledge
#' @export
compute_fledge.trace_deterministic <- function(model) {
  return(model$fledge$trace[, model$global$tnow])
}

#' @title Update SIRS bird population (stochastic)
#' @description Draw a Poisson distributed number of fledglings with mean parameter
#' from the column of the trace matrix for this day.
#' @inheritParams compute_fledge
#' @importFrom stats rpois
#' @export
compute_fledge.trace_stochastic <- function(model) {
  return(rpois(n = model$global$p, lambda = model$fledge$trace[, model$global$tnow]))
}


# add eggs

#' @title Add eggs to fledgling model
#' @param model an object from [MicroWNV::make_microWNV]
#' @param eggs a vector of length `p` giving eggs for each place
#' @export
add_eggs <- function(model, eggs) {
  UseMethod("add_eggs", model$fledge)
}

#' @title Add eggs to trace fledgling model
#' @description This function does nothing as trace models are not affected by
#' endogenous dynamics.
#' @inheritParams add_eggs
#' @export
add_eggs.trace <- function(model, eggs) {invisible()}
