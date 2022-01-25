# classes and methods to deal with fledglings and egg laying for birds

#' @title Setup fledglings with trace (forced) births
#' @description This requires the birds to be set up prior to being called.
#' @param model an object from [MicroMoB::make_MicroMoB]
#' @param trace either a vector of length equal to `p`, a matrix with `p` rows
#' and `tmax` columns, or a matrix with `p` rows and `365` columns
#' @param stochastic should the model update deterministically or stochastically?
#' @importFrom MicroMoB time_patch_varying_parameter
#' @export
setup_fledge_trace <- function(model, trace, stochastic) {
  stopifnot(inherits(model, "MicroMoB"))
  tmax <- model$global$tmax
  p <- model$global$p

  fledge_class <- c("trace")
  if (stochastic) {
    fledge_class <- c(fledge_class, "trace_stochastic")
  } else {
    fledge_class <- c(fledge_class, "trace_deterministic")
  }

  model$fledge <- structure(list(), class = fledge_class)
  model$fledge$trace <- time_patch_varying_parameter(param = trace, p = p, tmax = tmax)

}


# step

#' @title Update fledgling populations for forced births
#' @description This function does nothing as trace models are do not have
#' endogenous dynamics.
#' @inheritParams step_fledge
#' @export
step_fledge.trace <- function(model) {invisible()}


# get fledglings

#' @title Compute number of new fledglings from trace
#' @inheritParams compute_fledge
#' @details see [MicroWNV::compute_fledge.trace_deterministic] and [MicroWNV::compute_fledge.trace_stochastic]
#' @export
compute_fledge.trace <- function(model) {
  NextMethod()
}

#' @title Compute number of new fledglings from trace (deterministic)
#' @description Return the column of the trace matrix for this day.
#' @inheritParams compute_fledge
#' @export
compute_fledge.trace_deterministic <- function(model) {
  return(model$fledge$trace[, model$global$tnow])
}

#' @title Compute number of new fledglings from trace (stochastic)
#' @description Draw a Poisson distributed number of fledglings with mean parameter
#' from the column of the trace matrix for this day.
#' @inheritParams compute_fledge
#' @importFrom stats rpois
#' @export
compute_fledge.trace_stochastic <- function(model) {
  return(rpois(n = model$global$p, lambda = model$fledge$trace[, model$global$tnow]))
}
