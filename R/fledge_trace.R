# classes and methods to deal with fledglings and egg laying for birds

#' @title Setup fledglings with trace (forced) births
#' @description This requires the birds to be set up prior to being called.
#' @param model an object from [MicroMoB::make_MicroMoB]
#' @param trace either a vector of length equal to `p`, a matrix with `p` rows
#' and `tmax` columns, or a matrix with `p` rows and `365` columns
#' @param stochastic should the model update deterministically or stochastically?
#' @export
setup_fledge_trace <- function(model, trace, stochastic) {
  stopifnot(inherits(model, "MicroMoB"))
  tmax <- model$global$tmax
  p <- model$global$p

  if (inherits(trace, "matrix")) {
    stopifnot(nrow(trace) == p)
    if (ncol(trace) == 365L) {
      ix <- (1:tmax) %% 365L
      ix[which(ix == 0L)] <- 365L
      trace_mat <- trace[, ix, drop = FALSE]
    } else if (ncol(trace) == tmax) {
      trace_mat <- trace
    } else {
      stop("incorrect dimensions of trace matrix")
    }
  } else {
    stopifnot(length(trace) == p)
    if (p > 1) {
      trace_mat <- replicate(n = tmax, expr = trace)
    } else {
      trace_mat <- matrix(data = trace, nrow = 1, ncol = tmax)
    }
  }

  fledge_class <- c("trace")
  if (stochastic) {
    fledge_class <- c(fledge_class, "trace_stochastic")
  } else {
    fledge_class <- c(fledge_class, "trace_deterministic")
  }

  model$fledge <- structure(list(), class = fledge_class)
  model$fledge$trace <- trace_mat

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


# add clutch

#' @title Add eggs to trace fledgling model
#' @description This function does nothing as trace models are not affected by
#' endogenous dynamics.
#' @inheritParams add_clutch
#' @export
add_clutch.trace <- function(model, eggs) {invisible()}


# compute clutch (eggs/patch/day)

#' @title Compute null egg clutches from birds
#' @description This is to be used with modeling fledgling birth as a trace.
#' @inheritParams compute_clutch
#' @export
compute_clutch.trace <- function(model, N) {invisible()}

# if we have compute_clutch.logistic, for example, call NextMethod to
# dispatch on deterministic or stochastic
