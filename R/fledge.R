# classes and methods to deal with fledglings and egg laying for birds

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


# add clutch

#' @title Add clutch (eggs) to fledgling model
#' @param model an object from [MicroWNV::make_microWNV]
#' @param eggs a vector of length `p` giving eggs for each place
#' @export
add_clutch <- function(model, eggs) {
  UseMethod("add_clutch", model$fledge)
}

#' @title Add eggs to trace fledgling model
#' @description This function does nothing as trace models are not affected by
#' endogenous dynamics.
#' @inheritParams add_clutch
#' @export
add_clutch.trace <- function(model, eggs) {invisible()}




# clutch laying dynamics

#' @title Setup null model for clutches
#' @description This requires the birds to be set up prior to being called. The
#' null model is to be used with trace-based fledgling births (from [MicroWNV::setup_fledge_trace]).
#' @param model an object from [MicroWNV::make_microWNV]
#' @export
setup_clutch_null <- function(model) {
  stopifnot(inherits(model, "microWNV"))
  stopifnot(!is.null(model$bird))

  model$bird$clutch <- structure(list(), class = "null")
}


# compute clutch (eggs/patch/day)

#' @title Compute egg clutches from birds
#' @param model an object from [MicroWNV::make_microWNV]
#' @param N total number of birds in each patch
#' @export
compute_clutch <- function(model, N) {
  UseMethod("compute_clutch", model$bird$clutch)
}

#' @title Compute null egg clutches from birds
#' @description This is to be used with modeling fledgling birth as a trace.
#' @inheritParams compute_clutch
#' @export
compute_clutch.null <- function(model, N) {invisible()}

# if we have compute_clutch.logistic, for example, call NextMethod to
# dispatch on deterministic or stochastic
