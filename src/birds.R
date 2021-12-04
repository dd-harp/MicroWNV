#' @title Setup birds with SIRS infection model
#' @description This requires the humans to be set up prior to being called.
#' @param model an object from [MicroWNV::make_microWNV]
#' @param fledge_disperse a dispersal matrix for fledglings
#' @param theta a matrix giving time spent in each bird's home range
#' @param wf biting weights
#' @export
setup_birds.SIRS <- function(model, fledge_disperse, theta, SIR, wf = NULL, b = 0.55, c = 0.15, gamma = 1/5, r = 1/120) {
  stopifnot(inherits(model, "microWNV"))
  stopifnot(inherits(fledge_disperse, "matrix"))
  stopifnot(inherits(theta, "matrix"))

  p <- model$global$p

  stopifnot(nrow(theta) == ncol(theta))
  stopifnot(nrow(theta) == p)
  stopifnot(rowSums(theta) == 1)

  stopifnot(nrow(fledge_disperse) == ncol(fledge_disperse))
  stopifnot(nrow(fledge_disperse) == p)
  stopifnot(rowSums(fledge_disperse) == 1)

  if (is.null(colnames(SIR))) {
    colnames(SIR) <- c("S", "I", "R")
  } else {
    stopifnot(colnames(SIR) == c("S", "I", "R"))
  }
  stopifnot(is.finite(SIR))
  stopifnot(rowSums(SIR) >= 0)

  if (is.null(wf)) {
    wf <- rep(1, n)
  }

  model$bird <- structure(list(), class = "SIRS")
  model$bird$fledge_disperse <- fledge_disperse
  model$bird$theta <- theta
  model$bird$wf <- wf
  model$bird$SIR <- SIR

  model$bird$b <- b
  model$bird$c <- c
  model$bird$gamma <- gamma
}


step_birds.deterministic <- function(model) {
  # get new fledglings and their dispersion
  fledglings <- compute_fledge(model)
  fledglings <- fledglings %*% model$bird$fledge_disperse
}





#' @title Compute available humans
#' @param model
#' @return a vector of length `p`
#' @export
compute_W <- function(model) {
  UseMethod("compute_W", model$human)
}

#' @inheritParams compute_W
#' @export
compute_W.SIR <- function(model) {
  Psi <- model$human$theta
  W <- t(Psi) %*% (model$human$wf * model$human$H)
  return(W)
}

#' @title Compute net infectiousness of humans
#' @param model
#' @return a vector of length `n`
compute_x <- function(model) {
  UseMethod("compute_x", model$human)
}

#' @inheritParams compute_x
#' @export
compute_x.SIR <- function(model) {
  X <- model$human$SIR[, "I"] / model$human$H
  return(X * model$human$c)
}
