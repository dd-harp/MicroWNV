#' @title Setup humans with SIR infection model
#' @param model an object from [MicroWNV::make_microWNV]
#' @param theta a time spent matrix
#' @param wf biting weights
#' @param H vector of strata population sizes
#' @param SIR a matrix giving S, I, R counts for each strata
#' @param b transmission efficiency (mosquito to human)
#' @param c transmission efficiency (human to mosquito)
#' @param gamma rate of recovery
#' @export
setup_humans.SIR <- function(model, theta, wf = NULL, H, SIR, b = 0.55, c = 0.15, gamma = 1/5) {
  stopifnot(inherits(model, "microWNV"))
  stopifnot(inherits(theta, "matrix"))

  stopifnot(nrow(theta) >= ncol(theta))
  stopifnot(rowSums(theta) <= 1)

  n <- nrow(theta)
  p <- ncol(theta)

  stopifnot(length(wf) == n)
  stopifnot(is.finite(wf))
  stopifnot(wf >= 0)

  stopifnot(nrow(SIR) == n)
  stopifnot(ncol(SIR) == 3L)
  stopifnot(rowSums(SIR) == H)

  stopifnot(is.finite(c(b, c, gamma)))
  stopifnot(c(b, c, gamma) > 0)
  stopifnot(c(b, c) <= 1)

  model$global$n <- n
  model$global$p <- p

  if (is.null(wf)) {
    wf <- rep(1, n)
  }

  if (is.null(colnames(SIR))) {
    colnames(SIR) <- c("S", "I", "R")
  } else {
    stopifnot(colnames(SIR) == c("S", "I", "R"))
  }

  model$human <- structure(list(), class = "SIR")
  model$human$theta <- theta
  model$human$wf <- wf
  model$human$H <- H
  model$human$SIR <- SIR

  model$human$b <- b
  model$human$c <- c
  model$human$gamma <- gamma
}

#' @title Compute available humans
#' @param model an object from [MicroWNV::make_microWNV]
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
#' @param model an object from [MicroWNV::make_microWNV]
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
