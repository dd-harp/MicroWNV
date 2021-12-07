#' @title Setup humans with SIR infection model
#' @param stochastic should the model update deterministically or stochastically?
#' @param model an object from [MicroWNV::make_microWNV]
#' @param theta a time spent matrix
#' @param wf biting weights
#' @param H vector of strata population sizes
#' @param SIR a matrix giving S, I, R counts for each strata
#' @param b transmission efficiency (mosquito to human)
#' @param c transmission efficiency (human to mosquito)
#' @param gamma rate of recovery
#' @export
setup_humans_SIR <- function(model, stochastic, theta, wf = NULL, H, SIR, b = 0.55, c = 0.15, gamma = 1/5) {
  stopifnot(inherits(model, "microWNV"))
  stopifnot(inherits(theta, "matrix"))

  stopifnot(nrow(theta) >= ncol(theta))
  stopifnot(approx_equal(rowSums(theta), 1))

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

  human_class <- c("SIR")
  if (stochastic) {
    human_class <- c(human_class, "SIR_stochastic")
  } else {
    human_class <- c(human_class, "SIR_deterministic")
  }

  model$human <- structure(list(), class = human_class)
  model$human$theta <- theta
  model$human$wf <- wf
  model$human$H <- H
  model$human$SIR <- SIR

  model$human$h <- rep(0, n)

  model$human$b <- b
  model$human$c <- c
  model$human$gamma <- gamma
}


#' @title Compute available humans for SIR model
#' @inheritParams compute_W
#' @export
compute_W.SIR <- function(model) {
  Psi <- model$human$theta
  W <- t(Psi) %*% (model$human$wf * model$human$H)
  return(W)
}

#' @title Compute net infectiousness for SIR model
#' @inheritParams compute_x
#' @export
compute_x.SIR <- function(model) {
  X <- model$human$SIR[, "I"] / model$human$H
  return(X * model$human$c)
}


#' @title Update SIR human model
#' @inheritParams step_humans
#' @export
step_humans.SIR <- function(model) {
  NextMethod()
}

#' @title Update SIR human model (deterministic)
#' @inheritParams step_humans
#' @importFrom stats pexp
#' @export
step_humans.SIR_deterministic <- function(model) {

  # compute differences: S
  S_leave <- model$human$SIR[, "S"] * pexp(q =  model$human$h)

  # compute differences: I
  I_leave <- model$human$SIR[, "I"] * pexp(q =  model$human$gamma)

  # update
  model$human$SIR[, "S"] <- model$human$SIR[, "S"] - S_leave
  model$human$SIR[, "I"] <- model$human$SIR[, "I"] + S_leave - I_leave
  model$human$SIR[, "R"] <- model$human$SIR[, "R"] + I_leave

  model$human$SIR <- pmax(model$human$SIR, 0)

}

#' @title Update SIR human model (stochastic)
#' @inheritParams step_humans
#' @importFrom stats pexp rbinom
#' @export
step_humans.SIR_stochastic <- function(model) {

  n <- model$global$n

  # compute differences: S
  S_leave <- rbinom(n = n, size = model$human$SIR[, "S"], prob = pexp(q =  model$human$h))

  # compute differences: I
  I_leave <- rbinom(n = n, size = model$human$SIR[, "I"], prob = pexp(q =  model$human$gamma))

  # update
  model$human$SIR[, "S"] <- model$human$SIR[, "S"] - S_leave
  model$human$SIR[, "I"] <- model$human$SIR[, "I"] + S_leave - I_leave
  model$human$SIR[, "R"] <- model$human$SIR[, "R"] + I_leave

}
