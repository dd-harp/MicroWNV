#' @title Setup birds with SIRS infection model
#' @description This requires the humans to be set up prior to being called.
#' @param model an object from [MicroWNV::make_microWNV]
#' @param stochastic should the model update deterministically or stochastically?
#' @param fledge_disperse a dispersal matrix for fledglings
#' @param theta a matrix giving time spent in each bird's home range
#' @param SIR matrix of initial states for each patch
#' @param mu vector of length `p` or matrix with `p` rows and `tmax` columns giving
#' daily bird death rates
#' @param wf biting weights
#' @param b transmission efficiency (mosquitoes to birds)
#' @param c transmission efficiency (birds to mosquitoes)
#' @param gamma inverse of infectious duration
#' @param r recovery rate
#' @export
setup_birds_SIRS <- function(model, stochastic, fledge_disperse, theta, SIR, mu, wf = NULL, b = 0.55, c = 0.15, gamma = 1/5, r = 1/120) {
  stopifnot(inherits(model, "microWNV"))
  stopifnot(inherits(fledge_disperse, "matrix"))
  stopifnot(inherits(theta, "matrix"))
  stopifnot(is.logical(stochastic))

  p <- model$global$p

  stopifnot(nrow(theta) == ncol(theta))
  stopifnot(nrow(theta) == p)
  stopifnot(all.equal(rowSums(theta), rep(1, p)))

  stopifnot(nrow(fledge_disperse) == ncol(fledge_disperse))
  stopifnot(nrow(fledge_disperse) == p)
  stopifnot(all.equal(rowSums(fledge_disperse), rep(1, p)))

  if (inherits(mu, "matrix")) {
    stopifnot(nrow(mu) == model$global$p)
    stopifnot(ncol(mu) == model$global$tmax)
  } else {
    stopifnot(length(mu) == model$global$p)
    mu <- replicate(n = model$global$tmax, expr = mu)
  }

  if (is.null(colnames(SIR))) {
    colnames(SIR) <- c("S", "I", "R")
  } else {
    stopifnot(colnames(SIR) == c("S", "I", "R"))
  }
  stopifnot(is.finite(SIR))
  stopifnot(rowSums(SIR) >= 0)

  if (is.null(wf)) {
    wf <- rep(1, p)
  }

  bird_class <- c("SIRS")
  if (stochastic) {
    bird_class <- c(bird_class, "SIRS_stochastic")
  } else {
    bird_class <- c(bird_class, "SIRS_deterministic")
  }

  model$bird <- structure(list(), class = bird_class)
  model$bird$fledge_disperse <- fledge_disperse
  model$bird$theta <- theta
  model$bird$wf <- wf
  model$bird$SIR <- SIR

  model$bird$h <- rep(0, p)
  model$bird$mu <- mu

  model$bird$b <- b
  model$bird$c <- c
  model$bird$gamma <- gamma
  model$bird$r <- r
}


# update birds over one time step

#' @title Update bird population
#' @param model an object from [MicroWNV::make_microWNV]
#' @export
step_birds <- function(model) {
  UseMethod("step_birds", model$bird)
}

#' @title Update SIRS bird population
#' @inheritParams step_birds
#' @details see [MicroWNV::step_birds.SIRS_deterministic] and [MicroWNV::step_birds.SIRS_stochastic]
#' @importFrom stats pexp
#' @export
step_birds.SIRS <- function(model) {
  NextMethod()
}

#' @title Update SIRS bird population (deterministic)
#' @inheritParams step_birds
#' @export
step_birds.SIRS_deterministic <- function(model) {

  # get new fledglings and their dispersion
  fledglings <- compute_fledge(model)
  fledglings <- fledglings %*% model$bird$fledge_disperse

  # compute eggs laid
  N <- rowSums(model$bird$SIR)
  eggs <- compute_clutch(model, N)

  # compute differences: S
  S_haz <- model$bird$h + model$bird$mu[, model$global$tnow]
  S_leave <- model$bird$SIR[, "S"] * pexp(q =  S_haz)
  S_toI <- S_leave * (model$bird$h / S_haz)

  # compute differences: I
  I_haz <- model$bird$gamma + model$bird$mu[, model$global$tnow]
  I_leave <- model$bird$SIR[, "I"] * pexp(q =  I_haz)
  I_toR <- I_leave * (model$bird$gamma / I_haz)

  # compute differences: R
  R_haz <- model$bird$r + model$bird$mu[, model$global$tnow]
  R_leave <- model$bird$SIR[, "R"] * pexp(q =  R_haz)
  R_toS <- R_leave * (model$bird$r / R_haz)

  # update
  add_clutch(model, eggs)
  model$bird$SIR[, "S"] <- model$bird$SIR[, "S"] - S_leave + fledglings + R_toS
  model$bird$SIR[, "I"] <- model$bird$SIR[, "I"] - I_leave + S_toI
  model$bird$SIR[, "R"] <- model$bird$SIR[, "R"] - R_leave + I_toR

}

#' @title Update SIRS bird population (stochastic)
#' @inheritParams step_birds
#' @export
step_birds.SIRS_stochastic <- function(model) {
  # get new fledglings and their dispersion
  fledglings <- compute_fledge(model)
  fledglings <- fledglings %*% model$bird$fledge_disperse
}


