#' @title Setup birds with SIRS infection model
#' @description This model interfaces with bloodmeals via the vector `model$bird$h`,
#' giving the per-capita force of infection for birds in each patch.
#' @param model an object from [MicroMoB::make_MicroMoB]
#' @param stochastic should the model update deterministically or stochastically?
#' @param fledge_disperse a dispersal matrix for fledglings; this parameter is part
#' of the adult model rather than fledgling model because the handoff of responsibility
#' between the two occurs at the moment a fledgling leaves its nest, which is exactly
#' the point at which they decide where to disperse as a new adult.
#' @param theta a matrix giving time spent in each bird's home range
#' @param SIR matrix of initial states for each patch
#' @param mu either a scalar, a vector of length `tmax`, or a vector of length `365`
#' giving daily mortality rates
#' @param wf biting weights, should be a vector of length `p` or `NULL` to use `1` for all places/patches.
#' @param b transmission efficiency (mosquitoes to birds)
#' @param c transmission efficiency (birds to mosquitoes)
#' @param gamma inverse of infectious duration (recovery rate)
#' @param r inverse of immune duration (rate of loss of immunity)
#' @param beta number of eggs produced, per adult, per day
#' @importFrom MicroMoB approx_equal time_varying_parameter
#' @export
setup_birds_SIRS <- function(model, stochastic, fledge_disperse, theta, SIR, mu, wf = NULL, b = 0.55, c = 0.15, gamma = 1/5, r = 1/120, beta = 0.5) {
  stopifnot(inherits(model, "MicroMoB"))
  stopifnot(inherits(fledge_disperse, "matrix"))
  stopifnot(inherits(theta, "matrix"))
  stopifnot(is.logical(stochastic))

  p <- model$global$p
  tmax <- model$global$tmax

  stopifnot(nrow(theta) == ncol(theta))
  stopifnot(nrow(theta) == p)
  stopifnot(approx_equal(rowSums(theta), 1))

  stopifnot(nrow(fledge_disperse) == ncol(fledge_disperse))
  stopifnot(nrow(fledge_disperse) == p)
  stopifnot(approx_equal(rowSums(fledge_disperse), 1))

  stopifnot(length(mu) > 0)
  stopifnot(mu > 0)
  stopifnot(is.finite(mu))

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

  model$bird$EIR <- rep(0, p)
  model$bird$h <- rep(0, p)
  model$bird$mu <- time_varying_parameter(param = mu, tmax = tmax)

  model$bird$b <- b
  model$bird$c <- c
  model$bird$gamma <- gamma
  model$bird$r <- r
}


# update birds over one time step

#' @title Update SIRS bird population
#' @description This function dispatches on the second argument of `model$bird`
#' for stochastic or deterministic behavior.
#' @inheritParams step_birds
#' @details see [MicroWNV::step_birds.SIRS_deterministic] and [MicroWNV::step_birds.SIRS_stochastic]
#' @export
step_birds.SIRS <- function(model) {
  NextMethod()
}

#' @title Update SIRS bird population (deterministic)
#' @inheritParams step_birds
#' @importFrom stats pexp
#' @export
step_birds.SIRS_deterministic <- function(model) {

  model$bird$h <- model$bird$EIR * model$bird$b

  # get new fledglings and their dispersion
  fledglings <- compute_fledge(model)
  fledglings <- fledglings %*% model$bird$fledge_disperse

  # compute differences: S
  S_haz <- model$bird$h + model$bird$mu[model$global$tnow]
  S_leave <- model$bird$SIR[, "S"] * pexp(q =  S_haz)
  S_toI <- S_leave * (model$bird$h / S_haz)

  # compute differences: I
  I_haz <- model$bird$gamma + model$bird$mu[model$global$tnow]
  I_leave <- model$bird$SIR[, "I"] * pexp(q =  I_haz)
  I_toR <- I_leave * (model$bird$gamma / I_haz)

  # compute differences: R
  R_haz <- model$bird$r + model$bird$mu[model$global$tnow]
  R_leave <- model$bird$SIR[, "R"] * pexp(q =  R_haz)
  R_toS <- R_leave * (model$bird$r / R_haz)

  # update
  model$bird$SIR[, "S"] <- model$bird$SIR[, "S"] - S_leave + fledglings + R_toS
  model$bird$SIR[, "I"] <- model$bird$SIR[, "I"] - I_leave + S_toI
  model$bird$SIR[, "R"] <- model$bird$SIR[, "R"] - R_leave + I_toR

  model$bird$SIR <- pmax(model$bird$SIR, 0)

}

#' @title Update SIRS bird population (stochastic)
#' @inheritParams step_birds
#' @importFrom stats pexp rbinom
#' @importFrom MicroMoB sample_stochastic_vector
#' @export
step_birds.SIRS_stochastic <- function(model) {

  model$bird$h <- model$bird$EIR * model$bird$b

  p <- model$global$p

  # get new fledglings and their dispersion
  fledglings <- compute_fledge(model)
  fledglings <- sample_stochastic_vector(x = fledglings, prob = model$bird$fledge_disperse)

  # compute differences: S
  S_haz <- model$bird$h + model$bird$mu[model$global$tnow]
  S_leave <- rbinom(n = p,  size = model$bird$SIR[, "S"], prob = pexp(q = S_haz))
  S_toI <- rbinom(n = p, size = S_leave, prob = model$bird$h / S_haz)

  # compute differences: I
  I_haz <- model$bird$gamma + model$bird$mu[model$global$tnow]
  I_leave <- rbinom(n = p,  size = model$bird$SIR[, "I"], prob = pexp(q = I_haz))
  I_toR <- rbinom(n = p, size = I_leave, prob = model$bird$gamma / I_haz)

  # compute differences: R
  R_haz <- model$bird$r + model$bird$mu[model$global$tnow]
  R_leave <- rbinom(n = p, size = model$bird$SIR[, "R"] , prob = pexp(q =  R_haz))
  R_toS <- rbinom(n = p, size = R_leave, prob = model$bird$r / R_haz)

  # update
  model$bird$SIR[, "S"] <- model$bird$SIR[, "S"] - S_leave + fledglings + R_toS
  model$bird$SIR[, "I"] <- model$bird$SIR[, "I"] - I_leave + S_toI
  model$bird$SIR[, "R"] <- model$bird$SIR[, "R"] - R_leave + I_toR

}


# compute available birds

#' @title Compute available SIRS bird population (\eqn{W_{B}})
#' @inheritParams compute_WB
#' @export
compute_WB.SIRS <- function(model) {
  theta <- model$bird$theta
  WB <- (model$bird$wf * rowSums(model$bird$SIR)) %*% theta
  return(as.vector(WB))
}

# compute bird biting weights

#' @title Compute bird biting weights of SIRS birds (\eqn{w_{f_{B}}})
#' @inheritParams compute_wfB
#' @export
compute_wfB.SIRS <- function(model) {
  model$bird$wf
}

# compute net infectiousness of birds

#' @title Compute net infectiousness of SIRS birds (\eqn{x_{B}})
#' @inheritParams compute_xB
#' @export
compute_xB.SIRS <- function(model) {
  XB <- model$bird$SIR[, "I"] / rowSums(model$bird$SIR)
  return(as.vector(XB * model$bird$c))
}

# compute total bird population

#' @title Compute total SIRS bird population (\eqn{B_{pop}})
#' @inheritParams compute_B_pop
#' @export
compute_B_pop.SIRS <- function(model) {
  return(rowSums(model$bird$SIR))
}

# home range matrix

#' @title Compute SIRS bird time at risk matrix (\eqn{\Psi})
#' @inheritParams compute_PsiB
#' @export
compute_PsiB.SIRS <- function(model) {
  model$bird$theta
}

# eggs laid by birds

#' @title Compute SIRS egg clutches
#' @inheritParams compute_clutch
#' @export
compute_clutch.SIRS <- function(model) {
  rowSums(model$bird$SIR) * model$bird$beta
}

