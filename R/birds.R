#' @title Setup birds with SIRS infection model
#' @description This requires the humans to be set up prior to being called.
#' @param model an object from [MicroWNV::make_microWNV]
#' @param stochastic should the model update deterministically or stochastically?
#' @param fledge_disperse a dispersal matrix for fledglings
#' @param theta a matrix giving time spent in each bird's home range
#' @param SIR matrix of initial states for each patch
#' @param wf biting weights
#' @param b transmission efficiency (mosquitoes to birds)
#' @param c transmission efficiency (birds to mosquitoes)
#' @param gamma inverse of infectious duration
#' @param r recovery rate
#' @export
setup_birds_SIRS <- function(model, stochastic, fledge_disperse, theta, SIR, wf = NULL, b = 0.55, c = 0.15, gamma = 1/5, r = 1/120) {
  stopifnot(inherits(model, "microWNV"))
  stopifnot(inherits(fledge_disperse, "matrix"))
  stopifnot(inherits(theta, "matrix"))
  stopifnot(is.logical(stochastic))
  stopifnot(!is.null(model$human))

  p <- model$global$p
  n <- model$global$n

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

  model$bird$b <- b
  model$bird$c <- c
  model$bird$gamma <- gamma
}


compute_fledge <- function(model) {

}



#' @title Update bird population
#' @param model an object from [MicroWNV::make_microWNV]
#' @export
step_birds <- function(model) {
  UseMethod("step_birds", model$bird)
}

#' @title Update SIRS bird population
#' @inheritParams step_birds
#' @details see [MicroWNV::step_birds.SIRS_deterministic] and [MicroWNV::step_birds.SIRS_stochastic]
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
  # compute new adults vectors
}

#' @title Update SIRS bird population (stochastic)
#' @inheritParams step_birds
#' @export
step_birds.SIRS_stochastic <- function(model) {
  # get new fledglings and their dispersion
  fledglings <- compute_fledge(model)
  fledglings <- fledglings %*% model$bird$fledge_disperse
}


