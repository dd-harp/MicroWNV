# classes and methods to implement a reasonably detailed RM model of mosquitoes

#' @title Setup generalized Ross-Macdonald mosquito model
#' @description This requires the birds to be set up prior to being called.
#' @param model an object from [MicroWNV::make_microWNV]
#' @param stochastic should the model update deterministically or stochastically?
#' @param a the feeding rate on humans and birds (normally calculated as \eqn{a = fq} using
#' Ross-Macdonald parameters)
#' @param eip the Extrinsic Incubation Period, may either be a scalar, a vector of
#' length 365, or a vector of length equal to `tmax` in the `model` object from [MicroWNV::make_microWNV]
#' @param p daily survival probability, may either be a scalar, a vector of
#' length 365, or a vector of length equal to `tmax` in the `model` object from [MicroWNV::make_microWNV]
#' @param psi a mosquito dispersal matrix (rows must sum to 1)
#' @param M total mosquito density per patch
#' @param Y density of incubating mosquitoes per patch
#' @param Z density of infectious mosquitoes per patch
#' @export
setup_mosquito_RM <- function(model, stochastic, a, eip, p, psi, M, Y, Z) {
  stopifnot(inherits(model, "microWNV"))
  stopifnot(is.logical(stochastic))

  if (length(eip) == 1L) {
    stopifnot(is.finite(eip))
    stopifnot(eip > 0)
    eip_vec <- rep(eip, model$global$tmax)
  } else if(length(eip) == 365L) {
    stopifnot(is.finite(eip))
    stopifnot(eip > 0)
    ix <- (1:model$global$tmax) %% 365L
    ix[which(ix == 0L)] <- 365L
    eip_vec <- eip[ix]
  } else if(length(eip) == model$global$tmax) {
    stopifnot(is.finite(eip))
    stopifnot(eip > 0)
    eip_vec <- eip
  } else {
    stop("incorrect length of eip vector")
  }

  maxEIP <- max(eip_vec)

  if (length(p) == 1L) {
    stopifnot(is.finite(p))
    stopifnot(p > 0)
    p_vec <- rep(p, model$global$tmax)
  } else if(length(p) == 365L) {
    stopifnot(is.finite(p))
    stopifnot(p > 0)
    ix <- (1:model$global$tmax) %% 365L
    ix[which(ix == 0L)] <- 365L
    p_vec <- p[ix]
  } else if(length(p) == model$global$tmax) {
    stopifnot(is.finite(p))
    stopifnot(p > 0)
    p_vec <- p
  } else {
    stop("incorrect length of p vector")
  }

  stopifnot(p_vec <= 1)
  stopifnot(p_vec >= 0)
  stopifnot(!is.null(p_vec))

  stopifnot(dim(psi) == model$global$p)
  stopifnot(approx_equal(a = rowSums(psi), b = 1))

  mosy_class <- c("RM")
  if (stochastic) {
    mosy_class <- c(mosy_class, "RM_stochastic")
  } else {
    mosy_class <- c(mosy_class, "RM_deterministic")
  }

  model$mosquito <- structure(list(), class = mosy_class)
  model$mosquito$a <- a
  model$mosquito$eip <- eip_vec
  model$mosquito$maxEIP <- maxEIP
  model$mosquito$p <- p_vec
  model$mosquito$psi <- psi

  model$mosquito$kappa <- rep(0, model$global$p)

  stopifnot(length(M) == model$global$p)
  stopifnot(length(Y) == model$global$p)
  stopifnot(length(Z) == model$global$p)
  stopifnot(is.finite(M))
  stopifnot(is.finite(Y))
  stopifnot(is.finite(Z))
  stopifnot(M >= Y)
  stopifnot(Y >= Z)

  model$mosquito$M <- M # mosquito density
  model$mosquito$Y <- Y # infected (incubating)
  model$mosquito$Z <- Z # infectious
  model$mosquito$ZZ <- matrix(data = 0, nrow = maxEIP, ncol = model$global$p) # each row is the number that will be added to the infectious state on that day

  ZZ_shift <- matrix(0, nrow = maxEIP, ncol = maxEIP)
  ZZ_shift[1:(maxEIP-1), 2:maxEIP] <- diag(maxEIP-1)
  model$mosquito$ZZ_shift <- ZZ_shift

}


# update mosquitoes over one time step

#' @title Update Ross-Macdonald mosquitoes
#' @description This function dispatches on the second argument of `model$mosquito`
#' for stochastic or deterministic behavior.
#' @inheritParams step_mosquitoes
#' @details see [MicroWNV::step_mosquitoes.RM_deterministic] and [MicroWNV::step_mosquitoes.RM_stochastic]
#' @export
step_mosquitoes.RM <- function(model) {
  NextMethod()
}

#' @title Update Ross-Macdonald mosquitoes (deterministic)
#' @inheritParams step_mosquitoes
#' @export
step_mosquitoes.RM_deterministic <- function(model) {

  # parameters
  tnow <- model$global$tnow
  EIP <- model$mosquito$eip[tnow]
  maxEIP <- model$mosquito$maxEIP
  p <- model$mosquito$p[tnow]
  psi <- model$mosquito$psi

  # newly infected mosquitoes
  Y0 <- model$mosquito$a * model$mosquito$kappa * (model$mosquito$M - model$mosquito$Y)
  Y0 <- pmax(Y0, 0)

  # newly emerging adults
  lambda <- compute_emergents(model)

  # survival
  model$mosquito$M <- (p * model$mosquito$M) + lambda
  model$mosquito$Y <- p * (model$mosquito$Y + Y0)
  model$mosquito$Z <- p * model$mosquito$Z
  model$mosquito$ZZ <- p * model$mosquito$ZZ

  # dispersal
  model$mosquito$M = model$mosquito$M %*% psi
  model$mosquito$Y = model$mosquito$Y %*% psi
  model$mosquito$Z = (model$mosquito$Z + model$mosquito$ZZ[1, ]) %*% psi
  model$mosquito$ZZ <- model$mosquito$ZZ %*% psi

  # ZZ[t, ] is the number of mosquitoes that become infectious in each patch t days from now.
  model$mosquito$ZZ <- model$mosquito$ZZ_shift %*% model$mosquito$ZZ
  model$mosquito$ZZ[EIP, ] <- model$mosquito$ZZ[EIP, ] + (psi %*% Y0 * p)

  # make vectors
  model$mosquito$M <- as.vector(model$mosquito$M)
  model$mosquito$Y <- as.vector(model$mosquito$Y)
  model$mosquito$Z <- as.vector(model$mosquito$Z)

}

#' @title Update Ross-Macdonald mosquitoes (stochastic)
#' @inheritParams step_mosquitoes
#' @importFrom stats rbinom rmultinom
#' @export
step_mosquitoes.RM_stochastic <- function(model) {

  # parameters
  tnow <- model$global$tnow
  EIP <- model$mosquito$eip[tnow]
  maxEIP <- model$mosquito$maxEIP
  p <- model$mosquito$p[tnow]
  psi <- model$mosquito$psi
  n_patch <- model$global$p

  # newly emerging adults
  lambda <- compute_emergents(model)

  # survival
  m_deaths <- rbinom(n = n_patch, size = model$mosquito$M, prob = 1 - p)
  y_deaths <- suppressWarnings(rbinom(n = n_patch, size = m_deaths, prob = model$mosquito$Y / model$mosquito$M))
  y_deaths[is.na(y_deaths)] <- 0
  z_deaths <- suppressWarnings(rbinom(n = n_patch, size = y_deaths, prob = model$mosquito$Z / model$mosquito$Y))
  z_deaths[is.na(z_deaths)] <- 0

  zz_deaths <- vapply(X = 1:n_patch, FUN = function(x) {
    probs <- model$mosquito$ZZ[, x] > 0 # scatter deaths across populated bins
    if (all(!probs)) {
      rep(0, n_patch)
    } else {
      rmultinom(n = 1, size = y_deaths[x], prob = probs)
    }
  },FUN.VALUE = numeric(n_patch), USE.NAMES = FALSE)

  model$mosquito$M <- model$mosquito$M - m_deaths + lambda
  model$mosquito$Y <- model$mosquito$Y - y_deaths
  model$mosquito$ZZ <- model$mosquito$ZZ - zz_deaths
  model$mosquito$Z <- model$mosquito$Z - z_deaths + model$mosquito$ZZ[1, ]

  # newly infected mosquitoes, and dispersal
  h <- model$mosquito$a * model$mosquito$kappa
  Y0 <- rbinom(n = n_patch, size = model$mosquito$M - model$mosquito$Y, prob = h)
  Y0 <- sample_stochastic_matrix(x = Y0, prob = psi)

  # dispersal
  model$mosquito$M <- sample_stochastic_matrix(x = model$mosquito$M, prob = psi)
  model$mosquito$Y <- sample_stochastic_matrix(x = model$mosquito$Y, prob = psi)
  model$mosquito$Z <- sample_stochastic_matrix(x = model$mosquito$Z, prob = psi)
  model$mosquito$ZZ <- t(vapply(X = 1:maxEIP, FUN = function(d) {
    sample_stochastic_matrix(x = model$mosquito$ZZ[d, ], prob = psi)
  }, FUN.VALUE = numeric(n_patch), USE.NAMES = FALSE))

  # add newly infecteds
  model$mosquito$Y <- model$mosquito$Y + Y0

  # ZZ[t, ] is the number of mosquitoes that become infectious in each patch t days from now.
  model$mosquito$ZZ <- model$mosquito$ZZ_shift %*% model$mosquito$ZZ
  model$mosquito$ZZ[EIP, ] <- model$mosquito$ZZ[EIP, ] + Y0

}
