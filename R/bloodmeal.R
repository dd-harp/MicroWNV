#' @title Compute bloodmeals taken by mosquitoes on hosts
#' @description This should be run prior to any `step` functions to update
#' components over a time step. It computes various quantities related to
#' disease transmission between species using the generic interfaces (methods)
#' provided by each component. It updates `h`, vectors giving per-capita force
#' of infection for the bird and human components, and `kappa`, the net infectiousness
#' of hosts for the mosquito component.
#' @param model an object from [MicroMoB::make_MicroMoB]
#' @importFrom MicroMoB compute_H compute_x compute_wf compute_Psi
#' @importFrom MicroMoB compute_f compute_Z
#' @export
compute_bloodmeal <- function(model) {
  stopifnot(inherits(model, "MicroMoB"))

  n <- model$global$n
  p <- model$global$p

  # human quantities
  H <- compute_H(model)
  x <- compute_x(model)
  wf <- compute_wf(model)
  Psi <- compute_Psi(model)
  W <- as.vector(t(Psi) %*% (wf * H))

  # bird quantities
  WB <- compute_WB(model)
  B_pop <- compute_B_pop(model)
  xB <- compute_xB(model)
  wfB <- compute_wfB(model)
  PsiB <- compute_PsiB(model)

  # human biting distribution matrix (n x p)
  beta_H <- diag(wf, nrow = n, ncol = n) %*% Psi %*% diag(1/W, nrow = p, ncol = p)

  # bird biting distribution matrix (n x p)
  beta_B <- diag(wfB, nrow = p, ncol = n) %*% PsiB %*% diag(1/WB, nrow = p, ncol = p)

  stopifnot(nrow(beta_H) == n)
  stopifnot(ncol(beta_H) == p)

  stopifnot(nrow(beta_B) == p)
  stopifnot(ncol(beta_B) == p)

  # total availability of blood hosts
  B <- W + WB

  # blood feeding rates
  f <- compute_f(model, B = B)

  # human blood feeding fraction
  q <- compute_q(model, W = W, WB = WB)

  # density of infectious mosquitoes
  Z <- compute_Z(model)

  # calculate EIR and kappa (mosy->human, human->mosy
  model$human$EIR <- beta_H %*% (f*q*Z)
  model$human$EIR <- as.vector(model$human$EIR)

  model$bird$EIR <-  beta_B %*% (f*(1-q)*Z)
  model$bird$EIR <- as.vector(model$bird$EIR)

  model$mosquito$kappa <- (q * (t(beta_H) %*% (x*H))) + ((1 - q) * (t(beta_B) %*% (xB*B_pop)))
  model$mosquito$kappa <- as.vector(model$mosquito$kappa)

  stopifnot(length(model$human$EIR) == n)
  stopifnot(length(model$bird$EIR) == p)
  stopifnot(length(model$mosquito$kappa) == p)

}
