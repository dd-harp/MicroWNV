#' @title Compute human blood feeding fraction (\eqn{q})
#' @description This function is redefined from `MicroMoB` because the computation of
#' \eqn{q} may differ with modeling of birds.
#' @param model an object from [MicroMoB::make_MicroMoB]
#' @param W a vector of length `p` giving human availability by patch (\eqn{W})
#' @param WB a vector of length `p` giving bird availability by patch (\eqn{W_{B}})
#' @export
compute_q <- function(model, W, WB) {
  UseMethod("compute_q", model$mosquito)
}

#' @title Compute human blood feeding fraction for RM model (\eqn{q})
#' @description This method simply returns the `q` parameter of the mosquito object,
#' because the RM model assumes a constant fraction of blood meals are taken on
#' human hosts.
#' @param model an object from [MicroMoB::make_MicroMoB]
#' @param W a vector of length `p` giving human availability by patch (\eqn{W})
#' @param WB a vector of length `p` giving bird availability by patch (\eqn{W_{B}})
#' @export
compute_q.RM <- function(model, W, WB) {
  model$mosquito$q
}
