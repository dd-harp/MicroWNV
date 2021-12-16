#' @title Sample a stochastic matrix
#' @description Given a vector of counts in cells, `x` and a matrix `prob`, each
#' row of which describes a probability distribution of how that cell should be
#' distributed among bins, sample destination bins for each cell count, and return
#' a vector giving the number of counts in bins.
#' @param x a vector
#' @param prob a matrix, it must have number of rows equal to `x` and rows that sum
#' to one
#' @return a vector of length equal to the number of columns of `prob`
#' @importFrom stats rmultinom
#' @export
sample_stochastic_matrix <- function(x, prob) {
  stopifnot(length(x) == nrow(prob))
  samp <- vapply(X = 1:length(x), FUN = function(i) {
    rmultinom(n = 1, size = x[i], prob = prob[i, ])
  }, FUN.VALUE = numeric(ncol(prob)), USE.NAMES = FALSE)
  rowSums(samp)
}


#' @noRd
approx_equal <- function(a, b, tol = sqrt(.Machine$double.eps)) {
  abs(a - b) <= tol
}

ZZ <- matrix(data = c(10, 100, 1000,
                      20, 200, 2000), nrow = 2, ncol = 3, byrow = T)

psi <- matrix(
  c(0.9, 0.025, 0.075,
    0.15, 0.6, 0.25,
    0.05, 0.2, 0.75),nrow = 3, ncol = 3,byrow = T
)

rbind(
ZZ[1, ] %*% psi,
ZZ[2,] %*% psi
)

rbind(
(ZZ[1,1] * psi[1,]) + (ZZ[1,2] * psi[2,]) + (ZZ[1,3] * psi[3,]),
(ZZ[2,1] * psi[1,]) + (ZZ[2,2] * psi[2,]) + (ZZ[2,3] * psi[3,])
)

ZZ %*% psi


vapply(X = 1:2, FUN = function(x) {
  sample_stochastic_matrix(x = ZZ[x, ], prob = psi)
}, FUN.VALUE = numeric(3), USE.NAMES = FALSE)

t(vapply(X = 1:2, FUN = function(x) {
  sample_stochastic_matrix(x = ZZ[x, ], prob = psi)
}, FUN.VALUE = numeric(3), USE.NAMES = FALSE))
