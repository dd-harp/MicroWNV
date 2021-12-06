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
