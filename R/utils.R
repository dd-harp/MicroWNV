#' @noRd
approx_equal <- function(a, b, tol = sqrt(.Machine$double.eps)) {
  abs(a - b) < tol
}

#' @noRd
divmod <- function(a,b){
  a <- as.integer(a)
  b <- as.integer(b)
  c(
    quo = a %/% b,
    rem = a %% b
  )
}

#' @noRd
distribute <- function(n,p){
  n <- as.integer(n)
  p <- as.integer(p)
  distn <- rep(0L,p)
  div <- divmod(n,p)
  for(i in 0:(p-1)){
    distn[i+1] <- div[["quo"]] + (i < div[["rem"]])
  }
  return(distn)
}


#' @title Sample a stochastic vector
#' @description Given a vector of counts in cells, `x` and a stochastic matrix `prob`, each
#' row of which describes a probability distribution of how that cell should be
#' distributed among bins, sample destination bins for each cell count, and return
#' a vector giving the number of counts in bins. It is conceptually similar to
#' "stochastically" distributing the vector as `x %*% prob`, which gives the
#' expectation.
#' @param x a vector
#' @param prob a matrix, it must have number of rows equal to `x` and rows that sum
#' to one
#' @return a vector of length equal to the number of columns of `prob`
#' @importFrom stats rmultinom
#' @export
sample_stochastic_vector <- function(x, prob) {
  stopifnot(length(x) == nrow(prob))
  samp <- vapply(X = 1:length(x), FUN = function(i) {
    rmultinom(n = 1, size = x[i], prob = prob[i, ])
  }, FUN.VALUE = numeric(ncol(prob)), USE.NAMES = FALSE)
  rowSums(samp)
}


#' @title Sample a stochastic matrix
#' @description `x` is a matrix with arbitrary number of rows but whose columns
#' are equal to the number of bins that the stochastic matrix `prob` parameterizes
#' a distribution over. Each row of `x` gives a distribution of counts over bins
#' and is resampled according to `prob`. It is conceptually similar to
#' "stochastically" distributing the matrix as `x %*% prob`, which gives the
#' expectation.
#' @param x a matrix
#' @param prob a matrix, it must have number of columns equal to the number of columns of `x` and rows that sum
#' to one
#' @return a matrix whose dimensions equal the original `x`
#' @export
sample_stochastic_matrix <- function(x, prob) {
  stopifnot(ncol(x) == ncol(prob))
  samp <- lapply(X = 1:nrow(x), FUN = function(i) {
    sample_stochastic_vector(x = x[i, ], prob = prob)
  })
  do.call(rbind, samp)
}


#' @noRd
approx_equal <- function(a, b, tol = sqrt(.Machine$double.eps)) {
  abs(a - b) <= tol
}


