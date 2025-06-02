#' @export
#' @title Simulate from a negative binomial distribution
#' @description Parametrized by mean (rate) and variance. Both parameters can be
#'  vector arguments. For this case with mean = variance = c(r1, r2) and n = 5,
#'  the returned vector contains 5 Poisson samples. Three samples are drawn from
#'  a Poisson distribution with rate r1 (index 1, 3 and 5 in output vector) and
#'  two from a Poisson with rate r2 (index 2 and 4).
#' @param n Number of samples (integer)
#' @param mean Mean vector (rate parameter)
#' @param variance Variance vector
#' @param gamma.variance (optional) poisson-gamma mixture parametrization.
#'   Variance (vector) of gamma distribution with mean 1.
#' @return Vector of n realizations
#' @name rnb
#' @examples
#' with(
#'   data.frame(x = rnb(1e4, mean = 100, var = 500)),
#'   c(mean = mean(x), var = var(x))
#' )
rnb <- function(n, mean, variance = mean, gamma.variance = NULL) {
  if (any(variance < mean)) stop("Not feasible")
  y <- numeric(n)
  if (!is.null(gamma.variance)) {
    gamma.variance <- rep(gamma.variance, length.out = n)
    variance <- mean^2 * gamma.variance + mean
  }
  if (any(variance <= 0)) stop("Non-positive variance")
  mean <- rep(mean, length.out = n)
  variance <- rep(variance, length.out = n)
  idx <- which(mean == variance)
  idx0 <- setdiff(seq_along(y), idx)
  if (length(idx0) > 0) {
    p <- mean[idx0] / variance[idx0]
    ## r <- mean^2/(variance-mean)
    r <- mean[idx0] * p / (1 - p)
    y[idx0] <- rnbinom(length(idx0), size = r, prob = p)
  }
  if (length(idx) > 0) {
    y[idx] <- rpois(length(idx), mean[idx])
  }
  return(y)
}

#' @title Multivariate normal distribution function
#' @description Draw random samples from multivariate normal distribution with
#'   variance given by a correlation matrix.
#' @param n number of samples
#' @param mean matrix with mean values (either a 1xp or nxp matrix)
#' @param cor matrix with correlation (either a 1x((p-1)*p/2) or nx((p-1)*p/2)
#'   matrix. The correlation coefficients must be given in the order R(1,2),
#'   R(1,3), ..., R(1,p), R(2,3), ... R(2,p), ... where R(i,j) is the entry in
#'   row i and column j of the correlation matrix.
#' @param var Optional covariance matrix (instead of 'cor' argument)
#' @export
#' @examples
#' rmvn(10, cor = rep(c(-0.999, 0.999), each = 5))
rmvn <- function(n, mean, cor, var = NULL) {
  if (!is.null(var)) {
    cor <- rbind(var[upper.tri(var)])
  } else {
    if (length(cor) == n) { # one cor. pr obs. => 2-dimensional dist.
      cor <- cbind(cor)
    } else {
      cor <- rbind(cor)
    }
  }
  if (missing(mean)) {
    p <- (1 + sqrt(1 + 8 * ncol(cor))) / 2
    mean <- rep(0, p)
  }
  mean <- rbind(mean)
  .Call("_carts_rmvn", PACKAGE = "carts", n, mean, cor)
}
