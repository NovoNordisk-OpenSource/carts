### Unit tests for covar_sim

library("tinytest")
library("data.table")
set.seed(42)

test_estimate_covar_model_full_cond <- function() {
  n <- 1e4
  eps <- 0.1

  m <- lava::lvm(y ~ x) |>
    distribution(~y, lava::normal.lvm()) |>
    distribution(~x, lava::normal.lvm())

  samples <- lava::sim(m, n,
    p = c(
      "y~x" = 2, "y" = 1, "y~~y" = 2, # Y | X ~ N(1 + 2X, 2)
      "x" = 1, "x~~x" = 5 # X ~ N(1, 1)
    )
  )

  # function will define model from left to right. thus, change order to
  # estimate the factors p(y|x) and p(x) in p(x, y) = p(y|x)p(x)
  data <- samples[, c("x", "y")]
  m.est <- estimate_covar_model_full_cond(data=data)

   # mean of marginal p(x)
  expect_equal(m.est$sim.env$mu[["x"]], 1, tolerance = eps)

  # intercept (beta0)
  expect_equal(m.est$sim.env$mu[["y"]], 1, tolerance = eps)
  # for E[Y | X] = beta0 + beta1 * X
  expect_equal(m.est$sim.env$A[1, 2], 2, tolerance = eps) # beta1

  # now estimate  p(x|y) and p(y) for p(x, y) = p(x|y)p(y)
  data <- samples[, c("y", "x")]
  m.est <- estimate_covar_model_full_cond(data=data)
  expect_equal(m.est$sim.env$mu[["y"]], 3, tolerance = eps) # 3 follows from
  # the parameters of the original model m

  s <- sample_covar_parametric_model(1e4, m.est)
  expect_equal(mean(s$y), 3, tolerance = eps)
  expect_equal(var(s$y), 22, tolerance = eps) # marginal variance of y also
  # follows from the original model
}
test_estimate_covar_model_full_cond()

test_sample_covar_parametric_model <- function() {
  # test that factors are correctly sampled
  data.levels <- c("a", "b")
  y <- NULL
  data <- data.table::data.table(y = data.levels[rbinom(1e3, 1, 0.5) + 1]) |>
    transform(y = as.factor(y))

  m <- estimate_covar_model_full_cond(
    data, cond.dist = list(y = lava::binomial.lvm)
  )
  samples <- sample_covar_parametric_model(1e3, m)
  expect_true(all(levels(samples$y) == data.levels))
}
test_sample_covar_parametric_model()


test_covar_bootstrap <- function() {
  data(melanoma, package = "mets")
  d <- transform(melanoma, #nolint
                 Thick = cut(thick, breaks = 8), #nolint
                 thick = scale(thick) #nolint
                 )


  x <- covar_bootstrap(d, ~ ulc + thick + sex)
  d1 <- x(nrow(d))
  expect_true(nrow(d1) == nrow(d))
  # Check distributions are similar in last samples with replacement
  d1 <- x(10000L)
  expect_true(nrow(d1) == 10000L)
  expect_equivalent(colMeans(d1), colMeans(d[, colnames(d1)]), tolerance = .1)
  expect_true(sum((cov(d1) - cov(d[, colnames(d1)]))**2) < .1)
  # Testing that the factor levels are unaltered
  x <- covar_bootstrap(d, ~ ulc + Thick + sex)
  d1 <- x(nrow(d))
  expect_true(all(levels(d1$Thick) == levels(d$Thick)))
}
test_covar_bootstrap()


test_covar_join <- function() {
  n <- 10

  # joining generators that return data.frames / data.tables (single period)
  c1 <- function(n) data.frame(a = rnorm(n))
  c2 <- function(n) data.table(b = rnorm(n))

  # output matches data format of first input function
  expect_true(is.data.frame(covar_join(c1, c2)(n)))
  expect_true(is.data.table(covar_join(c2, c1)(n)))

  # column position reflects position of covariate functions
  expect_equal(names(covar_join(c2, c1)(n)), c("b", "a"))
  # do not allow duplicated column names
  expect_error(covar_join(c1, c1)(n))

  # no error for joining covariate functions with shared argument names
  # this implicitly tests that arguments are passed to downstream functions
  f <- covar_join(
    function(n, p = 0) data.frame(a = rbinom(n, 1, p)),
    function(n, p = 0) data.frame(b = rbinom(n, 1, p))
  )
  x <- f(n, p=1)
  expect_equal(c(unique(x$a), unique(x$b)), c(1, 1))

  # disable using named optional arguments to add covariate name
  expect_error(covar_join(c1, b = \(n) rnorm(n))(n))

  # multiple periods
  base <- setargs(covar_loggamma, normal.cor = .5)
  f <- base %join%
    function(n) {
      list(data.frame(a = rbinom(n, 1, 0.5)), data.frame(a = rbinom(n, 1, 0.5)))
    }
  x <- f(n)
  # expect different realizations per period
  expect_false(all(x[[1]]$a == x[[2]]$a))
  # check that output is converted from data.frame to data.table
  expect_true(inherits(x[[1]], "data.table"))

  f <- base %join% function(n) list(data.frame(a = rbinom(n, 1, 0.5)))
  # expect error because the added covariate generator function returns a list.
  # therefore, covar_join expects the covariates to be available for two periods
  expect_error(f(10))

  # correctly adding baseline covariates
  x <- (base %join% function(n) data.frame(w = rnorm(n)))(n)
  expect_equal(x[[1]]$w, x[[2]]$w)

  # piping multiple unnamed functions together works as expected
  f <- (function(n) data.frame(w1 = rnorm(n))) %join%
    (function(n) data.frame(w2 = rnorm(n))) %join%
    function(n) data.frame(w3 = rnorm(n))
  x <- f(n)
  expect_equal(nrow(x), n)
  expect_equal(names(x), paste0("w", seq(3)))
}
test_covar_join()

test_covar_add <- function() {
  n <- 10
  base <- function(n, ...) {
    list(data.frame(x = rnorm(n)), data.frame(x = rnorm(n)))
  }

  # do not support adding covariates with different names per period
  xt <- function(n) covar_add(base(n), list(a = 0, b = 1))
  expect_error(xt(n))
  # do not support duplicated column names
  xt <- function(n) covar_add(base(n), list(x = 0, x = 1))
  expect_error(xt(n))

  # adding treatment indicator that various between both periods
  xt <- function(n) covar_add(base(n), list(a = 0, a = 1))
  x <- xt(n)
  expect_equal(x[[1]]$a, rep(0, n))
  expect_equal(x[[2]]$a, rep(1, n))

  # test that elements in the list remain data.frames
  expect_true(is.data.frame(x[[1]]))

  base <- function(n, ...) {
    list(data.table(x = rnorm(n)), data.table(x = rnorm(n)))
  }
  # adding baseline covariates
  xt <- function(n) covar_add(base(n), \(n) data.frame(w = rnorm(n)))
  # output remains a data.table even though data.frames are joined
  x <- xt(n)
  expect_true(is.data.table(x[[1]]))
  # baseline covariates are the same in both periods
  expect_equal(x[[1]]$w, x[[2]]$w)
  # names argument overwrites covariate name
  xt <- function(n) covar_add(base(n), list(u=rnorm(n)), names = "w")
  expect_true("w" %in% colnames(xt(n)[[1]]))
  # error when supplied names do not match number of new covariates
  xt <- function(n) covar_add(base(n), list(u=rnorm(n)), names = c("w1", "w2"))
  expect_error(xt(5))
}
test_covar_add()


test_cor <- function() {
  ## Compound symmetry
  p <- carts:::setup_cor(0.3, p = 5, type = "cs")
  expect_equivalent(
    carts:::par2cor(cor = p, type = "un"),
    diag(nrow = 5) * .7 + 0.3
  )

  ## Auto-regressive AR(1)
  p <- carts:::setup_cor(0.9, p = 5, type = "ar")
  R <- carts:::par2cor(cor = p)
  R2 <- carts:::par2cor(cor = 0.9, type = "ar", var = rep(1, 5))
  expect_equivalent(R, R2)
  expect_equivalent(R[1, ], 0.9**(0:4))
  expect_equivalent(diag(R), rep(1, 5))
  expect_equivalent(diag(R[, -1]), rep(0.9, 4))
  expect_equivalent(diag(R[, -(1:2)]), rep(0.9**2, 3))

  ## Toeplitz
  inp <- c(0.2, 0.2, 0.05)
  p <- carts:::setup_cor(inp, p = 4, type = "to")
  R <- carts:::par2cor(cor = p)
  for (i in 1:3) {
    expect_equivalent(diag(R[, -seq_len(i), drop = FALSE]), rep(inp[i], 4 - i))
  }
}
test_cor()

test_covar_loggamma <- function() {
  z <- covar_loggamma(1e4, gamma.var = c(1, 2), normal.cor = 0.5)
  ## Check mean-structure
  m1 <- mean(exp(z[[1]][[1]]))
  m2 <- mean(exp(z[[2]][[1]]))
  expect_equivalent(m1, 1, tolerance = .1)
  expect_equivalent(m2, 1, tolerance = .1)
  ## Check variance
  v1 <- var(exp(z[[1]][[1]]))[1]
  v2 <- var(exp(z[[2]][[1]]))[1]
  expect_equivalent(v1, 1, tolerance = .3)
  expect_equivalent(v2, 2, tolerance = .3)
  ## Check correlation
  expect_true(cor(z[[1]], z[[2]], method = "spearman") > 0.3)
  z0 <- covar_loggamma(1e4, gamma.var = c(1, 2), normal.cor = 0)
  expect_true(abs(cor(z0[[1]], z0[[2]], method = "spearman")) < 0.1)

  ## Check varying correlation parameter
  n <- 5e3
  rho <- rep(c(0, 1), each = n)
  z <- covar_loggamma(length(rho), normal.cor = rho)
  expect_true(cor(z[[1]][[1]][1:n], z[[2]][[1]][1:n],
                  method = "spearman") < 0.1)
  expect_true(cor(z[[1]][[1]][(n + 1):length(rho)],
                  z[[2]][[1]][(n + 1):length(rho)],
                  method = "spearman"
                  ) > 0.99)
  ## Check varying variance parameters
  x <- rep(c(0, 1), each = n)
  z <- covar_loggamma(length(x),
    normal.cor = 1,
    gamma.var = cbind(1 + x, 2 - x)
    )
  eps <- .3
  v1 <- var(exp(z[[1]][[1]][1:n]))
  v2 <- var(exp(z[[1]][[1]][(n + 1):length(x)]))
  expect_equivalent(v1, 1, tolerance = eps)
  expect_equivalent(v2, 2, tolerance = eps)

  v1 <- var(exp(z[[2]][[1]][1:n]))
  v2 <- var(exp(z[[2]][[1]][(n + 1):length(x)]))
  expect_equivalent(v1, 2, tolerance = eps)
  expect_equivalent(v2, 1, tolerance = eps)

  ## Multiple observations
  z <- carts::covar_loggamma(
    10, normal.cor = 1, gamma.var = cbind(0.5, 1, 2, 3)
  )
  expect_true(length(z) == 4)
}
test_covar_loggamma()

test_covar_normal <- function() {
  z <- covar_normal(1e4, normal.var = c(1, 0.5), normal.cor = 0.5)
  ## Check mean-structure
  m1 <- mean(z[[1]][[1]])
  m2 <- mean(z[[2]][[1]])
  expect_equivalent(m1, 0, tolerance = .1)
  expect_equivalent(m2, 0, tolerance = .1)
  ## Check variance
  v1 <- var(z[[1]][[1]])[1]
  v2 <- var(z[[2]][[1]])[1]
  expect_equivalent(v1, 1, tolerance = .2)
  expect_equivalent(v2, .5, tolerance = .2)
  ## Check varying correlation parameter
  n <- 5e3
  rho <- rep(c(0, 1), each = n)
  z <- covar_normal(length(rho), normal.cor = rho)
  expect_true(cor(z[[1]][[1]][1:n], z[[2]][[1]][1:n],
                  method = "spearman") < 0.1)
  expect_true(cor(z[[1]][[1]][(n + 1):length(rho)],
                  z[[2]][[1]][(n + 1):length(rho)],
                  method = "spearman") > 0.99)
  ## Check varying variance parameters
  n <- 5e3
  x <- rep(c(0, 1), each = n)
  z <- covar_normal(length(x), normal.cor=1, normal.var = cbind(1 + x, 2 - x))
  eps <- .3
  v1 <- var(z[[1]][[1]][1:n])
  v2 <- var(z[[1]][[1]][(n + 1):length(x)])
  expect_equivalent(v1, 1, tolerance = eps)
  expect_equivalent(v2, 2, tolerance = eps)

  v1 <- var(z[[2]][[1]][1:n])
  v2 <- var(z[[2]][[1]][(n + 1):length(x)])
  expect_equivalent(v1, 2, tolerance = eps)
  expect_equivalent(v2, 1, tolerance = eps)

  ## Multiple observations
  z <- covar_normal(10, normal.var = cbind(0.5, 1, 2, 3), normal.cor=1)
  expect_true(length(z) == 4)
}
test_covar_normal()
