### Unit tests for optimization methods

library("tinytest")
set.seed(1)

test_bisection <- function() {
  logger::log_threshold(logger::WARN)

  bisection <- carts:::bisection
  root <- 5
  f <- function(x) (x - root)
  res <- bisection(f, c(0, 1000), niter = 30)
  expect_equal(res, root, tolerance = 1e-4)

  # test tolerance argument
  res2 <- carts:::bisection(f, c(0, 1000), niter = 30, tol = 1)
  expect_true(res != res2)

  # iter argument works as expected (0 iterations returns the center of the
  # interval)
  interval <- c(4, 6)
  res <- bisection(f, interval, niter = 0)
  expect_equal(res, (interval[2] - interval[1]) / 2 + interval[1])

  # return min and max of interval when root lays on either side of the interval
  root <- min(interval) - 1
  expect_equal(bisection(function(x) (root - x), interval), min(interval))

  root <- max(interval) + 1
  expect_equal(bisection(function(x) (root - x), interval), max(interval))
}
test_bisection()

test_optim_sa <- function() {
  logger::log_threshold(logger::WARN)
  optim_sa <- carts:::optim_sa
  # Finding approximate median u, P(X<=u) = .5, of X~Exp(1):
  control_args <- list(niter = 2000, alpha = .5) # Polyak–Ruppert
  f <- function(x) mean(rexp(10) <= x) - 0.5
  res <- optim_sa(f, 0, control = control_args)
  expect_equal(res$estimate, qexp(0.5), tolerance = 0.1)
  # check that burn.in parameter check is performed
  expect_error(optim_sa(f, 0, burn.in = -1.1))

  # check that burn.in arguments works with integer argument
  res <- optim_sa(f, 0, control = control_args, burn.in = 100)
  expect_true(length(res$running.mean) == control_args$niter - 100)
# check that burn.in arguments works with fractional argument
  res <- optim_sa(f, 0, control = control_args, burn.in = 0.1)
  expect_true(length(res$running.mean) == (control_args$niter * (1 - 0.1)))

  # With standard Robbins-Monro
  res <- optim_sa(f, 0, control = list(niter = 2000, alpha = 1))
  expect_equal(res$estimate, qexp(0.5), tolerance = 0.1)

  # test projection argument to constrain minimum of estimate to be 1
  lower_limit <- 1
  projection <- function(x) max(x, lower_limit)
  res <- optim_sa(f, 0, control = control_args, projection = projection)
  expect_equal(res$estimate, lower_limit, tolerance = 0.1)

  # optional arguments are passed to f
  fq <- function(x, q) mean(rexp(10) <= x) - q
  q <- 0.75
  res <- optim_sa(fq, 0, q = q, control = control_args)
  expect_equal(res$estimate, qexp(q), tolerance = 0.1)

  res <- optim_sa(fq, 0, function.args = list(q = q), control = control_args)
  expect_equal(res$estimate, qexp(q), tolerance = 0.1)

  # test that discrete/interpolate pass integers to stochastic function
  fint_warn <- function(n) {
    if (abs(floor(n) - n) > 1e-12) warning("no integer")
    return(mean(20 + 2 * n + rnorm(10)))
  }
  expect_silent(optim_sa(fint_warn, 0.5, method = "discrete"))
  expect_silent(optim_sa(fint_warn, 0.5, method = "interpolate"))
  expect_warning(optim_sa(fint_warn, 0.5, method = "standard"))

  # optimization returns integer
  control <- list(niter = 1000, alpha = 1, stepmult = 0.25)
  fint <- function(n) mean(20 + 2 * n + rnorm(10))
  res <- optim_sa(fint, 1, method = "discrete", control = control)
  expect_equal(res$estimate, -10)
  res <- optim_sa(fint, 1, method = "interpolate", control = control)
  expect_equal(res$estimate, -10)
  # now with Polyak–Ruppert
  control$alpha <- 0.2
  control$stepmult <- 0.01 # lower stepmult for algorithm to converge
  res <- optim_sa(fint, 1, method = "discrete", control = control)
  expect_equal(res$estimate, -10)
  res <- optim_sa(fint, 1, method = "interpolate", control = control)
  expect_equal(res$estimate, -10)
}
test_optim_sa()
