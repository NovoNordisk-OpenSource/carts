library("tinytest")

test_rnb <- function() {
  # Check Poisson case
  n <- 100
  lam <- 10
  set.seed(1)
  x <- rnb(n, lam, lam)
  set.seed(1)
  y <- rpois(n, lam)
  expect_equal(x, y)


  # Assert that the mean and variance parametrization works
  n <- 1e4
  lam <- 10
  x <- rnb(n, mean = lam, variance = 2 * lam)
  v1 <- var(x)
  m1 <- mean(x)
  eps <- 1 # threshold for comparing stochastic results
  expect_true(abs(v1 - 2 * lam) < eps)
  expect_true(abs(m1 - lam) < eps)


  # Check that vector parameters works
  n <- 1e4
  w <- rep(c(0, 1), n)
  lam <- 1 + 9 * w
  x <- rnb(length(lam), mean = lam, variance = 15)
  m1 <- mean(x[w == 1])
  m0 <- mean(x[w == 0])
  v1 <- var(x[w == 1])
  v0 <- var(x[w == 0])
  eps <- 1
  expect_true(abs(m1 - lam[w == 1][1]) < eps)
  expect_true(abs(m0 - lam[w == 0][1]) < eps)
  expect_true(abs(v1 - 15) < eps)
  expect_true(abs(v0 - 15) < eps)
  x <- rnb(length(lam), mean = lam, variance = 2 * lam)
  m1 <- mean(x[w == 1])
  m0 <- mean(x[w == 0])
  v1 <- var(x[w == 1])
  v0 <- var(x[w == 0])
  eps <- 1
  expect_true(abs(m1 - lam[w == 1][1]) < eps)
  expect_true(abs(m0 - lam[w == 0][1]) < eps)
  expect_true(abs(v1 - 2 * lam[w == 1][1]) < eps)
  expect_true(abs(v0 - 2 * lam[w == 0][1]) < eps)
}
test_rnb()
