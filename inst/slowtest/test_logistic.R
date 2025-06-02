# Setting up a simulation for comparing two proportions and testing that we get
# similar results as obtained from the `pwrss` package.

library("data.table")
progressr::handlers(global=TRUE)
progressr::handlers("progress")

logger::log_info("Testing sample-size calculations: logistic regression")

est <- function(data, ...) {
  glm(y ~ x, data, family = binomial) |>
    lava::estimate(keep="x")
}
outcome <- function(data, par = c(0, 1), type="normal", ...) {
  n <- nrow(data)
  if (type == "normal") {
    x <- rnorm(n)
  } else {
    x <- rbinom(n, 1, 0.5)
  }
  y <- rbinom(n, 1, lava::expit(par[1] + par[2] * x))
  data.table(y = y, x = x)
}
m <- carts::Trial$new(
  info = "Logistic regression",
  outcome = outcome,
  estimators = list(glm = est)
  )

set.seed(1)
m$args_model(par = c(0, 1))
res <- m$estimate_samplesize(
  sa.control = list(R = 5, niter = 250),
  bisection.control = list(R = 200, niter = 7),
  R = 3000
)

pest <- pwrss::pwrss.z.logreg(
  beta0 = 0,
  beta1 = 1,
  power = 0.9,
  distribution = "normal",
  verbose = FALSE
)

expect_equal(pest$n, coef(res), tolerance = 10)

## Binary covariate
set.seed(1)
m$args_model(par = c(0, 1), type="binary")
res2 <- m$estimate_samplesize(
  sa.control = list(R = 5, niter = 250),
  bisection.control = list(R = 200, niter = 7),
  R = 3000
)

pest2 <- pwrss::pwrss.z.logreg(
  beta0 = 0,
  beta1 = 1,
  power = 0.9,
  distribution = "bernoulli",
  verbose = FALSE
)

expect_equal(pest$n, coef(res), tolerance = 20)
