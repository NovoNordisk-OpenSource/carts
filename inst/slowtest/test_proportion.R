# Setting up a simulation for a coin flip example to test that we get similar
# results as obtained from the `pwrss` package.

progressr::handlers(global=TRUE)
progressr::handlers("progress")


est <- function(data, ...) {
  glm(y ~ 1, data, family=binomial)  |> lava::estimate()
}
m <- carts::Trial$new(
  info = "Coin flip experiment",
  outcome = function(x, p = 0.5, ...) rbinom(nrow(x), 1, p = p),
  estimators = list(glm = est)
)

set.seed(1)
m$args_model(p = 0.4)
res <- m$estimate_samplesize(
  sa.control = list(R = 5, niter = 250),
  bisection.control = list(R = 200, niter = 7),
  R = 3000
)

pest <- pwrss::pwrss.z.prop(
  p = 0.4,
  p0 = 0.5,
  power = 0.9,
  verbose = FALSE
)

expect_equal(pest$n, coef(res), tolerance = 25)
