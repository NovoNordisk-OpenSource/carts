
<!-- README.md is generated from inst/README.Rmd. Please edit that file -->

<!-- badges: start -->

[![complete-checks](https://github.com/NN-AI-Analytics/carts/actions/workflows/r-cmd-check.yaml/badge.svg)](https://github.com/NN-AI-Analytics/carts/actions/workflows/r-cmd-check.yaml)
[![vignettes](https://github.com/NN-AI-Analytics/carts/actions/workflows/vignettes.yaml/badge.svg)](https://github.com/NN-AI-Analytics/carts/actions/workflows/vignettes.yaml)
[![slowtests](https://github.com/NN-AI-Analytics/carts/actions/workflows/slow-tests.yaml/badge.svg)](https://github.com/NN-AI-Analytics/carts/actions/workflows/slow-tests.yaml)
<!-- badges: end -->

# carts package

The `carts` R package is a simulation tool for exploring various
estimators within a clinical trial context under differing assumptions.
The package provides a user-friendly interface for defining a clinical
trial object with different choices for its design, patient and endpoint
distributions, and treatment effect estimators. Once a clinical trial
object is specified, the package provides the functionality for
estimating the power with Monte Carlo simulations.

## Example

Here we emulate a simple parallel trial design with both observed and
unobserved covariates. The treatment effect is estimated using an
estimator based on the efficient influence function (EIF) where we
adjust for the observed covariate x.

The necessary sample-size to achieve 90% power for a one-sided
~superiority test is estimated using a variation of a bisection and a
Robbins-Monro stochastic approximation algorithm with parallelized
computations

``` r
library("carts")
```

    ## Loading required package: lava

``` r
library("data.table")
future::plan(future::multicore)
## progressr::handlers(global = TRUE)
## progressr::handlers(progressr::handler_cli)

## Covariates at baseline
x0 <- function(n, pa = 0.5, gamma.var = 0.7, ...) {
  data.table(
    a = rbinom(n, 1, pa), ## Treatment
    x = rnorm(n), ## Obs.
    z = log(rgamma(n, shape = 1 / gamma.var, rate = 1 / gamma.var)) ## Unobs.
  )
}
## Outcome model
outcome <- function(data, b = c(log(2.5), log(0.38)), ...) {
  X <- model.matrix(~ 1 + a, data)
  rate <- exp(X %*% b + with(data, x + z))
  data.table(y = rpois(length(rate), rate))
}

qmodel  <- targeted::learner_glm(y ~ a * x, family = poisson)
m <- Trial$new(
  covariates = x0,
  outcome = outcome,
  estimators = list(adj = est_adj(qmodel))
  )

## Sample-size estimation via Stochastic Approximation
e <- m$estimate_samplesize(R = 1000)
```

    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'
    ## Warning in cate_est(y = y, a = cbind(a), p = cbind(p[[x]]), q = cbind(q[[x]]),
    ## : partial argument match of 'X.cat' to 'X.cate'

``` r
print(e)
```

    ## ── Estimated sample-size to reach 90% power ── 
    ## 
    ## n = 91 (actual estimated power≈89.16%)

## Installation

The package can be installed on all unix systems from the command line
via `make install`. Already installed R packages that are dependencies
of `carts` will not be upgrade with `make install`. Instead, use the
`make upgrade` rule to upgrade all dependencies, followed by the
installation of the package.

## Project organization

We use the `dev` branch for development and the `main` branch for stable
releases, which currently follow a frequency of about 4 weeks. All
releases follow [semantic versioning](https://semver.org/), are
[tagged](https://github.com/NN-AI-Analytics/carts/tags) and notable
changes are reported in a
[changelog](https://github.com/NN-AI-Analytics/carts/blob/main/CHANGELOD.md).

## I Have a Question / I Want to Report a Bug

If you want to ask questions, require help or clarification, or report a
bug, we recommend to either contact a maintainer directly or the
following:

- Open an [Issue](https://github.com/NN-AI-Analytics/carts/issues).
- Provide as much context as you can about what you’re running into.
- Provide project and platform versions, depending on what seems
  relevant.

We will then take care of the issue as soon as possible.

## Maintainers

> Benedikt Sommer (<bkts@novonordisk.com>)  
> Klaus Kähler Holst (<kkzh@novonordisk.com>)
