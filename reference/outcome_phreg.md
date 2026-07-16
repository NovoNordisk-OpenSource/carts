# Outcome model for time-to-event end-points (proportional hazards)

Outcome model for time-to-event end-points (proportional hazards)

## Usage

``` r
outcome_phreg(
  data,
  lp = NULL,
  par = NULL,
  outcome.name = c("time", "status"),
  remove = c("id", "num"),
  model = NULL,
  cens.model = NULL,
  cens.lp = NULL,
  cens.par = NULL,
  ...
)
```

## Arguments

- data:

  (data.table) Covariate data, usually the output of the covariate model
  of a
  [Trial](https://novonordisk-opensource.github.io/carts/reference/Trial.md)
  object.

- lp:

  linear predictor (formula or function)

- par:

  optional list of model parameter

- outcome.name:

  names of outcome (time and censoring status)

- remove:

  variables that will be removed from input data (if formula is not
  specified)

- model:

  optional
  [mets::phreg](http://kkholst.github.io/mets/reference/phreg.md) object

- cens.model:

  optional model for censoring mechanism

- cens.lp:

  censoring linear predictor argument (formula or function)

- cens.par:

  list of censoring model parameters

- ...:

  Additional arguments to
  [outcome_lp](https://novonordisk-opensource.github.io/carts/reference/outcome_lp.md)

## Value

function (random generator)

## See also

[outcome_count](https://novonordisk-opensource.github.io/carts/reference/outcome_count.md)
[outcome_lp](https://novonordisk-opensource.github.io/carts/reference/outcome_lp.md)
[outcome_binary](https://novonordisk-opensource.github.io/carts/reference/outcome_binary.md)
[outcome_continuous](https://novonordisk-opensource.github.io/carts/reference/outcome_continuous.md)

## Author

Klaus Kähler Holst

## Examples

``` r
if (FALSE) { # \dontrun{
outcome_phreg <- carts:::outcome_phreg
library("survival")
data(pbc, package = "survival")
pbc0 <- na.omit(pbc) |>
  transform(trt = factor(trt, labels = c("Active", "Placebo")) |>
    relevel(ref = "Placebo"))

fit1 <- mets::phreg(Surv(time, status > 0) ~ age + sex * trt, data = pbc0)

covar <- covar_bootstrap(pbc0, subset = c("age", "sex")) %join%
  function(n, ...) data.frame(trt = sample(pbc0$trt, n, replace = TRUE))

outcome <- setargs(
  outcome_phreg,
  model = fit1,
  par = list("trtActive" = 0)
)

xx <- covar(5e3)
pbc1 <- outcome(xx) |> cbind(xx)
mets::phreg(formula(fit1), data = pbc1)


## Introducing additional interactions
fit2 <- mets::phreg(Surv(time, status > 0) ~ age + sex + trt, data = pbc0)

outcome <- setallargs(
  outcome_phreg,
  model = fit2,
  par = list("trtActive" = -.5, "age:trtActive" = 0),
  treatment = "trt",
  default.parameter = -0.2
)

xx <- covar(1e4)
attr(outcome(xx), "par")
pbc1 <- outcome(xx) |> cbind(xx)
mets::phreg(Surv(time, status) ~ (age + sex) * trt, pbc1)
rm(pbc1, xx)
} # }
```
