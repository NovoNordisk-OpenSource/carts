# Construct estimator for the treatment effect in RCT based on covariate adjustment

Efficient estimator of the treatment effect based on the efficient
influence function. This involves a model for the conditional mean of
the outcome variable given covariates (Q-model).

## Usage

``` r
est_adj(
  response = "y",
  treatment = "a",
  covariates = NULL,
  offset = NULL,
  id = NULL,
  family = gaussian(),
  level = 0.95,
  treatment.effect = "absolute",
  nfolds = 1,
  ...
)
```

## Arguments

- response:

  (character, formula,
  [targeted::learner](https://kkholst.github.io/targeted/reference/learner.html))
  The default behavior when providing a character is to use a
  [glm](https://rdrr.io/r/stats/glm.html) with treatment-covariate
  interactions for the Q-model. The covariates are specified via the
  `covariates` argument, where the default behavior is to use no
  covariates. When providing a formula, a
  [glm](https://rdrr.io/r/stats/glm.html) is used for the Q-model, where
  the design matrix is specified by the formula. The last option is to
  provide a
  [targeted::learner](https://kkholst.github.io/targeted/reference/learner.html)
  object that specifies the Q-model (see examples).

- treatment:

  (character) Treatment variable. Additional care must be taken when the
  treatment variable is encoded as a factor (see examples).

- covariates:

  (character) List of covariates. Only applicable when `response` is a
  character.

- offset:

  (character) Optional offset to include in the
  [glm](https://rdrr.io/r/stats/glm.html) model when `response` is a
  character.

- id:

  (character) Subject id variable

- family:

  (family) Family argument used in the
  [glm](https://rdrr.io/r/stats/glm.html) when `response` is a character
  or formula.

- level:

  (numeric) Confidence interval level

- treatment.effect:

  (character, function) Default is the average treatment effect, i.e.
  difference in expected outcomes (x, y) -\> x - y, with x = E\[Y(1)\]
  and y = E\[Y(0)\]). Other options are "logrr" (x, y) -\> log(x / y) )
  and "logor" (x, y) -\> log(x / (1 - x) \* y / (1 - y)). A user-defined
  function can alternatively be provided to target a population
  parameter other than the absolute difference, log rate ratio or log
  odds ratio (see details).

- nfolds:

  (integer) Number of folds for estimating the conditional average
  treatment effect with double machine learning.

- ...:

  Additional arguments to
  [targeted::learner_glm](https://kkholst.github.io/targeted/reference/learner_glm.html)
  when `response` is a character or formula.

## Value

function

## Details

The user-defined function for `treatment.effect` needs to accept a
single argument `x` of estimates of (E\[Y(1)\],E\[Y(0)\]). The estimates
are a vector, where the order of E\[Y(1)\] and E\[Y(0)\] depends on the
encoding of the `treatment` variable. E\[Y(0)\] is the first element
when the treatment variable is drawn from a Bernoulli distribution and
kept as a numeric variable or corresponds to the first level when the
treatment variable is encoded as a factor.

## See also

[Trial](https://novonordisk-opensource.github.io/carts/reference/Trial.md)
[est_glm](https://novonordisk-opensource.github.io/carts/reference/est_glm.md)

## Author

Klaus Kähler Holst

## Examples

``` r
if (FALSE)  # don't run because of high computational time
trial <- Trial$new(
    covariates = function(n) data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n)),
    outcome = setargs(outcome_count,
      mean = ~ 1 + a*x,
      par = c(1, -0.1, 0.5, 0.2),
      overdispersion = 2)
)
dd <- trial$simulate(1e4)
#> Error: object 'trial' not found

# equivalent specifications to estimate log(E[Y(1)] / E[Y(0)])
estimators <- list(
  est_adj(family = poisson, treatment.effect = "logrr"),
  est_glm(family = poisson),
  est_adj(response = y ~ a, family = poisson, treatment.effect = "logrr"),
  est_adj(response = targeted::learner_glm(y ~ a, family = poisson),
    treatment.effect = "logrr"
  )
)
lapply(estimators, \(est) est(dd))
#> Error in FUN(X[[i]], ...): object 'dd' not found


# now with covariates, estimating E[Y(1)] - E[Y(0)]
estimators <- list(
  est_adj(covariates = "x", family = poisson),
  est_adj(response = y ~ a * x, family = poisson),
  est_adj(response = targeted::learner_glm(y ~ a * x, family = poisson))
)
lapply(estimators, \(est) est(dd))
#> Error in FUN(X[[i]], ...): object 'dd' not found

# custom treatment.effect function
estimator <- est_adj(response = y ~ a * x, family = poisson,
  treatment.effect = \(x) x[2] - x[1] # x[1] contains the estimate of E[Y(0)]
)
estimator(dd)
#> Error: object 'dd' not found

dd_factor <- dd
#> Error: object 'dd' not found
# when using factors, the control/comparator treatment needs to be the first
# level to estimate the contrasts defined by the `treatment.level` argument
estimator <- est_adj(response = y ~ a * x, family = poisson)
dd_factor$a <- factor(dd_factor$a, levels = c(0, 1))
#> Error: object 'dd_factor' not found
estimator(dd_factor) # E[Y(1)] - E[Y(0)]
#> Error: object 'dd_factor' not found

dd_factor$a <- factor(dd_factor$a, levels = c(1, 0))
#> Error: object 'dd_factor' not found
estimator(dd_factor) # E[Y(1)] - E[Y(0)]
#> Error: object 'dd_factor' not found
 # \dontrun{}
```
