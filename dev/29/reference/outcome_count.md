# Simulate from count model given covariates

Simulate from count model with intensity \$\$\lambda =
\text{exposure-time}\exp(\text{par}^\top X)\$\$ where \\X\\ is the
design matrix specified by the formula

## Usage

``` r
outcome_count(
  data,
  mean = NULL,
  par = NULL,
  outcome.name = "y",
  exposure = 1,
  remove = c("id", "num"),
  zero.inflation = NULL,
  overdispersion = NULL,
  ...
)
```

## Arguments

- data:

  (data.table) Covariate data, usually the output of the covariate model
  of a
  [Trial](https://novonordisk-opensource.github.io/carts/reference/Trial.md)
  object.

- mean:

  (formula, function) Either a formula specifying the design from 'data'
  or a function that maps `data` to the conditional mean value on the
  link scale (see examples). If NULL all main-effects of the covariates
  will be used, except columns that are defined via the `remove`
  argument.

- par:

  (numeric) Regression coefficients (default zero). Can be given as a
  named list corresponding to the column names of `model.matrix`

- outcome.name:

  Name of outcome variable ("y")

- exposure:

  Exposure times. Either a scalar, vector or function.

- remove:

  Variables that will be removed from input `data` (if formula is not
  specified).

- zero.inflation:

  vector of probabilities or a function of the covariates 'x' including
  an extra column 'rate' with the rate parameter.

- overdispersion:

  variance of gamma-frailty either given as a numeric vector or a
  function of the covariates 'x' with an extra column 'rate' holding the
  rate parameter 'rate'

- ...:

  Additional arguments passed to `mean` and `exposure` function

## Value

data.table

## See also

[outcome_binary](https://novonordisk-opensource.github.io/carts/reference/outcome_binary.md)
[outcome_continuous](https://novonordisk-opensource.github.io/carts/reference/outcome_continuous.md)
[outcome_lp](https://novonordisk-opensource.github.io/carts/reference/outcome_lp.md)

## Examples

``` r
covariates <- function(n) data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n))
trial <- Trial$new(covariates = covariates, outcome = outcome_count)
trial$args_model(
  mean = ~ 1 + a + x,
  par = c(2.5, 0.65, 0),
  overdispersion = 1 / 2,
  exposure = 2 # identical exposure time for all subjects
)
est <- function(data) {
  glm(y ~ a + x + offset(log(exposure)), data, family = poisson())
}
trial$simulate(1e4) |> est()
#> 
#> Call:  glm(formula = y ~ a + x + offset(log(exposure)), family = poisson(), 
#>     data = data)
#> 
#> Coefficients:
#> (Intercept)            a            x  
#>    2.503024     0.657987     0.008428  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9997 Residual
#> Null Deviance:       212200 
#> Residual Deviance: 175400    AIC: 226100

# intercept + coef for x default to 0 and regression coef for a takes
# the provided value
trial$simulate(1e4, par = c(a = 0.65)) |> est()
#> 
#> Call:  glm(formula = y ~ a + x + offset(log(exposure)), family = poisson(), 
#>     data = data)
#> 
#> Coefficients:
#> (Intercept)            a            x  
#>    0.010257     0.647325    -0.005481  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9997 Residual
#> Null Deviance:       27160 
#> Residual Deviance: 24240     AIC: 48180
# trial$simulate(1e4, mean = ~ 1 + a, par = c("(Intercept)" = 1))

# define mean model that directly works on whole covariate data, incl id and
# num columns
trial$simulate(1e4, mean = \(x) with(x, exp(1 + 0.5 * a))) |>
  est()
#> 
#> Call:  glm(formula = y ~ a + x + offset(log(exposure)), family = poisson(), 
#>     data = data)
#> 
#> Coefficients:
#> (Intercept)            a            x  
#>    0.990683     0.507695    -0.006818  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9997 Residual
#> Null Deviance:       47790 
#> Residual Deviance: 43310     AIC: 77360

# treatment-dependent exposure times
trial$simulate(1e4, exposure = function(dd) 1 - 0.5 * dd$a) |>
  head()
#>       id     a           x   num     y exposure
#>    <num> <int>       <num> <num> <num>    <num>
#> 1:     1     1 -1.48217765     0    24      0.5
#> 2:     2     0 -1.18088521     0    14      1.0
#> 3:     3     0  0.08345518     0    14      1.0
#> 4:     4     0  1.60456977     0     6      1.0
#> 5:     5     1 -1.10279220     0    20      0.5
#> 6:     6     0 -0.29483347     0    13      1.0
```
