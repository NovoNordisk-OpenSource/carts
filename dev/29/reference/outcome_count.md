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
#>     2.50544      0.65458      0.01503  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9997 Residual
#> Null Deviance:       213000 
#> Residual Deviance: 176600    AIC: 227200

# intercept + coef for x default to 0 and regression coef for a takes
# the provided value
trial$simulate(1e4, par = c(a = 0.65)) |> est()
#> 
#> Call:  glm(formula = y ~ a + x + offset(log(exposure)), family = poisson(), 
#>     data = data)
#> 
#> Coefficients:
#> (Intercept)            a            x  
#>   0.0187912    0.6359110   -0.0007297  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9997 Residual
#> Null Deviance:       27150 
#> Residual Deviance: 24320     AIC: 48290
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
#>    0.983436     0.524986    -0.004498  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9997 Residual
#> Null Deviance:       49220 
#> Residual Deviance: 44420     AIC: 78420

# treatment-dependent exposure times
trial$simulate(1e4, exposure = function(dd) 1 - 0.5 * dd$a) |>
  head()
#>       id     a          x   num     y exposure
#>    <num> <int>      <num> <num> <num>    <num>
#> 1:     1     0 -0.9034504     0    20      1.0
#> 2:     2     1 -0.2522925     0     7      0.5
#> 3:     3     0 -0.6563022     0     2      1.0
#> 4:     4     0  1.0823261     0    16      1.0
#> 5:     5     0 -1.9236021     0     3      1.0
#> 6:     6     1 -0.1704307     0     2      0.5
```
