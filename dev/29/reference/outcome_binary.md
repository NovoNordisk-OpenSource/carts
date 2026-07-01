# Simulate from binary model given covariates

Simulate from binary model with probability \$\$\pi = g(\text{par}^\top
X)\$\$ where \\X\\ is the design matrix specified by the formula, and
\\g\\ is the link function specified by the family argument

## Usage

``` r
outcome_binary(
  data,
  mean = NULL,
  par = NULL,
  outcome.name = "y",
  remove = c("id", "num"),
  family = binomial(logit),
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

- remove:

  Variables that will be removed from input `data` (if formula is not
  specified).

- family:

  exponential family (default `binomial(logit)`)

- ...:

  Additional arguments passed to `mean` function (see examples)

## Value

data.table

## See also

[outcome_count](https://novonordisk-opensource.github.io/carts/reference/outcome_count.md)
[outcome_lp](https://novonordisk-opensource.github.io/carts/reference/outcome_lp.md)
[outcome_continuous](https://novonordisk-opensource.github.io/carts/reference/outcome_continuous.md)

## Examples

``` r
trial <- Trial$new(
  covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
  outcome = outcome_binary
)
est <- function(data) glm(y ~ a, data = data, family = binomial(logit))
trial$simulate(1e4, mean = ~ 1 + a, par = c(1, 0.5)) |> est()
#> 
#> Call:  glm(formula = y ~ a, family = binomial(logit), data = data)
#> 
#> Coefficients:
#> (Intercept)            a  
#>      1.0038       0.5592  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9998 Residual
#> Null Deviance:       10550 
#> Residual Deviance: 10420     AIC: 10430

# default behavior is to set all regression coefficients to 0
trial$simulate(1e4, mean = ~ 1 + a) |> est()
#> 
#> Call:  glm(formula = y ~ a, family = binomial(logit), data = data)
#> 
#> Coefficients:
#> (Intercept)            a  
#>   -0.003639    -0.007048  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9998 Residual
#> Null Deviance:       13860 
#> Residual Deviance: 13860     AIC: 13870

# intercept defaults to 0 and regression coef for a takes the provided value
trial$simulate(1e4, mean = ~ 1 + a, par = c(a = 0.5)) |> est()
#> 
#> Call:  glm(formula = y ~ a, family = binomial(logit), data = data)
#> 
#> Coefficients:
#> (Intercept)            a  
#>   -0.003622     0.522619  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9998 Residual
#> Null Deviance:       13700 
#> Residual Deviance: 13540     AIC: 13540
# trial$simulate(1e4, mean = ~ 1 + a, par = c("(Intercept)" = 1))

# define mean model that directly works on whole covariate data, incl id and
# num columns
trial$simulate(1e4, mean = \(x) with(x, lava::expit(1 + 0.5 * a))) |>
  est()
#> 
#> Call:  glm(formula = y ~ a, family = binomial(logit), data = data)
#> 
#> Coefficients:
#> (Intercept)            a  
#>      0.9806       0.4757  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9998 Residual
#> Null Deviance:       10830 
#> Residual Deviance: 10730     AIC: 10730

# par argument of outcome_binary is not passed on to mean function
trial$simulate(1e4,
  mean = \(x,  reg.par) with(x, lava::expit(reg.par[1] + reg.par[2] * a)),
  reg.par = c(1, 0.8)
) |> est()
#> 
#> Call:  glm(formula = y ~ a, family = binomial(logit), data = data)
#> 
#> Coefficients:
#> (Intercept)            a  
#>      1.0402       0.7501  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9998 Residual
#> Null Deviance:       10050 
#> Residual Deviance: 9831  AIC: 9835
```
