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

  formula specifying design from 'data' or a function that maps x to the
  mean value. If NULL all main-effects of the covariates will be used

- par:

  (numeric) Regression coefficients (default zero). Can be given as a
  named list corresponding to the column names of `model.matrix`

- outcome.name:

  Name of outcome variable ("y")

- remove:

  variables that will be removed from input data (if formula is not
  specified)

- family:

  exponential family (default `binomial(logit)`)

- ...:

  Additional arguments passed to `mean` function (see examples)

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
#>      0.9742       0.5561  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9998 Residual
#> Null Deviance:       10670 
#> Residual Deviance: 10540     AIC: 10540

# default behavior is to set all regression coefficients to 0
trial$simulate(1e4, mean = ~ 1 + a) |> est()
#> 
#> Call:  glm(formula = y ~ a, family = binomial(logit), data = data)
#> 
#> Coefficients:
#> (Intercept)            a  
#>    -0.04696      0.07982  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9998 Residual
#> Null Deviance:       13860 
#> Residual Deviance: 13860     AIC: 13860

# intercept defaults to 0 and regression coef for a takes the provided value
trial$simulate(1e4, mean = ~ 1 + a, par = c(a = 0.5)) |> est()
#> 
#> Call:  glm(formula = y ~ a, family = binomial(logit), data = data)
#> 
#> Coefficients:
#> (Intercept)            a  
#>    0.007602     0.449294  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9998 Residual
#> Null Deviance:       13730 
#> Residual Deviance: 13610     AIC: 13610
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
#>      0.9841       0.5388  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9998 Residual
#> Null Deviance:       10670 
#> Residual Deviance: 10550     AIC: 10550

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
#>      1.0033       0.8103  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9998 Residual
#> Null Deviance:       10120 
#> Residual Deviance: 9859  AIC: 9863
```
