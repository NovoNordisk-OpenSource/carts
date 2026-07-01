# Simulate from continuous outcome model given covariates

Simulate from continuous outcome model with mean \$\$g(\text{par}^\top
X)\$\$ where \\X\\ is the design matrix specified by the formula, and
\\g\\ is the link function specified by the family argument

## Usage

``` r
outcome_continuous(
  data,
  mean = NULL,
  par = NULL,
  sd = 1,
  het = 0,
  outcome.name = "y",
  remove = c("id", "num"),
  family = gaussian(),
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

- sd:

  (numeric) standard deviation of Gaussian measurement error

- het:

  Introduce variance hetereogeneity by adding a residual term \\het
  \cdot \mu_x \cdot e\\, where \\\mu_x\\ is the mean given covariates
  and \\e\\ is an independent standard normal distributed variable. This
  term is in addition to the measurement error introduced by the `sd`
  argument.

- outcome.name:

  Name of outcome variable ("y")

- remove:

  Variables that will be removed from input `data` (if formula is not
  specified).

- family:

  exponential family (default `gaussian(identity)`)

- ...:

  Additional arguments passed to `mean` function (see examples)

## Value

data.table

## See also

[outcome_count](https://novonordisk-opensource.github.io/carts/reference/outcome_count.md)
[outcome_binary](https://novonordisk-opensource.github.io/carts/reference/outcome_binary.md)
[outcome_lp](https://novonordisk-opensource.github.io/carts/reference/outcome_lp.md)

## Examples

``` r
trial <- Trial$new(
  covariates = \(n) data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n)),
  outcome = outcome_continuous
)
est <- function(data) glm(y ~ a + x, data = data)
trial$simulate(1e4, mean = ~ 1 + a + x, par = c(1, 0.5, 2)) |> est()
#> 
#> Call:  glm(formula = y ~ a + x, data = data)
#> 
#> Coefficients:
#> (Intercept)            a            x  
#>      0.9967       0.5366       2.0094  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9997 Residual
#> Null Deviance:       50790 
#> Residual Deviance: 10120     AIC: 28510

# default behavior is to set all regression coefficients to 0
trial$simulate(1e4, mean = ~ 1 + a + x) |> est()
#> 
#> Call:  glm(formula = y ~ a + x, data = data)
#> 
#> Coefficients:
#> (Intercept)            a            x  
#>    0.008401    -0.015159    -0.006731  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9997 Residual
#> Null Deviance:       9902 
#> Residual Deviance: 9901  AIC: 28290

# intercept defaults to 0 and regression coef for a takes the provided value
trial$simulate(1e4, mean = ~ 1 + a, par = c(a = 0.5)) |> est()
#> 
#> Call:  glm(formula = y ~ a + x, data = data)
#> 
#> Coefficients:
#> (Intercept)            a            x  
#>    -0.01457      0.50885     -0.02027  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9997 Residual
#> Null Deviance:       10610 
#> Residual Deviance: 9955  AIC: 28340
# trial$simulate(1e4, mean = ~ 1 + a, par = c("(Intercept)" = 0.5)) |> est()

# define mean model that directly works on whole covariate data, incl id and
# num columns
trial$simulate(1e4, mean = \(x) with(x, -1 + a * 2 + x * -3)) |>
  est()
#> 
#> Call:  glm(formula = y ~ a + x, data = data)
#> 
#> Coefficients:
#> (Intercept)            a            x  
#>     -0.9974       1.9876      -3.0092  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9997 Residual
#> Null Deviance:       110800 
#> Residual Deviance: 10030     AIC: 28420

# par argument is not passed on to mean function
trial$simulate(1e4,
  mean = \(x,  reg.par) with(x, reg.par[1] + reg.par[2] * a),
  reg.par = c(1, 5)
) |> est()
#> 
#> Call:  glm(formula = y ~ a + x, data = data)
#> 
#> Coefficients:
#> (Intercept)            a            x  
#>     0.99608      5.00195      0.01398  
#> 
#> Degrees of Freedom: 9999 Total (i.e. Null);  9997 Residual
#> Null Deviance:       72520 
#> Residual Deviance: 9979  AIC: 28370
```
