# Calculate linear predictor from covariates

Calculate linear predictor \$\$\text{par}^\top X\$\$ where \\X\\ is the
design matrix specified by the formula

## Usage

``` r
outcome_lp(
  data,
  mean = NULL,
  par = NULL,
  model = NULL,
  offset = NULL,
  treatment = NULL,
  intercept = TRUE,
  default.parameter = 0,
  family = gaussian(),
  remove = c("id", "num"),
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

- model:

  Optional model object ([glm](https://rdrr.io/r/stats/glm.html),
  [mets::phreg](http://kkholst.github.io/mets/reference/phreg.md), ...)

- offset:

  Optional offset variable name

- treatment:

  Optional name of treatment variable

- intercept:

  When FALSE the intercept will removed from the design matrix

- default.parameter:

  when `model` and `treatment` is specified, interaction terms between
  `treatment` and all other covariates in `model` is added to the
  simulation model. `default.parameter` specifies the default parameter
  of these extra parameters which can be changed individually with the
  `par` argument.

- family:

  family (default 'gaussian(identity)'). The inverse link-function is
  used to map the mean to the linear predictor scale (if mean is given
  as a function)

- remove:

  variables that will be removed from input data (if formula is not
  specified)

- ...:

  Additional arguments passed to `mean` function (see examples)

## Value

data.table

## See also

[outcome_count](https://novonordisk-opensource.github.io/carts/reference/outcome_count.md)
[outcome_binary](https://novonordisk-opensource.github.io/carts/reference/outcome_binary.md)
[outcome_continuous](https://novonordisk-opensource.github.io/carts/reference/outcome_continuous.md)
[outcome_phreg](https://novonordisk-opensource.github.io/carts/reference/outcome_phreg.md)
