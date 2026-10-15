# EXPERIMENTAL: Outcome model for recurrent events with terminal events end-points

This function is still in an experimental state where the interface and
functionality might change in the future

## Usage

``` r
outcome_recurrent(
  data,
  lp = NULL,
  par = NULL,
  outcome.name = c("time", "status"),
  remove = c("id", "num"),
  model = NULL,
  death.model = NULL,
  death.lp = NULL,
  death.par = NULL,
  cens.model = NULL,
  cens.lp = NULL,
  cens.par = NULL,
  ...
)
```

## Arguments

- data:

  data.frame (covariates)

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

- death.model:

  optional model for death (terminal) events

- death.lp:

  optional death linear predictor argument (formula or function)

- death.par:

  optional list of death model parameters

- cens.model:

  optional model for censoring mechanism

- cens.lp:

  optional censoring linear predictor argument (formula or function)

- cens.par:

  optional list of censoring model parameters

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
[outcome_phreg](https://novonordisk-opensource.github.io/carts/reference/outcome_phreg.md)
