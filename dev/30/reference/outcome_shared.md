# Outcome model

Outcome model

## Arguments

- data:

  (data.table) Covariate data, usually the output of the covariate model
  of a
  [Trial](https://novonordisk-opensource.github.io/carts/reference/Trial.md)
  object.

- par:

  (numeric) Regression coefficients (default zero). Can be given as a
  named list corresponding to the column names of `model.matrix`

- outcome.name:

  Name of outcome variable ("y")

- remove:

  Variables that will be removed from input `data` (if formula is not
  specified).

- mean:

  (formula, function) Either a formula specifying the design from 'data'
  or a function that maps `data` to the conditional mean value on the
  link scale (see examples). If NULL all main-effects of the covariates
  will be used, except columns that are defined via the `remove`
  argument.

- ...:

  Additional arguments passed to `mean` function (see examples)

## Value

data.table
