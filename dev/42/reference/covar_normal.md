# Simulate from multivariate normal distribution

Simulate from MVN with compound symmetry variance structure and mean
zero. The result is returned as a list where the ith element is the
column vector with n observations from the ith coordinate of the MVN.

## Usage

``` r
covar_normal(
  n,
  normal.cor = NULL,
  normal.var = 1,
  names = c("z"),
  type = "cs",
  ...
)
```

## Arguments

- n:

  Number of samples

- normal.cor:

  Correlation parameter (n x r) or (1 x r) matrix

- normal.var:

  marginal variance (can be specified as a p-dim. vector or a nxp
  matrix)

- names:

  Column name of the column vector (default "z")

- type:

  of correlation matrix structure (cs: compound-symmetry / exchangable,
  ar: autoregressive, un: unstructured, to: toeplitz). The dimension of
  `normal.cor` must match, i.e., for a Toeplitz correlation matrix r =
  p-1, and for a cs and ar r=1.

- ...:

  Additional arguments passed to lower level functions

## Value

list of data.tables

## See also

[outcome_count](https://novonordisk-opensource.github.io/carts/reference/outcome_count.md)
[Trial](https://novonordisk-opensource.github.io/carts/reference/Trial.md)
[covar_loggamma](https://novonordisk-opensource.github.io/carts/reference/covar_loggamma.md)
