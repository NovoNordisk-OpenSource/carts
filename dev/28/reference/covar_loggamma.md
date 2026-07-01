# Simulate from a log gamma-gaussian copula distribution

Simulate from the logarithmic transform of a Gaussian copula model with
compound symmetry correlation structure and with Gamma distributed
marginals with mean one.

## Usage

``` r
covar_loggamma(
  n,
  normal.cor = NULL,
  gamma.var = 1,
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

- gamma.var:

  Variance of gamma distribution (n x p or 1 x p matrix)

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

## Details

We simulate from the Gaussian copula by first drawing \\X\sim N(0,R)\\
and transform the margins with \\x\mapsto \log(F\_\nu^{-1}\\\Phi(x)\\)\\
where \\\Phi\\ is the standard normal CDF and \\F\_\nu^{-1}\\ is the
quantile function of the Gamma distribution with scale and rate
parameter equal to \\\nu\\.

## See also

[outcome_count](https://novonordisk-opensource.github.io/carts/reference/outcome_count.md)
[Trial](https://novonordisk-opensource.github.io/carts/reference/Trial.md)
[covar_normal](https://novonordisk-opensource.github.io/carts/reference/covar_normal.md)
