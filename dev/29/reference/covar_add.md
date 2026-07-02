# Add additional covariates to existing list of covariates

For use with
[Trial](https://novonordisk-opensource.github.io/carts/reference/Trial.md)
objects, this function makes it possible to easily add additional
covariates to an existing list of covariates (in the form of a
data.frame or data.table).

## Usage

``` r
covar_add(covars, x, names = NULL, ...)
```

## Arguments

- covars:

  list of covariates (data.frame's or data.table's)

- x:

  new covariates (function or list of functions/scalars)

- names:

  optional names of new covariates

- ...:

  additional arguments to function `x` or functions in `x`

## Value

matching format of covariates in `covars`

## Author

Klaus Kähler Holst

## Examples

``` r
# adding "fixed" treatment indicator in each period
n <- 5
xt <- function(n, ...) {
 covar_loggamma(n, normal.cor = 0.2) |>
   covar_add(list(a = 0, a = 1))
}
xt(n)
#> $`0`
#>              z     a
#>          <num> <num>
#> 1:  0.03062041     0
#> 2: -1.33846690     0
#> 3: -2.35014835     0
#> 4:  1.18265161     0
#> 5: -1.23432110     0
#> 
#> $`1`
#>             z     a
#>         <num> <num>
#> 1:  0.3802533     1
#> 2:  1.2393094     1
#> 3: -0.4607869     1
#> 4: -0.1367060     1
#> 5: -1.0617934     1
#> 
# adding randomized treatment indicator
xt <- function(n, ...) {
 covar_loggamma(n, normal.cor = 0.2) |>
   covar_add(list(a = rbinom(n, 1, 0.5), a = rbinom(n, 1, 0.5)))
}
xt(5)
#> $`0`
#>             z     a
#>         <num> <int>
#> 1: -2.1029542     0
#> 2: -1.8260280     0
#> 3:  0.4472138     0
#> 4: -0.7894082     0
#> 5:  0.4864255     1
#> 
#> $`1`
#>             z     a
#>         <num> <int>
#> 1: -0.5561899     1
#> 2: -2.1827479     1
#> 3:  0.6074767     1
#> 4: -2.5400333     1
#> 5: -0.3591942     0
#> 
# adding baseline covariates
xt <- function(n, ...) {
 covar_loggamma(n, normal.cor = 0.2) |>
   covar_add(rnorm(n), names = "w1") |> # data
   covar_add(list(w2 = rnorm(n))) |> # data
   covar_add(data.frame(w3 = rnorm(n))) |> # data
   covar_add(\(n) data.frame(w4 = rnorm(n))) |> # function
   covar_add(\(n) rnorm(n), names = "w5") # function
}
xt(5)
#> $`0`
#>             z         w1          w2         w3         w4          w5
#>         <num>      <num>       <num>      <num>      <num>       <num>
#> 1: -1.0703386 -1.0305073  0.18822825 -0.2192844 -0.6318909 -0.69553077
#> 2:  0.6200269  0.7704762  1.95150653 -0.8935691  0.0689740 -0.89125131
#> 3: -1.8731875 -1.2894581 -1.10794783 -0.3782867  0.4287800 -0.24231246
#> 4: -2.4693700  1.5771846 -0.04758160  0.4732930 -0.3322069  1.77474338
#> 5:  0.2167925 -0.1139907 -0.09504016  1.8267454 -0.1287370  0.05588728
#> 
#> $`1`
#>              z         w1          w2         w3         w4          w5
#>          <num>      <num>       <num>      <num>      <num>       <num>
#> 1: -3.68037032 -1.0305073  0.18822825 -0.2192844 -0.6318909 -0.69553077
#> 2: -0.26996234  0.7704762  1.95150653 -0.8935691  0.0689740 -0.89125131
#> 3:  0.21677558 -1.2894581 -1.10794783 -0.3782867  0.4287800 -0.24231246
#> 4:  0.02052801  1.5771846 -0.04758160  0.4732930 -0.3322069  1.77474338
#> 5: -3.42270762 -0.1139907 -0.09504016  1.8267454 -0.1287370  0.05588728
#> 
```
