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
#>             z     a
#>         <num> <num>
#> 1: -1.8833343     0
#> 2: -3.1048093     0
#> 3: -0.8916169     0
#> 4: -0.8435981     0
#> 5: -0.1165410     0
#> 
#> $`1`
#>             z     a
#>         <num> <num>
#> 1:  0.0140861     1
#> 2:  0.2444489     1
#> 3: -1.0366326     1
#> 4:  0.8809147     1
#> 5: -1.0918548     1
#> 
# adding randomized treatment indicator
xt <- function(n, ...) {
 covar_loggamma(n, normal.cor = 0.2) |>
   covar_add(list(a = rbinom(n, 1, 0.5), a = rbinom(n, 1, 0.5)))
}
xt(5)
#> $`0`
#>              z     a
#>          <num> <int>
#> 1:  0.03800383     0
#> 2:  0.70534369     0
#> 3:  0.32385338     0
#> 4: -2.06181198     1
#> 5: -2.30089130     0
#> 
#> $`1`
#>             z     a
#>         <num> <int>
#> 1:  0.3522481     1
#> 2: -0.2564330     1
#> 3:  0.4439519     1
#> 4: -2.8753023     1
#> 5:  0.4111807     1
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
#>             z         w1         w2         w3         w4          w5
#>         <num>      <num>      <num>      <num>      <num>       <num>
#> 1: -0.7749406 -0.3163322 -2.6452123 -0.6439059  0.3504924 -0.51490204
#> 2: -0.2549987 -0.8396228 -1.0324574  0.5870206  1.4337010  1.51974447
#> 3: -2.3589657 -1.3549281 -0.7074664 -0.1504031  0.7659068 -0.32849168
#> 4:  1.4947674 -0.8175683 -0.7005600 -1.7108218  1.1675207 -0.05367151
#> 5: -0.4476100 -0.6344000  0.5378854  1.4310326 -0.1369434 -0.56352463
#> 
#> $`1`
#>             z         w1         w2         w3         w4          w5
#>         <num>      <num>      <num>      <num>      <num>       <num>
#> 1: -1.6409284 -0.3163322 -2.6452123 -0.6439059  0.3504924 -0.51490204
#> 2: -1.0916523 -0.8396228 -1.0324574  0.5870206  1.4337010  1.51974447
#> 3: -2.5917357 -1.3549281 -0.7074664 -0.1504031  0.7659068 -0.32849168
#> 4: -1.5696407 -0.8175683 -0.7005600 -1.7108218  1.1675207 -0.05367151
#> 5: -0.2075977 -0.6344000  0.5378854  1.4310326 -0.1369434 -0.56352463
#> 
```
