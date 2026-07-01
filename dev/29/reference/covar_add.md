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
#> 1: -5.17288914     0
#> 2: -0.23373400     0
#> 3: -0.08233841     0
#> 4: -0.90611371     0
#> 5: -0.68288623     0
#> 
#> $`1`
#>             z     a
#>         <num> <num>
#> 1: -1.6169708     1
#> 2: -1.0005530     1
#> 3: -0.5023820     1
#> 4: -0.5755219     1
#> 5:  0.9159616     1
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
#> 1: -0.02970268     1
#> 2: -0.57207451     0
#> 3: -1.34815658     0
#> 4: -2.36085184     1
#> 5: -1.44558282     1
#> 
#> $`1`
#>             z     a
#>         <num> <int>
#> 1:  0.2360212     1
#> 2: -1.2211126     1
#> 3:  0.3353071     0
#> 4:  0.1842567     1
#> 5: -0.3917614     0
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
#>             z         w1         w2         w3          w4          w5
#>         <num>      <num>      <num>      <num>       <num>       <num>
#> 1:  0.2046948 -0.7578805  0.2196922 -0.6367395  0.20654095  0.08879881
#> 2: -0.8009428  0.1555087 -1.9430812 -1.0566792 -1.25708492 -0.33157817
#> 3: -1.8136618  0.8679761 -1.3355050 -0.4414375 -0.17152478  0.09385026
#> 4: -0.4692325 -1.1228447 -1.3627044  1.9745329  0.02171084  0.59624495
#> 5: -1.9679504  1.9299495 -0.7176322  0.9207201 -2.42886722  0.43675162
#> 
#> $`1`
#>             z         w1         w2         w3          w4          w5
#>         <num>      <num>      <num>      <num>       <num>       <num>
#> 1: -0.8063998 -0.7578805  0.2196922 -0.6367395  0.20654095  0.08879881
#> 2: -2.8375397  0.1555087 -1.9430812 -1.0566792 -1.25708492 -0.33157817
#> 3: -0.2435243  0.8679761 -1.3355050 -0.4414375 -0.17152478  0.09385026
#> 4:  0.5535224 -1.1228447 -1.3627044  1.9745329  0.02171084  0.59624495
#> 5:  0.5124240  1.9299495 -0.7176322  0.9207201 -2.42886722  0.43675162
#> 
```
