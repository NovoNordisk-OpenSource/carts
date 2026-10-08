# Add additional covariates to existing covariate random generator

For use with
[Trial](https://novonordisk-opensource.github.io/carts/reference/Trial.md)
objects, this function makes it possible to easily add additional
covariates to an existing random generator (function(n ...) returning a
data.frame or data.table)

## Usage

``` r
covar_join(f, ...)
```

## Arguments

- f:

  covariate random generator

- ...:

  additional covariate generators or constant covariates

## Value

function, with returned data type matching that of `f`

## Examples

``` r
# single period
n <- 5
c1 <- function(n) data.frame(a = rnorm(n))
c2 <- function(n) data.frame(b = rnorm(n))
x <- c1 %join% c2
x(n)
#>            a          b
#> 1 -0.7439090 -1.9165383
#> 2 -0.1090417  0.2360958
#> 3 -0.5608292  0.6289534
#> 4  0.1880015  0.4179257
#> 5  0.7488509  1.9767585

# adding covariates that remain constant when sampling
x <- c1 %join% data.frame(b = rnorm(n))
all.equal(x(n)$b, x(n)$b)
#> [1] TRUE

# adding multiple anonymous functions require parenthesis enclosing, with
# the exception of the last function
x <- c1 %join%
 (\(n) data.frame(b = rnorm(n))) %join%
 \(n) data.frame(c = rnorm(n))
x(n)
#>            a           b          c
#> 1 -1.4215347 -0.86434980  0.9480316
#> 2  1.1700562 -1.09147035 -0.1742460
#> 3 -1.4047145 -0.03705146 -1.1062360
#> 4  1.1017081  0.81005379 -0.9459850
#> 5  0.6979863 -0.49935541  0.2890896

# multiple periods
base <- setargs(covar_loggamma, normal.cor = .5)
x <- base %join%
  function(n) list(
      data.frame(a = rbinom(n, 1, 0.5)),
      data.frame(a = rbinom(n, 1, 0.5))
    )
x(n)
#> $`0`
#>             z     a
#>         <num> <int>
#> 1: -2.0656478     1
#> 2:  0.2567700     0
#> 3:  0.6673978     1
#> 4:  0.5587506     0
#> 5:  0.4513367     1
#> 
#> $`1`
#>             z     a
#>         <num> <int>
#> 1: -0.7814325     1
#> 2:  0.9159688     0
#> 3:  0.5268661     1
#> 4:  1.0317708     0
#> 5: -0.3181130     1
#> 

# constant covariate
x <- base %join% list(data.frame(a = 0), data.frame(a = 1))
x(n)
#> $`0`
#>              z     a
#>          <num> <num>
#> 1: -0.86779109     0
#> 2: -0.04871164     0
#> 3:  0.10909452     0
#> 4: -0.09242179     0
#> 5: -0.38622147     0
#> 
#> $`1`
#>             z     a
#>         <num> <num>
#> 1: -0.7310004     1
#> 2:  0.6189961     1
#> 3: -0.5622807     1
#> 4: -1.5261900     1
#> 5:  0.1792019     1
#> 

# baseline covariate
x <- base %join% function(n) data.frame(w = rnorm(n))
x(n)
#> $`0`
#>             z          w
#>         <num>      <num>
#> 1: -1.0982626  2.4808234
#> 2:  1.0124891 -2.1799567
#> 3: -1.2868438  0.4208746
#> 4: -0.9693053 -0.3575283
#> 5: -1.5783395 -0.6468615
#> 
#> $`1`
#>             z          w
#>         <num>      <num>
#> 1: -1.2874984  2.4808234
#> 2:  0.2514685 -2.1799567
#> 3: -1.8688456  0.4208746
#> 4: -0.7644110 -0.3575283
#> 5: -0.9668556 -0.6468615
#> 
```
