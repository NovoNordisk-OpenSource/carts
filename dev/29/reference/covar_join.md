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
#> 1 -1.5869430 -1.0575659
#> 2  0.6758453 -0.3149336
#> 3 -0.4758486  0.5017690
#> 4 -0.4963323  0.3992236
#> 5  0.1729910 -0.5015149

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
#>            a           b           c
#> 1  0.2489909  0.76828520  1.00512123
#> 2 -1.2416351 -1.70602622 -0.01778903
#> 3 -1.2054971  0.13690206  0.24343866
#> 4  0.4802692 -0.08016452  1.06749504
#> 5  0.5917036 -0.76433721  0.60605805

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
#> 1:  0.8347324     1
#> 2: -0.1030212     1
#> 3: -1.1768648     0
#> 4: -1.3352554     0
#> 5:  0.3196695     1
#> 
#> $`1`
#>             z     a
#>         <num> <int>
#> 1:  0.5220863     0
#> 2: -1.9232908     0
#> 3: -1.5993146     0
#> 4: -0.4414907     1
#> 5: -0.5364583     1
#> 

# constant covariate
x <- base %join% list(data.frame(a = 0), data.frame(a = 1))
x(n)
#> $`0`
#>             z     a
#>         <num> <num>
#> 1:  0.1596370     0
#> 2: -1.4714101     0
#> 3:  0.5108381     0
#> 4: -0.6095166     0
#> 5:  1.0387186     0
#> 
#> $`1`
#>             z     a
#>         <num> <num>
#> 1:  0.4515384     1
#> 2: -0.5865852     1
#> 3: -0.2889046     1
#> 4: -1.4698447     1
#> 5: -0.1878039     1
#> 

# baseline covariate
x <- base %join% function(n) data.frame(w = rnorm(n))
x(n)
#> $`0`
#>             z            w
#>         <num>        <num>
#> 1:  0.9074631  0.351596615
#> 2: -3.3398755  0.588027668
#> 3:  0.1499204 -1.922173391
#> 4: -1.5436171 -0.897130373
#> 5:  0.1862902  0.002374843
#> 
#> $`1`
#>              z            w
#>          <num>        <num>
#> 1:  0.71434564  0.351596615
#> 2:  0.04959620  0.588027668
#> 3:  0.01613242 -1.922173391
#> 4: -1.08179027 -0.897130373
#> 5:  0.46177323  0.002374843
#> 
```
