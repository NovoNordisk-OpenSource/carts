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
#> 1 -0.8437922 -0.3247586
#> 2 -0.0701223 -0.4507634
#> 3  0.3500692 -0.4491302
#> 4 -0.3465585 -0.8723956
#> 5 -0.5338262  2.2914497

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
#>             a           b         c
#> 1 -0.07372123 -0.04654242 -1.134252
#> 2 -0.70833494  0.47023920  1.555373
#> 3  2.57386464  1.07676514  1.299628
#> 4 -0.16731092  0.59211779 -1.770495
#> 5 -1.21207795  1.33633437 -1.914311

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
#> 1: -0.1458053     1
#> 2:  1.1224364     1
#> 3: -0.9210082     1
#> 4: -1.8092526     1
#> 5: -0.3101717     1
#> 
#> $`1`
#>             z     a
#>         <num> <int>
#> 1:  0.4270937     0
#> 2:  1.1440197     1
#> 3: -0.6747639     0
#> 4: -0.1704249     0
#> 5: -1.0701348     0
#> 

# constant covariate
x <- base %join% list(data.frame(a = 0), data.frame(a = 1))
x(n)
#> $`0`
#>               z     a
#>           <num> <num>
#> 1:  0.476688894     0
#> 2: -0.002701164     0
#> 3: -0.369159008     0
#> 4:  0.221762889     0
#> 5: -1.156739555     0
#> 
#> $`1`
#>             z     a
#>         <num> <num>
#> 1: -0.3524553     1
#> 2: -0.6021720     1
#> 3:  0.6591839     1
#> 4: -0.2299671     1
#> 5:  0.3291745     1
#> 

# baseline covariate
x <- base %join% function(n) data.frame(w = rnorm(n))
x(n)
#> $`0`
#>             z          w
#>         <num>      <num>
#> 1: -0.1049118 -2.1343549
#> 2: -0.6793058  0.6233803
#> 3: -1.9515172  0.2781125
#> 4: -3.1403259 -0.3549830
#> 5:  0.8139797  0.8301683
#> 
#> $`1`
#>             z          w
#>         <num>      <num>
#> 1: -1.3955962 -2.1343549
#> 2:  0.7340489  0.6233803
#> 3: -1.2564605  0.2781125
#> 4: -2.9664508 -0.3549830
#> 5:  0.7023209  0.8301683
#> 
```
