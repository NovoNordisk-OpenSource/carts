# Set default arguments of a function

Sets default values for arguments in `f`. Care should be taken when
`missing` is used in `f` (see examples).

## Usage

``` r
setargs(f, ..., setargs.warn = TRUE)
```

## Arguments

- f:

  function

- ...:

  arguments to set

- setargs.warn:

  cast warning when trying to set default values for arguments that do
  not exist in `f`

## Author

Benedikt Sommer

## Examples

``` r
foo <- function(x, a = 5, ...) {
  foo1 <- function(x, b = 5) return(b)
  c(a = a, b = foo1(x, ...), x = x)
}
foo(1)
#> a b x 
#> 5 5 1 

f <- setargs(foo, a = 10) # set new default value for a
f(1)
#>  a  b  x 
#> 10  5  1 

# default value of b in lower-level function is unaffected and warning is
# cast to inform that b is not an argument in f
f <- setargs(foo, b = 10)
#> Warning: Argument b not found in function f.
f(1)
#> a b x 
#> 5 5 1 
# disable warning message
setargs(foo, b = 10, setargs.warn = FALSE)(1)
#> a b x 
#> 5 5 1 

# arguments of lower-level functions can be set with setallargs
f <- setallargs(foo, a = 10, b = 10)
f(1)
#>  a  b  x 
#> 10 10  1 

# does not work when `missing` checks for missing formal arguments.
foo1 <- function(x, a) {
  if (missing(a)) a <- 5
  return(c(x = x, a = a))
}
f <- setargs(foo1, a = 10)
#> Warning: `missing` calls not working with `formals`
f(1)
#> x a 
#> 1 5 
```
