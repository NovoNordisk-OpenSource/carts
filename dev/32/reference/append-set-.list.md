# Assignment function to append values to existing list

Assignment function to append values to existing list

## Usage

``` r
# S3 method for class 'list'
append(x, name = NULL) <- value
```

## Arguments

- x:

  existing list

- name:

  optional name of new element

- value:

  new value to add to existing list

## Examples

``` r
x <- list()
append(x) <- 1
append(x, name = "a") <- 2
# duplicated names are allowed
append(x, name = "a") <- 3
x
#> [[1]]
#> [1] 1
#> 
#> $a
#> [1] 2
#> 
#> $a
#> [1] 3
#> 
```
