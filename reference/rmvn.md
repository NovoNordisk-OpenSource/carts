# Multivariate normal distribution function

Draw random samples from multivariate normal distribution with variance
given by a correlation matrix.

## Usage

``` r
rmvn(n, mean, cor, var = NULL)
```

## Arguments

- n:

  number of samples

- mean:

  matrix with mean values (either a 1xp or nxp matrix)

- cor:

  matrix with correlation (either a 1x((p-1)\*p/2) or nx((p-1)\*p/2)
  matrix. The correlation coefficients must be given in the order
  R(1,2), R(1,3), ..., R(1,p), R(2,3), ... R(2,p), ... where R(i,j) is
  the entry in row i and column j of the correlation matrix.

- var:

  Optional covariance matrix (instead of 'cor' argument)

## Examples

``` r
rmvn(10, cor = rep(c(-0.999, 0.999), each = 5))
#>              [,1]        [,2]
#>  [1,]  0.50168484 -0.48090225
#>  [2,] -0.01713045 -0.05586825
#>  [3,] -1.36865560  1.29345745
#>  [4,]  0.86647975 -0.70593371
#>  [5,]  0.40364408 -0.49304993
#>  [6,] -1.31027390 -1.27702669
#>  [7,] -0.16385178 -0.18277040
#>  [8,]  0.39502264  0.37591215
#>  [9,] -0.50946392 -0.51972147
#> [10,] -0.70808148 -0.67948034
```
