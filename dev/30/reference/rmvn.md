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
#>             [,1]       [,2]
#>  [1,] -1.2378456  1.2214024
#>  [2,]  1.3611741 -1.4012403
#>  [3,] -0.7539647  0.7790928
#>  [4,] -0.5961257  0.5680358
#>  [5,] -0.5499398  0.5630490
#>  [6,] -1.4764842 -1.4738534
#>  [7,]  1.0009839  1.0721759
#>  [8,]  0.2278189  0.2057029
#>  [9,]  0.8637573  0.8554866
#> [10,] -0.3766165 -0.4369817
```
