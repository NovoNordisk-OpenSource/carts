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
#>              [,1]         [,2]
#>  [1,]  0.70131184 -0.594627749
#>  [2,]  1.87082646 -1.824626445
#>  [3,]  0.24092149 -0.186389754
#>  [4,]  0.03149381  0.015497754
#>  [5,] -1.21461231  1.135646431
#>  [6,] -0.42736360 -0.381533101
#>  [7,]  0.14919043  0.141731109
#>  [8,] -0.03153515 -0.006252889
#>  [9,] -1.24925603 -1.163937354
#> [10,]  0.13764538  0.194171524
```
