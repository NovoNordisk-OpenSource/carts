# Simulate from a negative binomial distribution

Parametrized by mean (rate) and variance. Both parameters can be vector
arguments. For this case with mean = variance = c(r1, r2) and n = 5, the
returned vector contains 5 Poisson samples. Three samples are drawn from
a Poisson distribution with rate r1 (index 1, 3 and 5 in output vector)
and two from a Poisson with rate r2 (index 2 and 4).

## Usage

``` r
rnb(n, mean, variance = mean, gamma.variance = NULL)
```

## Arguments

- n:

  Number of samples (integer)

- mean:

  Mean vector (rate parameter)

- variance:

  Variance vector

- gamma.variance:

  (optional) poisson-gamma mixture parametrization. Variance (vector) of
  gamma distribution with mean 1.

## Value

Vector of n realizations

## Examples

``` r
with(
  data.frame(x = rnb(1e4, mean = 100, var = 500)),
  c(mean = mean(x), var = var(x))
)
#> Warning: partial argument match of 'var' to 'variance'
#>     mean      var 
#> 100.6809 506.5187 
```
