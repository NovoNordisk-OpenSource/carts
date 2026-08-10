# Sample from an estimated parametric covariate model

Sample from an estimated parametric covariate model

## Usage

``` r
sample_covar_parametric_model(n, model = NULL, model.path = NULL)
```

## Arguments

- n:

  Sample size

- model:

  lava::lvm object with estimated coefficients

- model.path:

  Path to dumped model object (RDS file) on disk (optional)

## Value

data.table

## Author

Benedikt Sommer

## Examples

``` r
data <- data.table::data.table(
  x = rnorm(1e3), y = as.factor(rbinom(1e3, size = 1, prob=0.5))
)

m <- estimate_covar_model_full_cond(data)
samples <- sample_covar_parametric_model(n=10, model = m)
print(head(samples))
#>            x y
#> 1 -0.7077978 0
#> 2  1.4489132 1
#> 3  0.3109190 0
#> 4 -0.4915126 0
#> 5 -1.7206634 0
#> 6  0.6717264 0
```
