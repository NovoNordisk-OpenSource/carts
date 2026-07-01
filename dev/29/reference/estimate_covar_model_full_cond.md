# Full conditional covariate simulation model

Estimates a full conditional model to approximate the joint distribution
of covariate data. Each factor \\p(x_i \| x_1, \dots x\_{i-1})\\ is
modelled with a `glm`, with mean \\E\[x_i \| x_1, \dots x\_{i-1}\] =
g^{-1}(\beta_0 + \sum\_{j=1}^{i-1}\beta_j x_j)\\. The parametric
distribution of each factor is either derived from the column type (see
[`derive_covar_distribution`](https://novonordisk-opensource.github.io/carts/reference/derive_covar_distribution.md))
or specified by `cond.dist`.

## Usage

``` r
estimate_covar_model_full_cond(data, cond.dist = NULL)
```

## Arguments

- data:

  Covariate `data.table`

- cond.dist:

  `list` with random generator functions for the conditional
  distribution of each covariate

## Value

lava::lvm object with estimated coefficients

## Author

Benedikt Sommer

## Examples

``` r
data <- data.table::data.table(
y = as.factor(rbinom(1e3, size = 1, prob=0.1))
)

# infer distribution of y from column type
m.est <- estimate_covar_model_full_cond(data)
y <- sample_covar_parametric_model(1e4, m.est)$y |> as.integer() - 1
print(mean(y))
#> [1] 0.1065

# specify distribution of y
m.est <- estimate_covar_model_full_cond(
  data, cond.dist = list(y = binomial.lvm)
)
y <- sample_covar_parametric_model(1e4, m.est)$y |> as.integer() - 1
print(mean(y))
#> [1] 0.1015
```
