# trial.estimates class object

[Trial\$run()](https://novonordisk-opensource.github.io/carts/reference/Trial.md)
returns an S3 class object `trial.estimates`. The object contains all
information to reproduce the estimates as shown in the example. The
object is a list with the following components:

- model:

  [Trial](https://novonordisk-opensource.github.io/carts/reference/Trial.md)
  object used to generate the estimates.

- estimates:

  (list) Estimates of Monte-Carlo runs for each of the estimators.

- sample.data:

  (data.table) Sample data returned from
  [Trial\$simulate()](https://novonordisk-opensource.github.io/carts/reference/Trial.md).

- estimators:

  (list) Estimators applied to simulated data in each Monte-Carlo run.

- sim.args:

  (list) Arguments passed on to
  [Trial\$simulate()](https://novonordisk-opensource.github.io/carts/reference/Trial.md)
  when simulating data in each Monte-Carlo run.

- R:

  (numeric) Number of Monte-Carlo replications.

## S3 generics

The following S3 generic functions are available for an object of class
`trial.estimates`:

- `print`:

  Basic print method.

- `summary`:

  Apply decision-making to estimates of each run and estimator.

## Examples

``` r
trial <- Trial$new(
  covariates = function(n) data.frame(a = rbinom(n, 1, 0.5)),
  outcome = function(data) rnorm(nrow(data), data$a * -1)
 )
res <- trial$run(n = 100, R = 10, estimators = est_glm())
print(res)
#> ── trial.estimates ── 
#> 
#> Model arguments: 
#> Estimators: est1
#> Simulation parameters: n = 100, R = 10
#> 
#> Sample data:
#>       id     a   num          y
#>    <num> <int> <num>      <num>
#> 1:     1     1     0 -1.0113119
#> 2:     2     0     0  1.0584097
#> 3:     3     0     0 -0.7170027
#> 4:     4     1     0 -0.7097310
#> 5:     5     0     0  0.4030567
#> 6:     6     0     0 -0.7028861

# assuming previous estimates have been saved to disk.
# load estimates object and repeat simulation with more Monte-Carlo runs
res2 <- do.call(
  res$model$run,
  c(list(R = 20, estimators = res$estimators), res$sim.args)
)
print(res2)
#> ── trial.estimates ── 
#> 
#> Model arguments: 
#> Estimators: est1
#> Simulation parameters: n = 100, R = 20
#> 
#> Sample data:
#>       id     a   num          y
#>    <num> <int> <num>      <num>
#> 1:     1     1     0  0.1290326
#> 2:     2     1     0 -0.8260042
#> 3:     3     0     0  0.4271669
#> 4:     4     1     0 -1.8398480
#> 5:     5     0     0 -0.2946902
#> 6:     6     1     0 -2.5813019
```
