# carts: Simulation-Based Assessment of Covariate Adjustment in Randomized Trials

Monte Carlo simulation framework for different randomized clinical trial
designs with a special emphasis on estimators based on covariate
adjustment.

Monte Carlo simulation framework for different randomized clinical trial
designs with a special emphasis on estimators based on covariate
adjustment.

## See also

Useful links:

- <https://novonordisk-opensource.github.io/carts/>

- <https://github.com/NovoNordisk-OpenSource/carts>

- Report bugs at
  <https://github.com/NovoNordisk-OpenSource/carts/issues>

## Author

**Maintainer**: Benedikt Sommer <benediktsommer92@gmail.com>

Authors:

- Benedikt Sommer <benediktsommer92@gmail.com>

- Klaus K. Holst <klaus@holst.it>

- Foroogh Shamsi <foroogh.shamsi1@gmail.com>

Benedikt Sommer, Klaus Holst, Foroogh Shamsi

## Examples

``` r
if (FALSE)  # don't run because of high computational time
trial <- Trial$new(
  covariates = \(n) data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n)),
  outcome = setargs(outcome_count, par = c(1, 0.5, 1), overdispersion = 0.7)
)

trial$estimators(
  unadjusted = est_glm(family = "poisson"),
  adjusted = est_glm(family = "poisson", covariates = "x")
)
#> Error: object 'trial' not found

trial$run(n = 200, R = 100)
#> Error: object 'trial' not found
trial$summary()
#> Error: object 'trial' not found
 # \dontrun{}
```
