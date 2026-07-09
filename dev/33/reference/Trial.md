# R6 class for power and sample-size calculations for a clinical trial

Simulation of RCT with flexible covariates distributions simulation.

## Author

Klaus Kähler Holst, Benedikt Sommer

Benedikt Sommer

Klaus Kähler Holst

## Public fields

- `info`:

  Optional information/name of the model

- `covariates`:

  covariate generator (function of sample-size n and optional
  parameters)

- `outcome_model`:

  Generator for outcome given covariates (function of covariates x in
  long format)

- `exclusion`:

  function defining exclusion criterion

- `estimates`:

  ([trial.estimates](https://novonordisk-opensource.github.io/carts/reference/trial.estimates-class.md))
  Parameter estimates of Monte-Carlo simulations returned by
  Trial\$run(). The field is flushed, i.e. set to its default value
  *NULL*, when model arguments (Trial\$args_model()) or estimators
  (Trial\$estimators()) are modified.

## Methods

### Public methods

- [`Trial$new()`](#method-Trial-initialize)

- [`Trial$args_model()`](#method-Trial-args_model)

- [`Trial$args_summary()`](#method-Trial-args_summary)

- [`Trial$estimators()`](#method-Trial-estimators)

- [`Trial$simulate()`](#method-Trial-simulate)

- [`Trial$run()`](#method-Trial-run)

- [`Trial$estimate_power()`](#method-Trial-estimate_power)

- [`Trial$estimate_samplesize()`](#method-Trial-estimate_samplesize)

- [`Trial$summary()`](#method-Trial-summary)

- [`Trial$print()`](#method-Trial-print)

- [`Trial$clone()`](#method-Trial-clone)

------------------------------------------------------------------------

### `Trial$new()`

Initialize new Trial object

#### Usage

    Trial$new(
      outcome,
      covariates = NULL,
      exclusion = identity,
      estimators = list(),
      summary.args = list(),
      info = NULL
    )

#### Arguments

- `outcome`:

  outcome model given covariates (the first positional argument must be
  the covariate data)

- `covariates`:

  covariate simulation function (must have 'n' as first named argument
  and returns either a list of data.table (data.frame) for each
  observation period or a single data.table (data.frame) in case of a
  single observation period)

- `exclusion`:

  function describing selection criterion (the first positional argument
  must be the combined covariate and outcome data and the function must
  return the subjects who are included in the trial)

- `estimators`:

  optional list of estimators or single estimator function

- `summary.args`:

  list of arguments that override default values in Trial\$summary()
  when power and sample sizes are estimated with Trial\$estimate_power()
  and Trial\$estimate_samplesize()

- `info`:

  optional string describing the model

------------------------------------------------------------------------

### `Trial$args_model()`

Get, specify or update parameters of covariate, outcome and exclusion
model. Parameters are set in a named list, and updated when parameter
names match with existing values in the list.

#### Usage

    Trial$args_model(.args = NULL, .reset = FALSE, ...)

#### Arguments

- `.args`:

  (list or character) named list of arguments to update or set. A single
  or subset of arguments can be retrieved by passing the respective
  argument names as a character or character vector.

- `.reset`:

  (logical or character) Reset all or a subset of previously set
  parameters. Can be combined with setting new parameters.

- `...`:

  Alternative to using `.args` to update or set arguments

#### Examples

    trial <- Trial$new(
      covariates = function(n, p = 0.5) data.frame(a = rbinom(n, 1, p)),
      outcome = function(data, ate, mu) rnorm(nrow(data), mu + data$a * ate)
    )

    # set and update parameters
    trial$args_model(.args = list(ate = 2, p = 0.5, mu = 3))
    trial$args_model(ate = 5, p = 0.6) # update parameters
    trial$args_model(list(ate = 2), p = 0.5) # combine first arg with ...

    # retrieve parameters
    trial$args_model() # return all set parameters
    trial$args_model("ate") # select a single parameter
    trial$args_model(c("ate", "mu")) # multiple parameters

    # remove parameters
    trial$args_model(.reset = "ate") # remove a single parameter
    trial$args_model(.reset = TRUE) # remove all parameters

    # remove and set/update parameters
    trial$args_model(ate = 2, p = 0.5, .reset = TRUE)
    trial$args_model(ate = 5, .reset = "p") # removing p and updating ate

------------------------------------------------------------------------

### `Trial$args_summary()`

Get, specify or update the summary.args attribute.

#### Usage

    Trial$args_summary(.args = NULL, .reset = FALSE, ...)

#### Arguments

- `.args`:

  (list or character) named list of arguments to update or set. A single
  or subset of arguments can be retrieved by passing the respective
  argument names as a character or character vector.

- `.reset`:

  (logical or character) Reset all or a subset of previously set
  parameters. Can be combined with setting new parameters.

- `...`:

  Alternative to using `.args` to update or set arguments

#### Examples

    trial <- Trial$new(
      covariates = function(n, p = 0.5) data.frame(a = rbinom(n, 1, p)),
      outcome = function(data, ate, mu) rnorm(nrow(data), mu + data$a * ate)
    )
    # set and update parameters
    trial$args_summary(list(level = 0.05, alternative = "<"))
    trial$args_summary(level = 0.25) # update parameters

    # retrieve parameters
    trial$args_summary() # return all set parameters
    trial$args_summary("level") # select a single parameter
    trial$args_summary(c("level", "alternative")) # multiple parameters

    # remove parameters
    trial$args_summary(.reset = "level") # remove a single parameter
    trial$args_summary(.reset = TRUE) # remove all parameters

    # remove and set/update parameters
    trial$args_summary(alternative = "!=", level = 0.05, .reset = TRUE)
    # removing alternative and setting level
    trial$args_summary(level = 0.05, .reset = "alternative")

------------------------------------------------------------------------

### `Trial$estimators()`

Get, specify or update estimators.

#### Usage

    Trial$estimators(.estimators = NULL, .reset = FALSE, ...)

#### Arguments

- `.estimators`:

  (list, function or character) Argument controlling the getter or
  setter behavior. Estimators are set or updated by providing a single
  estimator (function) or list of estimators, and retrieved by providing
  a character (see examples).

- `.reset`:

  (logical or character) Reset all or a subset of previously set
  estimators. Can be combined with setting new estimators.

- `...`:

  Alternative to `.estimators` for updating or setting estimators.

#### Details

A name is internally assigned to estimators when calling the method with
`.estimators` set to a single estimator or a list with unnamed elements.
The names are a combination of an *est* prefix and an integer that
indicates the *n*th added unnamed estimator.

#### Examples

    estimators <- list(marginal = est_glm(), adj = est_glm(covariates = "x"))
    trial <- Trial$new(
      covariates = \(n) data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n)),
      outcome = \(data, ate = -0.5) rnorm(nrow(data), data$a * ate),
      estimators = estimators
    )

    # get estimators
    trial$estimators() |> names() # list all estimators
    trial$estimators("marginal") |> names() # select a single estimator
    trial$estimators(c("marginal", "adj")) |> names() # select mult. est.

    # remove estimators
    trial$estimators(.reset = "marginal") # remove a single estimator
    trial$estimators(.reset = TRUE) # remove all estimators

    # set or update estimators
    trial$estimators(estimators)
    trial$estimators(adj2 = est_adj(covariates = "x")) # add new estimator
    # update adj and remove adj2
    trial$estimators(adj = est_glm(covariates = "x"), .reset = "adj2")

    # unnamed estimators (adding default name)
    estimators <- list(est_glm(), est_glm(covariates = "x"))
    trial$estimators(estimators, .reset = TRUE)
    trial$estimators(.reset = "est1")
    trial$estimators(est_glm()) # replaces removed est1

------------------------------------------------------------------------

### `Trial$simulate()`

Simulate data by applying parameters to the trial model. The method
samples first from the covariate model. Outcome data sampling follows by
passing the simulated covariate data to the outcome model. An optional
exclusion model is applied to the combined covariates and outcomes data.
The sampling process is repeated in case any subjects are removed by the
exclusion model until a total of `n` subjects are sampled or the maximum
iteration number `.niter` is reached.

The method adds two auxiliary columns to the simulated data to identify
distinct patients (*id*) and periods (*num*) in case of time-dependent
covariate and outcome models. The columns are added to the sampled
covariate data before sampling the outcomes. A *data.table* with both
columns is provided to the outcome model in case no covariate model is
defined. Thus, the outcome model is always applied to at least a
*data.table* with an *id* and *num* column. The default column name *y*
is used for the outcome variable in the returned *data.table* when the
defined outcome model returns a vector. The name is easily changed by
returning a *data.table* with a named column (see examples).

The optional argument `...` of this method can be used to provide
parameters to the trial model as an addition to parameters that have
already been defined via Trial\$args_model(). Data is simulated from the
union of parameters, where parameters provided via the optional argument
of this method take precedence over parameters defined via
Trial\$args_model(). However, parameters that have been set via
Trial\$args_model() are not updated when optional arguments are
provided.

#### Usage

    Trial$simulate(n, .niter = 500L, ...)

#### Arguments

- `n`:

  (integer) Number of observations (sample size)

- `.niter`:

  (integer) Maximum number of simulation runs to avoid infinite loops
  for ill defined exclusion functions.

- `...`:

  Arguments to covariate, outcome and exclusion model functions

#### Returns

data.table with `n` rows

#### Examples

    trial <- Trial$new(
      covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
      outcome = \(data, ate = 0) with(data, rnorm(nrow(data), a * ate))
    )

    # applying and modifying parameters
    n <- 10
    trial$simulate(n) # use parameters set during initialization
    trial$args_model(ate = -100) # update parameters
    trial$simulate(n)
    trial$simulate(n, ate = 100) # change ate via optional argument

    # rename outcome variable
    trial <- Trial$new(
      covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
      outcome = \(data, ate = 0) {
        data.frame(yy = with(data, rnorm(nrow(data), a * ate)))
      }
    )
    trial$simulate(n)

    # return multiple outcome variables
    trial <- Trial$new(
      covariates = \(n) data.frame(a = rbinom(n, 1, 0.5), y.base = rnorm(n)),
      outcome = \(data, ate = 0) {
        y  <-  with(data, rnorm(nrow(data), a * ate))
        return(data.frame(y = y, y.chg = data$y.base - y))
      }
    )
    trial$simulate(n)

    # use exclusion argument to post-process sampled outcome variable to
    # achieve the same as in the above example but without modifying the
    # originally defined outcome model
    trial <- Trial$new(
      covariates = \(n) data.frame(a = rbinom(n, 1, 0.5), y.base = rnorm(n)),
      outcome = \(data, ate = 0) with(data, rnorm(nrow(data), a * ate)),
      exclusion = \(data) {
       cbind(data, data.frame(y.chg = data$y.base - data$y))
      }
    )
    trial$simulate(n)

    # no covariate model
    trial <- Trial$new(
      outcome = \(data, ate = 0) {
        n <- nrow(data)
        a <- rbinom(n, 1, 0.5)
        return(data.frame(a = a, y = rnorm(n, a * ate)))
      }
    )
    trial$simulate(n)

------------------------------------------------------------------------

### `Trial$run()`

Run trial and estimate parameters multiple times

The method calls Trial\$simulate() `R` times and applies the specified
estimators to each simulated dataset of sample size `n`. Parameters to
the covariates, outcome and exclusion models can be provided as optional
arguments to this method call in addition to parameters that have
already been defined via Trial\$args_model(). The behavior is identical
to Trial\$simulate(), except that `.niter` can be provided as an
optional argument to this method for controlling the maximum number of
simulation runs in Trial\$simulate().

Estimators fail silently in that errors occurring when applying an
estimator to each simulated dataset will not terminate the method call.
The returned
[trial.estimates](https://novonordisk-opensource.github.io/carts/reference/trial.estimates-class.md)
object will instead indicate that estimators failed.

#### Usage

    Trial$run(n, R = 100, estimators = NULL, ...)

#### Arguments

- `n`:

  (integer) Number of observations (sample size)

- `R`:

  (integer) Number of replications

- `estimators`:

  (list or function) List of estimators or a single unnamed estimator

- `...`:

  Arguments to covariate, outcome and exclusion model functions

#### Returns

(invisible) An object of class
[trial.estimates](https://novonordisk-opensource.github.io/carts/reference/trial.estimates-class.md),
which contains the estimates of all estimators and all information to
repeat the simulation. The return object is also assigned to the
`estimates` field of this Trial class object (see examples).

#### Examples

     # don't run because of high computational time
    # future::plan("multicore")
    trial <- Trial$new(
      covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
      outcome = \(data, ate = 0) with(data, rnorm(nrow(data), a * ate)),
      estimators = list(glm = est_glm())
    )
    trial$args_summary(alternative = "<")

    # the returned trial.estimates object contains the estimates for each of
    # the R simulated data sets + all necessary information to re-run the
    # simulation
    res <- trial$run(n = 100, R = 50) # store return object in a new variable
    print(trial$estimates) # trial$estimates == res

    # the basic usage is to apply the summary method to the generated
    # trial.estimates object.
    trial$summary()

    # combining Trial$run and summary is faster than using
    # Trial$estimate_power when modifying only the parameters of the
    # decision-making function
    sapply(
      c(0, 0.25, 0.5),
      \(ni) trial$summary(ni.margin = ni)[, "power"]
    )

    # changing the ate parameter value
    trial$run(n = 100, R = 50, ate = -0.2)

    # supplying another estimator
    trial$run(n = 100, R = 50, estimators = est_glm(robust = FALSE))

------------------------------------------------------------------------

### `Trial$estimate_power()`

Estimates statistical power for a specified trial

Convenience method that first runs Trial\$run() and subsequently applies
Trial\$summary() to derive the power for each estimator. The behavior of
passing arguments to lower level functions is identical to Trial\$run().

#### Usage

    Trial$estimate_power(n, R = 100, estimators = NULL, summary.args = list(), ...)

#### Arguments

- `n`:

  (integer) Number of observations (sample size)

- `R`:

  (integer) Number of replications

- `estimators`:

  (list or function) List of estimators or a single unnamed estimator

- `summary.args`:

  (list) Arguments passed to summary method for decision-making

- `...`:

  Arguments to covariate, outcome and exclusion model functions

#### Returns

numeric

#### Examples

     # don't run because of high computational time
    # toy examples with small number of Monte-Carlo replicates
    # future::plan("multicore")
    trial <- Trial$new(
      covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
      outcome = \(data, ate = 0) with(data, rnorm(nrow(data), a * ate)),
      estimators = list(glm = est_glm())
    )
    trial$args_summary(alternative = "<")

    # using previously defined estimators and summary.args
    trial$estimate_power(n = 100, R = 20)

    # supplying parameters to outcome function
    trial$estimate_power(n = 100, R = 20, ate = -100)

    # modifying arguments of summary function
    trial$estimate_power(n = 100, R = 20, ate = -100,
     summary.args = list(alternative = ">")
    )

    # supplying estimators to overrule previously set estimators
    trial$estimate_power(n = 100, R = 20,
     estimators = list(est_glm(), est_adj()))

------------------------------------------------------------------------

### `Trial$estimate_samplesize()`

Estimate the minimum sample-size required to reach a desired statistical
power with a specified estimator. An initial rough estimate is obtained
via bisection, followed by a stochastic approximation (Robbins-Monro)
algorithm, and finally, a grid search (refinement step) in the
neighbourhood of the current best solution.

Note that the estimation procedure for the sample size will not populate
the estimates attribute of a trial object.

#### Usage

    Trial$estimate_samplesize(
      ...,
      power = 0.9,
      estimator = NULL,
      interval = c(50L, 10000L),
      bisection.control = list(R = 100, niter = 6),
      sa.control = list(R = 1, niter = 250, stepmult = 100, alpha = 0.5),
      refinement = seq(-10, 10, by = 5),
      R = 1000,
      interpolate = TRUE,
      verbose = TRUE,
      minimum = 10L,
      summary.args = list()
    )

#### Arguments

- `...`:

  Arguments to covariate, outcome and exclusion model functions

- `power`:

  (numeric) Desired statistical power

- `estimator`:

  (list or function) Estimator (function) to be applied. If NULL, then
  estimate sample size for all estimators defined via
  Trial\$estimators(). A prefix *est* is used to label unnamed
  estimators.

- `interval`:

  (integer vector) Interval in which to initially look for a solution
  with the bisection algorithm. Passing an integer will skip the
  bisection algorithm and use the provided integer as the initial
  solution for the stochastic approximation algorithm

- `bisection.control`:

  (list) Options controlling the bisection algorithm
  ([bisection](https://novonordisk-opensource.github.io/carts/reference/bisection.md)).
  Default values can also be changed for a subset of options only (see
  examples).

- `sa.control`:

  (list) Options controlling the stochastic approximation
  (Robbins-Monro) algorithm
  ([optim_sa](https://novonordisk-opensource.github.io/carts/reference/optim_sa.md)).
  Default values can also be changed for a subset of options only (see
  examples).

- `refinement`:

  (integer vector) Vector to create an interval whose center is the
  sample size estimate of the Robbins-Monro algorithm.

- `R`:

  (integer) Number of replications to use in Monte Carlo simulation of
  refinement calculations.

- `interpolate`:

  (logical) If TRUE, a linear interpolation of the refinement points
  will be used to estimate the power.

- `verbose`:

  (logical) If TRUE, additional output will be displayed.

- `minimum`:

  (integer) Minimum sample size.

- `summary.args`:

  (list) Arguments passed to summary method for decision-making

#### Returns

samplesize_estimate S3 object

#### Examples

     # don't run because of high computational time
    trial <- Trial$new(
      covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
      outcome = \(data, ate, sd) with(data, rnorm(nrow(data), a * ate, sd)),
      estimators = list(marginal = est_glm())
    )
    trial$args_model(ate = -1, sd = 5)
    trial$args_summary(alternative = "<")

    # supply model parameter and estimator to call to overwrite previously
    # set values
    trial$estimate_samplesize(ate = -2, estimator = est_glm())

    # reduce number of iterations for bisection step but keep R = 100
    # (default value)
    # trial$estimate_samplesize(bisection.control = list(niter = 2))

    # reduce significance level from 0.05 to 0.025, but keep alternative as
    # before
    # trial$estimate_samplesize(summary.args = list(level = 0.025))

------------------------------------------------------------------------

### `Trial$summary()`

Summarize Monte Carlo studies of different estimators for the treatment
effect in a randomized clinical trial. The method reports the power of
both superiority tests (one-sided or two-sided) and non-inferiority
tests, together with summary statistics of the different estimators.

#### Usage

    Trial$summary(
      level = 0.05,
      null = 0,
      ni.margin = NULL,
      alternative = "!=",
      reject.function = NULL,
      true.value = NULL,
      nominal.coverage = 0.9,
      estimates = NULL,
      ...
    )

#### Arguments

- `level`:

  (numeric) significance level

- `null`:

  (numeric) null hypothesis to test

- `ni.margin`:

  (numeric) non-inferiority margin

- `alternative`:

  alternative hypothesis (not equal !=, less \<, greater \>)

- `reject.function`:

  Optional function calculating whether to reject the null hypothesis

- `true.value`:

  Optional true parameter value

- `nominal.coverage`:

  Width of confidence limits

- `estimates`:

  Optional trial.estimates object. When provided, these estimates will
  be used instead of the object's stored estimates. This allows
  calculating summaries for different trial results without modifying
  the object's state.

- `...`:

  additional arguments to lower level functions

#### Returns

matrix with results of each estimator stored in separate rows

#### Examples

    outcome <- function(data, p = c(0.5, 0.25)) {
      a <- rbinom(nrow(data), 1, 0.5)
      data.frame(a = a, y = rbinom(nrow(data), 1, p[1] * (1 - a) + p[2] * a)
      )
    }
    trial <- Trial$new(outcome, estimators = est_glm())
    trial$run(n = 100, R = 100)
    # two-sided test with 0.05 significance level (alpha = 0.05) (default
    # values)
    trial$summary(level = 0.05, alternative = "!=")
    # on-sided test
    trial$summary(level = 0.025, alternative = "<")
    # non-inferiority test
    trial$summary(level = 0.025, ni.margin = -0.5)

    # provide simulation results to summary method via estimates argument
    res <- trial$run(n = 100, R = 100, p = c(0.5, 0.5))
    trial$summary(estimates = res)

    # calculate empirical bias, rmse and coverage for true target parameter
    trial$summary(estimates = res, true.value = 0)

------------------------------------------------------------------------

### `Trial$print()`

Print method for Trial objects

#### Usage

    Trial$print(..., verbose = FALSE)

#### Arguments

- `...`:

  Additional arguments to lower level functions (not used).

- `verbose`:

  (logical) By default, only print the function arguments of the
  covariates, outcome and exclusion models. If *TRUE*, then also print
  the function body.

#### Examples

    trial <- Trial$new(
      covariates = function(n) data.frame(a = rbinom(n, 1, 0.5)),
      outcome = function(data, sd = 1) rnorm(nrow(data), data$a * -1, sd),
      estimators = list(marginal = est_glm()),
      info = "Some trial info"
    )
    trial$args_model(sd = 2)
    trial$args_summary(level = 0.025)

    print(trial) # only function headers
    print(trial, verbose = TRUE) # also print function bodies

------------------------------------------------------------------------

### `Trial$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Trial$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

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

## ------------------------------------------------
## Method `Trial$args_model()`
## ------------------------------------------------

trial <- Trial$new(
  covariates = function(n, p = 0.5) data.frame(a = rbinom(n, 1, p)),
  outcome = function(data, ate, mu) rnorm(nrow(data), mu + data$a * ate)
)

# set and update parameters
trial$args_model(.args = list(ate = 2, p = 0.5, mu = 3))
trial$args_model(ate = 5, p = 0.6) # update parameters
trial$args_model(list(ate = 2), p = 0.5) # combine first arg with ...

# retrieve parameters
trial$args_model() # return all set parameters
#> $ate
#> [1] 2
#> 
#> $p
#> [1] 0.5
#> 
#> $mu
#> [1] 3
#> 
trial$args_model("ate") # select a single parameter
#> [1] 2
trial$args_model(c("ate", "mu")) # multiple parameters
#> $ate
#> [1] 2
#> 
#> $mu
#> [1] 3
#> 

# remove parameters
trial$args_model(.reset = "ate") # remove a single parameter
trial$args_model(.reset = TRUE) # remove all parameters

# remove and set/update parameters
trial$args_model(ate = 2, p = 0.5, .reset = TRUE)
trial$args_model(ate = 5, .reset = "p") # removing p and updating ate

## ------------------------------------------------
## Method `Trial$args_summary()`
## ------------------------------------------------

trial <- Trial$new(
  covariates = function(n, p = 0.5) data.frame(a = rbinom(n, 1, p)),
  outcome = function(data, ate, mu) rnorm(nrow(data), mu + data$a * ate)
)
# set and update parameters
trial$args_summary(list(level = 0.05, alternative = "<"))
trial$args_summary(level = 0.25) # update parameters

# retrieve parameters
trial$args_summary() # return all set parameters
#> $level
#> [1] 0.25
#> 
#> $null
#> [1] 0
#> 
#> $ni.margin
#> NULL
#> 
#> $alternative
#> [1] "<"
#> 
#> $reject.function
#> NULL
#> 
#> $true.value
#> NULL
#> 
#> $nominal.coverage
#> [1] 0.9
#> 
trial$args_summary("level") # select a single parameter
#> [1] 0.25
trial$args_summary(c("level", "alternative")) # multiple parameters
#> $level
#> [1] 0.25
#> 
#> $alternative
#> [1] "<"
#> 

# remove parameters
trial$args_summary(.reset = "level") # remove a single parameter
trial$args_summary(.reset = TRUE) # remove all parameters

# remove and set/update parameters
trial$args_summary(alternative = "!=", level = 0.05, .reset = TRUE)
# removing alternative and setting level
trial$args_summary(level = 0.05, .reset = "alternative")

## ------------------------------------------------
## Method `Trial$estimators()`
## ------------------------------------------------

estimators <- list(marginal = est_glm(), adj = est_glm(covariates = "x"))
trial <- Trial$new(
  covariates = \(n) data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n)),
  outcome = \(data, ate = -0.5) rnorm(nrow(data), data$a * ate),
  estimators = estimators
)

# get estimators
trial$estimators() |> names() # list all estimators
#> [1] "marginal" "adj"     
trial$estimators("marginal") |> names() # select a single estimator
#> NULL
trial$estimators(c("marginal", "adj")) |> names() # select mult. est.
#> [1] "marginal" "adj"     

# remove estimators
trial$estimators(.reset = "marginal") # remove a single estimator
trial$estimators(.reset = TRUE) # remove all estimators

# set or update estimators
trial$estimators(estimators)
trial$estimators(adj2 = est_adj(covariates = "x")) # add new estimator
# update adj and remove adj2
trial$estimators(adj = est_glm(covariates = "x"), .reset = "adj2")

# unnamed estimators (adding default name)
estimators <- list(est_glm(), est_glm(covariates = "x"))
trial$estimators(estimators, .reset = TRUE)
trial$estimators(.reset = "est1")
trial$estimators(est_glm()) # replaces removed est1

## ------------------------------------------------
## Method `Trial$simulate()`
## ------------------------------------------------

trial <- Trial$new(
  covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
  outcome = \(data, ate = 0) with(data, rnorm(nrow(data), a * ate))
)

# applying and modifying parameters
n <- 10
trial$simulate(n) # use parameters set during initialization
#>        id     a   num          y
#>     <num> <int> <num>      <num>
#>  1:     1     0     0  1.1484116
#>  2:     2     1     0 -1.8218177
#>  3:     3     1     0 -0.2473253
#>  4:     4     0     0 -0.2441996
#>  5:     5     0     0 -0.2827054
#>  6:     6     0     0 -0.5536994
#>  7:     7     0     0  0.6289820
#>  8:     8     0     0  2.0650249
#>  9:     9     1     0 -1.6309894
#> 10:    10     1     0  0.5124269
trial$args_model(ate = -100) # update parameters
trial$simulate(n)
#>        id     a   num            y
#>     <num> <int> <num>        <num>
#>  1:     1     0     0    0.4681544
#>  2:     2     0     0    0.3629513
#>  3:     3     0     0   -1.3045435
#>  4:     4     1     0  -99.2622237
#>  5:     5     0     0    1.8885049
#>  6:     6     0     0   -0.0974451
#>  7:     7     1     0 -100.9358474
#>  8:     8     1     0 -100.0159503
#>  9:     9     0     0   -0.8267890
#> 10:    10     0     0   -1.5123997
trial$simulate(n, ate = 100) # change ate via optional argument
#>        id     a   num            y
#>     <num> <int> <num>        <num>
#>  1:     1     1     0  99.86600299
#>  2:     2     0     0  -1.91008747
#>  3:     3     1     0  99.72076276
#>  4:     4     0     0  -0.31344598
#>  5:     5     1     0 101.06730788
#>  6:     6     0     0   0.07003485
#>  7:     7     1     0  99.36087668
#>  8:     8     1     0  99.95003510
#>  9:     9     1     0  99.74851656
#> 10:    10     0     0   0.44479712

# rename outcome variable
trial <- Trial$new(
  covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
  outcome = \(data, ate = 0) {
    data.frame(yy = with(data, rnorm(nrow(data), a * ate)))
  }
)
trial$simulate(n)
#>        id     a   num          yy
#>     <num> <int> <num>       <num>
#>  1:     1     1     0  0.86208648
#>  2:     2     0     0 -0.24323674
#>  3:     3     1     0 -0.20608719
#>  4:     4     1     0  0.01917759
#>  5:     5     1     0  0.02956075
#>  6:     6     0     0  0.54982754
#>  7:     7     1     0 -2.27411486
#>  8:     8     1     0  2.68255718
#>  9:     9     0     0 -0.36122126
#> 10:    10     0     0  0.21335575

# return multiple outcome variables
trial <- Trial$new(
  covariates = \(n) data.frame(a = rbinom(n, 1, 0.5), y.base = rnorm(n)),
  outcome = \(data, ate = 0) {
    y  <-  with(data, rnorm(nrow(data), a * ate))
    return(data.frame(y = y, y.chg = data$y.base - y))
  }
)
trial$simulate(n)
#>        id     a     y.base   num           y      y.chg
#>     <num> <int>      <num> <num>       <num>      <num>
#>  1:     1     1 -0.9758506     0  0.52390979 -1.4997604
#>  2:     2     1  1.0650573     0  0.60674805  0.4583093
#>  3:     3     0  0.1316706     0 -0.10993567  0.2416063
#>  4:     4     1  0.4886288     0  0.17218172  0.3164471
#>  5:     5     1 -1.6994506     0 -0.09032729 -1.6091233
#>  6:     6     0 -1.4707363     0  1.92434334 -3.3950796
#>  7:     7     0  0.2841503     0  1.29839276 -1.0142424
#>  8:     8     1  1.3373204     0  0.74879127  0.5885291
#>  9:     9     0  0.2366963     0  0.55622433 -0.3195280
#> 10:    10     0  1.3182934     0 -0.54825726  1.8665506

# use exclusion argument to post-process sampled outcome variable to
# achieve the same as in the above example but without modifying the
# originally defined outcome model
trial <- Trial$new(
  covariates = \(n) data.frame(a = rbinom(n, 1, 0.5), y.base = rnorm(n)),
  outcome = \(data, ate = 0) with(data, rnorm(nrow(data), a * ate)),
  exclusion = \(data) {
   cbind(data, data.frame(y.chg = data$y.base - data$y))
  }
)
trial$simulate(n)
#>        id     a      y.base   num          y      y.chg
#>     <num> <int>       <num> <num>      <num>      <num>
#>  1:     1     1  0.42418757     0 -0.3872136  0.8114011
#>  2:     2     0  1.06310200     0 -0.7854327  1.8485347
#>  3:     3     0  1.04871262     0 -1.0567369  2.1054495
#>  4:     4     1 -0.03810289     0 -0.7955414  0.7574385
#>  5:     5     0  0.48614892     0 -1.7562754  2.2424243
#>  6:     6     1  1.67288261     0 -0.6905379  2.3634205
#>  7:     7     1 -0.35436116     0 -0.5585420  0.2041808
#>  8:     8     0  0.94634789     0 -0.5366633  1.4830112
#>  9:     9     0  1.31682636     0  0.2271271  1.0896992
#> 10:    10     1 -0.29664002     0  0.9784549 -1.2750949

# no covariate model
trial <- Trial$new(
  outcome = \(data, ate = 0) {
    n <- nrow(data)
    a <- rbinom(n, 1, 0.5)
    return(data.frame(a = a, y = rnorm(n, a * ate)))
  }
)
trial$simulate(n)
#>        id     a          y
#>     <num> <int>      <num>
#>  1:     1     0  2.1268505
#>  2:     2     0  0.4248584
#>  3:     3     0 -1.6842815
#>  4:     4     1  0.2494018
#>  5:     5     1  1.0728383
#>  6:     6     1  2.0393693
#>  7:     7     0  0.4494538
#>  8:     8     1  1.3918140
#>  9:     9     1  0.4265665
#> 10:    10     1  0.1075840

## ------------------------------------------------
## Method `Trial$run()`
## ------------------------------------------------

if (FALSE)  # don't run because of high computational time
# future::plan("multicore")
trial <- Trial$new(
  covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
  outcome = \(data, ate = 0) with(data, rnorm(nrow(data), a * ate)),
  estimators = list(glm = est_glm())
)
trial$args_summary(alternative = "<")

# the returned trial.estimates object contains the estimates for each of
# the R simulated data sets + all necessary information to re-run the
# simulation
res <- trial$run(n = 100, R = 50) # store return object in a new variable
#> Error in names(estimators) <- paste0("est", seq_along(estimators)): 'names' attribute [1] must be the same length as the vector [0]
print(trial$estimates) # trial$estimates == res
#> NULL

# the basic usage is to apply the summary method to the generated
# trial.estimates object.
trial$summary()
#> Error in trial_summary(self = self, level = level, null = null, ni.margin = ni.margin,     alternative = alternative, reject.function = reject.function,     true.value = true.value, nominal.coverage = nominal.coverage,     estimates = estimates, ...): No estimates available. Run trial first.

# combining Trial$run and summary is faster than using
# Trial$estimate_power when modifying only the parameters of the
# decision-making function
sapply(
  c(0, 0.25, 0.5),
  \(ni) trial$summary(ni.margin = ni)[, "power"]
)
#> Error in trial_summary(self = self, level = level, null = null, ni.margin = ni.margin,     alternative = alternative, reject.function = reject.function,     true.value = true.value, nominal.coverage = nominal.coverage,     estimates = estimates, ...): No estimates available. Run trial first.

# changing the ate parameter value
trial$run(n = 100, R = 50, ate = -0.2)
#> Error in names(estimators) <- paste0("est", seq_along(estimators)): 'names' attribute [1] must be the same length as the vector [0]

# supplying another estimator
trial$run(n = 100, R = 50, estimators = est_glm(robust = FALSE))
 # \dontrun{}

## ------------------------------------------------
## Method `Trial$estimate_power()`
## ------------------------------------------------

if (FALSE)  # don't run because of high computational time
# toy examples with small number of Monte-Carlo replicates
# future::plan("multicore")
trial <- Trial$new(
  covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
  outcome = \(data, ate = 0) with(data, rnorm(nrow(data), a * ate)),
  estimators = list(glm = est_glm())
)
trial$args_summary(alternative = "<")

# using previously defined estimators and summary.args
trial$estimate_power(n = 100, R = 20)
#> Error in names(estimators) <- paste0("est", seq_along(estimators)): 'names' attribute [1] must be the same length as the vector [0]

# supplying parameters to outcome function
trial$estimate_power(n = 100, R = 20, ate = -100)
#> Error in names(estimators) <- paste0("est", seq_along(estimators)): 'names' attribute [1] must be the same length as the vector [0]

# modifying arguments of summary function
trial$estimate_power(n = 100, R = 20, ate = -100,
 summary.args = list(alternative = ">")
)
#> Error in names(estimators) <- paste0("est", seq_along(estimators)): 'names' attribute [1] must be the same length as the vector [0]

# supplying estimators to overrule previously set estimators
trial$estimate_power(n = 100, R = 20,
 estimators = list(est_glm(), est_adj()))
#> est1 est2 
#> 0.05 0.05 
 # \dontrun{}

## ------------------------------------------------
## Method `Trial$estimate_samplesize()`
## ------------------------------------------------

if (FALSE)  # don't run because of high computational time
trial <- Trial$new(
  covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
  outcome = \(data, ate, sd) with(data, rnorm(nrow(data), a * ate, sd)),
  estimators = list(marginal = est_glm())
)
trial$args_model(ate = -1, sd = 5)
trial$args_summary(alternative = "<")

# supply model parameter and estimator to call to overwrite previously
# set values
trial$estimate_samplesize(ate = -2, estimator = est_glm())
#> INFO [2026-07-09 10:54:27] Finding initial sample-size with bisection algorithm
#> INFO [2026-07-09 10:54:27] Evaluating left point: 50
#> INFO [2026-07-09 10:54:29] Evaluating right point: 10000
#> INFO [2026-07-09 10:54:34] Running stochastic approximation algorithm
#> INFO [2026-07-09 10:55:35] Refining estimate and calculating power
#> Warning: Over-parameterized model (NA parameters). Ignoring NA parameters
#> Warning: Over-parameterized model (NA parameters). Ignoring NA parameters
#> INFO [2026-07-09 10:55:50] [1/4] - power(10) = 0.941
#> Warning: Over-parameterized model (NA parameters). Ignoring NA parameters
#> INFO [2026-07-09 10:56:03] [2/4] - power(11) = 0.935
#> INFO [2026-07-09 10:56:17] [3/4] - power(16) = 0.994
#> INFO [2026-07-09 10:56:31] [4/4] - power(21) = 0.998
#> INFO [2026-07-09 10:56:31] Estimated sample size: 10
#> ── Estimated sample-size to reach 90% power ── 
#> 
#> n = 10 (actual estimated power≈93.94%)

# reduce number of iterations for bisection step but keep R = 100
# (default value)
# trial$estimate_samplesize(bisection.control = list(niter = 2))

# reduce significance level from 0.05 to 0.025, but keep alternative as
# before
# trial$estimate_samplesize(summary.args = list(level = 0.025))
 # \dontrun{}

## ------------------------------------------------
## Method `Trial$summary()`
## ------------------------------------------------

outcome <- function(data, p = c(0.5, 0.25)) {
  a <- rbinom(nrow(data), 1, 0.5)
  data.frame(a = a, y = rbinom(nrow(data), 1, p[1] * (1 - a) + p[2] * a)
  )
}
trial <- Trial$new(outcome, estimators = est_glm())
trial$run(n = 100, R = 100)
# two-sided test with 0.05 significance level (alpha = 0.05) (default
# values)
trial$summary(level = 0.05, alternative = "!=")
#>        estimate    std.err    std.dev power na
#> est1 -0.2586042 0.09297792 0.08489204  0.83  0
# on-sided test
trial$summary(level = 0.025, alternative = "<")
#>        estimate    std.err    std.dev power na
#> est1 -0.2586042 0.09297792 0.08489204  0.83  0
# non-inferiority test
trial$summary(level = 0.025, ni.margin = -0.5)
#>        estimate    std.err    std.dev power na
#> est1 -0.2586042 0.09297792 0.08489204   0.7  0

# provide simulation results to summary method via estimates argument
res <- trial$run(n = 100, R = 100, p = c(0.5, 0.5))
trial$summary(estimates = res)
#>        estimate    std.err   std.dev power na
#> est1 0.01201245 0.09932767 0.1047944  0.07  0

# calculate empirical bias, rmse and coverage for true target parameter
trial$summary(estimates = res, true.value = 0)
#>        estimate    std.err   std.dev power na       bias      rmse coverage
#> est1 0.01201245 0.09932767 0.1047944  0.07  0 0.01201245 0.1049588     0.85

## ------------------------------------------------
## Method `Trial$print()`
## ------------------------------------------------

trial <- Trial$new(
  covariates = function(n) data.frame(a = rbinom(n, 1, 0.5)),
  outcome = function(data, sd = 1) rnorm(nrow(data), data$a * -1, sd),
  estimators = list(marginal = est_glm()),
  info = "Some trial info"
)
trial$args_model(sd = 2)
trial$args_summary(level = 0.025)

print(trial) # only function headers
#> ── Trial Object ── 
#> Some trial info 
#> 
#> Model arguments:  
#>  • sd:  num 2
#> 
#> Summary arguments:  
#>  • level:  num 0.025
#>  • null:  num 0
#>  • ni.margin:  NULL
#>  • alternative:  chr "!="
#>  • reject.function:  NULL
#>  • true.value:  NULL
#>  • nominal.coverage:  num 0.9
#> 
#> Covariates:
#>  function (n, ...)   
#> 
#> Outcome:
#>  function (data, sd = 1, ...)   
#> 
#> Exclusion:
#>  function (x, ...)   
#> 
#> Estimators:
#>   1. marginal 
#> ────────────────────
print(trial, verbose = TRUE) # also print function bodies
#> ── Trial Object ── 
#> Some trial info 
#> 
#> Model arguments:  
#>  • sd:  num 2
#> 
#> Summary arguments:  
#>  • level:  num 0.025
#>  • null:  num 0
#>  • ni.margin:  NULL
#>  • alternative:  chr "!="
#>  • reject.function:  NULL
#>  • true.value:  NULL
#>  • nominal.coverage:  num 0.9
#> 
#> Covariates:
#>   function (n, ...)  
#>     data.frame(a = rbinom(n, 1, 0.5)) 
#> 
#> Outcome:
#>   function (data, sd = 1, ...)  
#>     rnorm(nrow(data), data$a * -1, sd) 
#> 
#> Exclusion:
#>   function (x, ...)  
#>     x 
#> 
#> Estimators:
#>   1. marginal 
#> ────────────────────
```
