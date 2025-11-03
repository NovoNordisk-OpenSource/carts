#' @title R6 class for power and sample-size calculations for a clinical trial
#' @description Simulation of RCT with flexible covariates distributions
#'  simulation.
#' @param ... Arguments to covariate, outcome and exclusion model
#' functions
#' @param R (integer) Number of replications
#' @param n (integer) Number of observations (sample size)
#' @param estimators (list or function) List of estimators or a single unnamed
#' estimator
#' @param summary.args (list) Arguments passed to summary method
#' for decision-making
#' @author Klaus Kähler Holst, Benedikt Sommer
#' @examples
#' \dontrun{
#' trial <- Trial$new(
#'   covariates = \(n) data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n)),
#'   outcome = setargs(outcome_count, par = c(1, 0.5, 1), overdispersion = 0.7)
#' )
#'
#' trial$estimators(
#'   unadjusted = est_glm(family = "poisson"),
#'   adjusted = est_glm(family = "poisson", covariates = "x")
#' )
#'
#' trial$run(n = 200, R = 100)
#' trial$summary()
#' }
#' @export
Trial <- R6::R6Class("Trial", #nolint
  public = list(
    #' @field info Optional information/name of the model
    info = NULL,
    #' @field covariates covariate generator
    #' (function of sample-size n and optional parameters)
    covariates = NULL,
    #' @field outcome_model Generator for outcome given covariates
    #' (function of covariates x in long format)
    outcome_model = NULL,
    #' @field exclusion function defining exclusion criterion
    exclusion = NULL,
    #' @field estimates ([trial.estimates-class]) Parameter estimates of
    #' Monte-Carlo simulations returned by [Trial$run()][Trial]. The field is
    #' flushed, i.e. set to its default value *NULL*, when model arguments
    #' ([Trial$args_model()][Trial]) or estimators ([Trial$estimators()][Trial])
    #' are modified.
    estimates = NULL,

    #' @description Initialize new Trial object
    #' @param covariates covariate simulation function (must have 'n' as first
    #' named argument and returns either a list of data.table (data.frame) for
    #' each observation period or a single data.table (data.frame) in case of a
    #' single observation period)
    #' @param outcome outcome model given covariates
    #' (the first positional argument must be the covariate data)
    #' @param exclusion function describing selection criterion (the first
    #' positional argument must be the combined covariate and outcome data and
    #' the function must return the subjects who are included in the trial)
    #' @param estimators optional list of estimators or single estimator
    #' function
    #' @param summary.args list of arguments that override default values in
    #' [Trial$summary()][Trial] when power and sample sizes are
    #' estimated with [Trial$estimate_power()][Trial] and
    #' [Trial$estimate_samplesize()][Trial]
    #' @param info optional string describing the model
    initialize = function(outcome,
                          covariates = NULL,
                          exclusion = identity,
                          estimators = list(),
                          summary.args = list(),
                          info = NULL
                          ) {
      self$covariates <- add_dots(covariates)
      if (!is.null(exclusion)) {
        self$exclusion <- add_dots(exclusion)
      }
      self$outcome_model <- add_dots(outcome)
      self$estimators(estimators)
      self$info <- as.list(info)

      args <- as.list(formals(self$summary))
      args[c("...", "estimates")] <- NULL
      private$summary.args_default <- args

      args[names(summary.args)] <- summary.args
      self$args_summary(args)

      return(invisible(self))

    },
    #' @description Get, specify or update parameters of covariate, outcome and
    #' exclusion model. Parameters are set in a named list, and updated when
    #' parameter names match with existing values in the list.
    #' @param .args (list or character) named list of arguments to update
    #' or set. A single or subset of arguments can be retrieved by passing the
    #' respective argument names as a character or character vector.
    #' @param .reset (logical or character) Reset all or a subset of previously
    #' set parameters. Can be combined with setting new parameters.
    #' @param ... Alternative to using `.args` to update or set arguments
    #' @examples
    #' trial <- Trial$new(
    #'   covariates = function(n, p = 0.5) data.frame(a = rbinom(n, 1, p)),
    #'   outcome = function(data, ate, mu) rnorm(nrow(data), mu + data$a * ate)
    #' )
    #'
    #' # set and update parameters
    #' trial$args_model(.args = list(ate = 2, p = 0.5, mu = 3))
    #' trial$args_model(ate = 5, p = 0.6) # update parameters
    #' trial$args_model(list(ate = 2), p = 0.5) # combine first arg with ...
    #'
    #' # retrieve parameters
    #' trial$args_model() # return all set parameters
    #' trial$args_model("ate") # select a single parameter
    #' trial$args_model(c("ate", "mu")) # multiple parameters
    #'
    #' # remove parameters
    #' trial$args_model(.reset = "ate") # remove a single parameter
    #' trial$args_model(.reset = TRUE) # remove all parameters
    #'
    #' # remove and set/update parameters
    #' trial$args_model(ate = 2, p = 0.5, .reset = TRUE)
    #' trial$args_model(ate = 5, .reset = "p") # removing p and updating ate
    #' @author Benedikt Sommer
    args_model = function(.args = NULL, .reset = FALSE, ...) {
      # using .args and .reset as function arguments to reduce the likelihood of
      # partial argument matching because it is unlikely that one uses
      # parameters .a and .r as argument names for any of the model components.
      return(
        private$get_set_args_estimator(args = .args, reset = .reset, ...,
          attr.name = "model.args")
      )
    },

    #' @description Get, specify or update the summary.args attribute.
        #' @param .args (list or character) named list of arguments to update
    #' or set. A single or subset of arguments can be retrieved by passing the
    #' respective argument names as a character or character vector.
    #' @param .reset (logical or character) Reset all or a subset of previously
    #' set parameters. Can be combined with setting new parameters.
    #' @param ... Alternative to using `.args` to update or set arguments
    #' @examples
    #' trial <- Trial$new(
    #'   covariates = function(n, p = 0.5) data.frame(a = rbinom(n, 1, p)),
    #'   outcome = function(data, ate, mu) rnorm(nrow(data), mu + data$a * ate)
    #' )
    #' # set and update parameters
    #' trial$args_summary(list(level = 0.05, alternative = "<"))
    #' trial$args_summary(level = 0.25) # update parameters
    #'
    #' # retrieve parameters
    #' trial$args_summary() # return all set parameters
    #' trial$args_summary("level") # select a single parameter
    #' trial$args_summary(c("level", "alternative")) # multiple parameters
    #'
    #' # remove parameters
    #' trial$args_summary(.reset = "level") # remove a single parameter
    #' trial$args_summary(.reset = TRUE) # remove all parameters
    #'
    #' # remove and set/update parameters
    #' trial$args_summary(alternative = "!=", level = 0.05, .reset = TRUE)
    #' # removing alternative and setting level
    #' trial$args_summary(level = 0.05, .reset = "alternative")
    args_summary = function(.args = NULL, .reset = FALSE,  ...) {
      return(
        private$get_set_args_estimator(args = .args, reset = .reset, ...,
          attr.name = "summary.args")
      )
    },

    #' @description Get, specify or update estimators.
    #' @param .estimators (list, function or character) Argument controlling
    #' the getter or setter behavior. Estimators are set or updated by providing
    #' a single estimator (function) or list of estimators, and retrieved by
    #' providing a character (see examples).
    #' @param .reset (logical or character) Reset all or a subset of previously
    #' set estimators. Can be combined with setting new estimators.
    #' @param ... Alternative to `.estimators` for updating or setting
    #' estimators.
    #' @details A name is internally assigned to estimators when calling the
    #' method with `.estimators` set to a single estimator or a list with
    #' unnamed elements. The names are a combination of an *est* prefix and an
    #' integer that indicates the *n*th added unnamed estimator.
    #' @examples
    #' estimators <- list(marginal = est_glm(), adj = est_glm(covariates = "x"))
    #' trial <- Trial$new(
    #'   covariates = \(n) data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n)),
    #'   outcome = \(data, ate = -0.5) rnorm(nrow(data), data$a * ate),
    #'   estimators = estimators
    #' )
    #'
    #' # get estimators
    #' trial$estimators() |> names() # list all estimators
    #' trial$estimators("marginal") |> names() # select a single estimator
    #' trial$estimators(c("marginal", "adj")) |> names() # select mult. est.
    #'
    #' # remove estimators
    #' trial$estimators(.reset = "marginal") # remove a single estimator
    #' trial$estimators(.reset = TRUE) # remove all estimators
    #'
    #' # set or update estimators
    #' trial$estimators(estimators)
    #' trial$estimators(adj2 = est_adj(covariates = "x")) # add new estimator
    #' # update adj and remove adj2
    #' trial$estimators(adj = est_glm(covariates = "x"), .reset = "adj2")
    #'
    #' # unnamed estimators (adding default name)
    #' estimators <- list(est_glm(), est_glm(covariates = "x"))
    #' trial$estimators(estimators, .reset = TRUE)
    #' trial$estimators(.reset = "est1")
    #' trial$estimators(est_glm()) # replaces removed est1
    estimators = function(.estimators = NULL, .reset = FALSE, ...) {
      trial_estimators(private, .estimators = .estimators, .reset = .reset, ...)
    },

    #' @description Simulate data by applying parameters to the trial model. The
    #' method samples first from the covariate model. Outcome data sampling
    #' follows by passing the simulated covariate data to the outcome model. An
    #' optional exclusion model is applied to the combined covariates and
    #' outcomes data. The sampling process is repeated in case any subjects are
    #' removed by the exclusion model until a total of `n` subjects are sampled
    #' or the maximum iteration number `.niter` is reached.
    #'
    #' The method adds two auxiliary columns to the simulated data to identify
    #' distinct patients (*id*) and periods (*num*) in case of time-dependent
    #' covariate and outcome models. The columns are added to the sampled
    #' covariate data before sampling the outcomes. A *data.table* with both
    #' columns is provided to the outcome model in case no covariate model is
    #' defined. Thus, the outcome model is always applied to at least a
    #' *data.table* with an *id* and *num* column. The default column name *y*
    #' is used for the outcome variable in the returned *data.table* when the
    #' defined outcome model returns a vector. The name is easily changed by
    #' returning a *data.table* with a named column (see examples).
    #'
    #' The optional argument `...` of this method can be used to provide
    #' parameters to the trial model as an addition to parameters that have
    #' already been defined via [Trial$args_model()][Trial]. Data is simulated
    #' from the union of parameters, where parameters provided via the optional
    #' argument of this method take precedence over parameters defined via
    #' [Trial$args_model()][Trial]. However, parameters that have been set via
    #' [Trial$args_model()][Trial] are not updated when optional arguments are
    #' provided.
    #' @param .niter (integer) Maximum number of simulation runs to avoid
    #'   infinite loops for ill defined exclusion functions.
    #' @return data.table with `n` rows
    #' @examples
    #' trial <- Trial$new(
    #'   covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
    #'   outcome = \(data, ate = 0) with(data, rnorm(nrow(data), a * ate))
    #' )
    #'
    #' # applying and modifying parameters
    #' n <- 10
    #' trial$simulate(n) # use parameters set during initialization
    #' trial$args_model(ate = -100) # update parameters
    #' trial$simulate(n)
    #' trial$simulate(n, ate = 100) # change ate via optional argument
    #'
    #' # rename outcome variable
    #' trial <- Trial$new(
    #'   covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
    #'   outcome = \(data, ate = 0) {
    #'     data.frame(yy = with(data, rnorm(nrow(data), a * ate)))
    #'   }
    #' )
    #' trial$simulate(n)
    #'
    #' # return multiple outcome variables
    #' trial <- Trial$new(
    #'   covariates = \(n) data.frame(a = rbinom(n, 1, 0.5), y.base = rnorm(n)),
    #'   outcome = \(data, ate = 0) {
    #'     y  <-  with(data, rnorm(nrow(data), a * ate))
    #'     return(data.frame(y = y, y.chg = data$y.base - y))
    #'   }
    #' )
    #' trial$simulate(n)
    #'
    #' # use exclusion argument to post-process sampled outcome variable to
    #' # achieve the same as in the above example but without modifying the
    #' # originally defined outcome model
    #' trial <- Trial$new(
    #'   covariates = \(n) data.frame(a = rbinom(n, 1, 0.5), y.base = rnorm(n)),
    #'   outcome = \(data, ate = 0) with(data, rnorm(nrow(data), a * ate)),
    #'   exclusion = \(data) {
    #'    cbind(data, data.frame(y.chg = data$y.base - data$y))
    #'   }
    #' )
    #' trial$simulate(n)
    #'
    #' # no covariate model
    #' trial <- Trial$new(
    #'   outcome = \(data, ate = 0) {
    #'     n <- nrow(data)
    #'     a <- rbinom(n, 1, 0.5)
    #'     return(data.frame(a = a, y = rnorm(n, a * ate)))
    #'   }
    #' )
    #' trial$simulate(n)
    #' @author Klaus Kähler Holst, Benedikt Sommer
    simulate = function(n, .niter = 500L, ...) {
      trial_simulate(self, n = n, .niter = .niter, ...)
    },

    #' @description Run trial and estimate parameters multiple times
    #'
    #' The method calls [Trial$simulate()][Trial] `R` times and applies the
    #' specified estimators to each simulated dataset of sample size `n`.
    #' Parameters to the covariates, outcome and exclusion models can be
    #' provided as optional arguments to this method call in addition to
    #' parameters that have already been defined via
    #' [Trial$args_model()][Trial]. The behavior is identical to
    #' [Trial$simulate()][Trial], except that `.niter` can be provided as an
    #' optional argument to this method for controlling the maximum number of
    #' simulation runs in [Trial$simulate()][Trial].
    #'
    #' Estimators fail silently in that errors occurring when applying an
    #' estimator to each simulated dataset will not terminate the method call.
    #' The returned [trial.estimates-class] object will instead indicate that
    #' estimators failed.
    #' @return (invisible) An object of class [trial.estimates-class], which
    #' contains the estimates of all estimators and all information to repeat
    #' the simulation. The return object is also assigned to the `estimates`
    #' field of this Trial class object (see examples).
    #' @examples
    #' # future::plan("multicore")
    #' trial <- Trial$new(
    #'   covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
    #'   outcome = \(data, ate = 0) with(data, rnorm(nrow(data), a * ate)),
    #'   estimators = list(glm = est_glm())
    #' )
    #' trial$args_summary(alternative = "<")
    #'
    #' # the returned trial.estimates object contains the estimates for each of
    #' # the R simulated data sets + all necessary information to re-run the
    #' # simulation
    #' res <- trial$run(n = 100, R = 50) # store return object in a new variable
    #' print(trial$estimates) # trial$estimates == res
    #'
    #' # the basic usage is to apply the summary method to the generated
    #' # trial.estimates object.
    #' trial$summary()
    #'
    #' # combining Trial$run and summary is faster than using
    #' # Trial$estimate_power when modifying only the parameters of the
    #' # decision-making function
    #' sapply(
    #'   c(0, 0.25, 0.5),
    #'   \(ni) trial$summary(ni.margin = ni)[, "power"]
    #' )
    #'
    #' # changing the ate parameter value
    #' trial$run(n = 100, R = 50, ate = -0.2)
    #'
    #' # supplying another estimator
    #' trial$run(n = 100, R = 50, estimators = est_glm(robust = FALSE))
    run = function(n, R = 100, estimators = NULL, ...) {
      return(trial_run(self, .private = private, n = n, R = R,
        estimators = estimators, ...)
      )
    },

    #' @description Estimates statistical power for a specified trial
    #'
    #' Convenience method that first runs [Trial$run()][Trial] and subsequently
    #' applies [Trial$summary()][Trial] to derive the power for each
    #' estimator. The behavior of passing arguments to lower level functions is
    #' identical to [Trial$run()][Trial].
    #' @examples
    #' # toy examples with small number of Monte-Carlo replicates
    #' # future::plan("multicore")
    #' trial <- Trial$new(
    #'   covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
    #'   outcome = \(data, ate = 0) with(data, rnorm(nrow(data), a * ate)),
    #'   estimators = list(glm = est_glm())
    #' )
    #' trial$args_summary(alternative = "<")
    #'
    #' # using previously defined estimators and summary.args
    #' trial$estimate_power(n = 100, R = 20)
    #'
    #' # supplying parameters to outcome function
    #' trial$estimate_power(n = 100, R = 20, ate = -100)
    #'
    #' # modifying arguments of summary function
    #' trial$estimate_power(n = 100, R = 20, ate = -100,
    #'  summary.args = list(alternative = ">")
    #' )
    #'
    #' # supplying estimators to overrule previously set estimators
    #' trial$estimate_power(n = 100, R = 20,
    #'  estimators = list(est_glm(), est_adj()))
    #' @return numeric
    estimate_power = function(n,  R = 100, estimators = NULL,
      summary.args = list(), ...) {
      sum.args <- self$args_summary()
      sum.args[names(summary.args)] <- summary.args

      args <- c(list(...), list(n = n, R = R, estimators = estimators))
      r <- do.call(self$run, args)
      sr <- do.call(self$summary, c(list(estimates = r), sum.args))
      return(sr[, "power"])
    },

    #' @description Estimate the minimum sample-size required to reach a desired
    #'   statistical power with a specified estimator. An initial rough estimate
    #'   is obtained via bisection, followed by a
    #'   stochastic approximation (Robbins-Monro) algorithm, and finally, a grid
    #'   search (refinement step) in the neighbourhood of the current best
    #'   solution.
    #'
    #' Note that the estimation procedure for the sample size will not populate
    #' the estimates attribute of a trial object.
    #' @param power (numeric) Desired statistical power
    #' @param estimator (list or function) Estimator (function) to be applied.
    #' If NULL, then estimate sample size for all estimators defined via
    #' [Trial$estimators()][Trial]. A prefix *est* is used to label unnamed
    #' estimators.
    #' @param interval (integer vector) Interval in which to initially look for
    #' a solution with the bisection algorithm. Passing an integer will skip the
    #' bisection algorithm and use the provided integer as the initial solution
    #' for the stochastic approximation algorithm
    #' @param bisection.control (list) Options controlling the bisection
    #' algorithm ([bisection]). Default values can also be changed for a
    #' subset of options only (see examples).
    #' @param sa.control (list) Options controlling the stochastic approximation
    #'   (Robbins-Monro) algorithm ([optim_sa]). Default values can also be
    #' changed for a subset of options only (see examples).
    #' @param refinement (integer vector) Vector to create an interval whose
    #' center is the sample size estimate of the Robbins-Monro algorithm.
    #' @param R (integer) Number of replications to use in Monte Carlo
    #'   simulation of refinement calculations.
    #' @param interpolate (logical) If TRUE, a linear interpolation of the
    #'   refinement points will be used to estimate the power.
    #' @param verbose (logical) If TRUE, additional output will be displayed.
    #' @param minimum (integer) Minimum sample size.
    #' @return samplesize_estimate S3 object
    #' @author Klaus Kähler Holst
    #' @examples
    #' \dontrun{
    #' trial <- Trial$new(
    #'   covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
    #'   outcome = \(data, ate, sd) with(data, rnorm(nrow(data), a * ate, sd)),
    #'   estimators = list(marginal = est_glm())
    #' )
    #' trial$args_model(ate = -1, sd = 5)
    #' trial$args_summary(alternative = "<")
    #'
    #' # supply model parameter and estimator to call to overwrite previously
    #' # set values
    #' trial$estimate_samplesize(ate = -2, estimator = est_glm())
    #'
    #' # reduce number of iterations for bisection step but keep R = 100
    #' # (default value)
    #' trial$estimate_samplesize(bisection.control = list(niter = 2))
    #'
    #' # reduce significance level from 0.05 to 0.025, but keep alternative as
    #' # before
    #' trial$estimate_samplesize(summary.args = list(level = 0.025))
    #' }
    estimate_samplesize = function(
      ...,
      power = 0.9,
      estimator = NULL,
      interval = c(50L, 10000L),
      bisection.control = list(
        R = 100,
        niter = 6
      ),
      sa.control = list(
        R = 1,
        niter = 250,
        stepmult = 100,
        alpha = 0.5
      ),
      refinement = seq(-10, 10, by = 5),
      R = 1000,
      interpolate = TRUE,
      verbose = TRUE,
      minimum = 10L,
      summary.args = list()
      ) {
        args <- list(
          power = power,
          estimator = estimator,
          interval = interval,
          bisection.control = bisection.control,
          sa.control = sa.control,
          refinement = refinement,
          R = R,
          interpolate = interpolate,
          verbose = verbose,
          minimum = minimum,
          summary.args = summary.args
        )
        args <- c(list(self = self), args, list(...))

        # don't write to estimates attribute when estimating sample size
        private$state$estimate_samplesize <- TRUE
        res <- tryCatch(
          expr = do.call(trial_estimate_samplesize, args),
          finally = {
            private$state$estimate_samplesize <- FALSE
          }
        )
        return(res)

      },

    #' @description Summarize Monte Carlo studies of different estimators for
    #'   the treatment effect in a randomized clinical trial. The method reports
    #'   the power of both superiority tests (one-sided or two-sided) and
    #'   non-inferiority tests, together with summary statistics of the
    #'   different estimators.
    #' @param level (numeric) significance level
    #' @param null (numeric) null hypothesis to test
    #' @param ni.margin (numeric) non-inferiority margin
    #' @param alternative alternative hypothesis (not equal !=, less <,
    #'   greater >)
    #' @param reject.function Optional function calculating whether to reject
    #'   the null hypothesis
    #' @param true.value Optional true parameter value
    #' @param nominal.coverage Width of confidence limits
    #' @param estimates Optional trial.estimates object. When provided, these
    #'   estimates will be used instead of the object's stored estimates. This
    #'   allows calculating summaries for different trial results without
    #'   modifying the object's state.
    #' @param ... additional arguments to lower level functions
    #' @return matrix with results of each estimator stored in separate rows
    #' @examples
    #' outcome <- function(data, p = c(0.5, 0.25)) {
    #'   a <- rbinom(nrow(data), 1, 0.5)
    #'   data.frame(a = a, y = rbinom(nrow(data), 1, p[1] * (1 - a) + p[2] * a)
    #'   )
    #' }
    #' trial <- Trial$new(outcome, estimators = est_glm())
    #' trial$run(n = 100, R = 100)
    #' # two-sided test with 0.05 significance level (alpha = 0.05) (default
    #' # values)
    #' trial$summary(level = 0.05, alternative = "!=")
    #' # on-sided test
    #' trial$summary(level = 0.025, alternative = "<")
    #' # non-inferiority test
    #' trial$summary(level = 0.025, ni.margin = -0.5)
    #'
    #' # provide simulation results to summary method via estimates argument
    #' res <- trial$run(n = 100, R = 100, p = c(0.5, 0.5))
    #' trial$summary(estimates = res)
    #'
    #' # calculate empirical bias, rmse and coverage for true target parameter
    #' trial$summary(estimates = res, true.value = 0)
    summary = function(level = .05,
                       null = 0,
                       ni.margin = NULL,
                       alternative = "!=",
                       reject.function = NULL,
                       true.value = NULL,
                       nominal.coverage = 0.9,
                       estimates = NULL,
                       ...) {
    trial_summary(self = self, level = level, null = null,
      ni.margin = ni.margin, alternative = alternative,
      reject.function = reject.function, true.value = true.value,
      nominal.coverage = nominal.coverage, estimates = estimates, ...
    )
    },

    #' @description Print method for Trial objects
    #' @param verbose (logical) By default, only print the
    #' function arguments of the covariates, outcome and exclusion models. If
    #' *TRUE*, then also print the function body.
    #' @param ... Additional arguments to lower level functions (not used).
    #' @examples
    #' trial <- Trial$new(
    #'   covariates = function(n) data.frame(a = rbinom(n, 1, 0.5)),
    #'   outcome = function(data, sd = 1) rnorm(nrow(data), data$a * -1, sd),
    #'   estimators = list(marginal = est_glm()),
    #'   info = "Some trial info"
    #' )
    #' trial$args_model(sd = 2)
    #' trial$args_summary(level = 0.025)
    #'
    #' print(trial) # only function headers
    #' print(trial, verbose = TRUE) # also print function bodies
    print = function(..., verbose = FALSE) {
      trial_print(self = self, verbose = verbose, ...)
      return(invisible())
    }
  ),
  active = list(),
  private = list(
    state = list(estimate_samplesize = FALSE),
    # default values of summary method arguments (see initialize method)
    summary.args_default = list(),
    model.args = list(),
    summary.args = list(),
    .estimators = list(),
    # utility to set, update or retrieve parameters of model.args, summary.args
    # and estimators.
    # #' @param attr.name name of the private attribute where the model.args,
    # summary.args or estimators are stored
    get_set_args_estimator = function(args = NULL, reset = FALSE,
      attr.name, ...) {
      dots <- list(...)

      method_call <- rev(sys.calls())[[2]] # assumes that only public methods
      # call this method
      .stop <- function(msg) rlang::abort(msg, call = method_call)
      first_arg_in_method_call <- ifelse(
        attr.name %in% c("model.args", "summary.args"), ".args", ".estimators"
      )
      .flush <- function() {
        # only flush estimates field when setting or updating model.args or
        # estimators
        if (attr.name %in% c("model.args", ".estimators")) {
          self$estimates <- NULL
        }
      }

      # first handle getter for a subset of parameters
      if (is.character(args)) {
        res <- private[[attr.name]][c(args)]
        # only return parameters that exist in the attribute
        res <- res[!is.na(names(res))]
        # unlist when only returning a single parameter or estimator
        if (length(res) == 1) res <- res[[1]]
        return(res)
      }

      if (
        !is.null(args) &&
        !(is.list(args) && !is.null(names(args))) && # list without names
        (attr.name != ".estimators") # exception for estimators
      ) {
        .stop(sprintf("%s must be a named list.", first_arg_in_method_call))
      }
      if (length(intersect(names(args), names(dots)))) {
        .stop(
          sprintf("Colliding arguments in %s and ...", first_arg_in_method_call)
        )
      }
      all_args <- c(args, dots)

      if (all(reset == TRUE) || is.character(reset)) {
        # reset attribute when no arguments are passed and reset == TRUE
        if (length(all_args) == 0) {
          if (is.logical(reset)) {
            private[[attr.name]] <- list()
            if (attr.name == "summary.args") {
              private$summary.args <- private$summary.args_default
            }
          } else {
            .exists <- sapply(c(reset), \(x) x %in% names(private[[attr.name]]))
            notfound <- names(.exists)[!.exists]
            msg <- ifelse(length(notfound) > 1,
              "Trying to reset parameters %s which have not been set yet.",
              "Trying to reset parameter %s which has not been set yet."
            )
            if (length(notfound)) {
              .stop(sprintf(msg, paste(notfound, collapse = ", ")))
            }
            private[[attr.name]][c(reset)] <- NULL
          }
          .flush()
          return(invisible())
        }
        if (is.logical(reset)) {
          # reset and set when args argument or dots are passed
          private[[attr.name]] <- all_args
        } else { # only reset specified parameters
          private[[attr.name]][c(reset)] <- NULL
          private[[attr.name]][names(all_args)] <- all_args
        }
        .flush()
        return(invisible())
      }

      # reset == FALSE and no arguments are passed. just return attribute
      if (length(all_args) == 0) return(private[[attr.name]])

      # update attribute with all passed arguments
      private[[attr.name]][names(all_args)] <- all_args
      .flush()
      return(invisible())
    }
  )
)

#' @export
print.samplesize_estimate <- function(x, ...) {
  pow <- formatC(x$power * 100, ...)
  estpow <- formatC(x$estimate_power * 100, ...)
  cat_header("Estimated sample-size to reach %s%% power", pow)
  cat(sprintf(
    "n = %d (actual estimated power\u2248%s%%)\n",
    coef(x), estpow
    ))
  return(invisible(x))
}

#' @export
coef.samplesize_estimate <- function(object, ...) {
  return(object$estimate)
}

trial_summary <- function(self, level, null, ni.margin, alternative,
  reject.function, true.value, nominal.coverage, estimates, ...) {
  est <- if (!is.null(estimates)) estimates else self$estimates
  if (is.null(est)) {
    stop("No estimates available. Run trial first.")
  }

  if (!(alternative %in% c("!=", "<", ">"))) {
    rlang::abort('alternative should be one of "!=", "<", ">"')
  }

  alternative <- gsub(" ", "", tolower(alternative[1]))
  q_alpha_cov <- qnorm(1 - (1 - nominal.coverage) / 2) # quantile for
  # calculating the nominal coverage when true.value is supplied
  q_alpha_rej <- qnorm(1 - level / 2) # quantile used for decision making
  # about the null hypothesis (here two-sided test, below for one-sided
  # test)
  if (alternative == "<") {
    alternative <- "<"
    q_alpha_rej <- qnorm(1 - level)
  }
  if (alternative == ">") {
    alternative <- ">"
    q_alpha_rej <- qnorm(1 - level)
  }
  if (!is.null(ni.margin)) {
    q_alpha_rej <- qnorm(1 - level)
    alternative <- ifelse(ni.margin >= 0, "<", ">")
  }

  if (is.null(reject.function)) {
    reject.function <- function(lower, upper, ...) {
      if (alternative == "<") {
        if (!is.null(ni.margin)) {
          return(upper < ni.margin)
        }
        return(upper < null)
      }
      if (alternative == ">") {
        if (!is.null(ni.margin)) {
          return(lower > ni.margin)
        }
        return(lower > null)
      }
      return((upper < null) | (lower > null))
    }
  } else {
    reject.function <- add_dots(reject.function)
  }

  inference <- function(estimates) {
    recalc <- function(estimates) {
      est <- estimates[, c("Estimate", "Std.Err")] |>
          as.data.frame()
      point_est <- est[, "Estimate"]
      est[, "z.score"] <- (point_est - null) / est[, "Std.Err"]
      est[, "lower.CI"] <- point_est - q_alpha_rej * est[, "Std.Err"]
      est[, "upper.CI"] <- point_est + q_alpha_rej * est[, "Std.Err"]
      est[, "lower.CI.cov"] <- point_est - q_alpha_cov * est[, "Std.Err"]
      est[, "upper.CI.cov"] <- point_est + q_alpha_cov * est[, "Std.Err"]
      est[, "Reject"] <- reject.function(
        estimate = est[, "Estimate"],
        stderr = est[, "Std.Err"],
        lower = est[, "lower.CI"],
        upper = est[, "upper.CI"]
      )
      return(est)
    }
      estimates <- recalc(estimates)
      out <- with(estimates, c(
        estimate = mean(Estimate, na.rm=TRUE),
        std.err = mean(Std.Err, na.rm=TRUE),
        std.dev = sd(Estimate, na.rm=TRUE),
        power = mean(Reject, na.rm=TRUE),
        na = sum(is.na(Estimate))
      ))
      if (!is.null(true.value)) {
        est_err <- estimates[, "Estimate"] - true.value
        out[["bias"]] <- mean(est_err, na.rm = TRUE)
        out[["rmse"]] <- mean(est_err ** 2, na.rm = TRUE) ** 0.5
        out[["coverage"]] <- mean(
          (estimates[, "lower.CI.cov"] <= true.value) &
            (true.value <= estimates[, "upper.CI.cov"]), na.rm = TRUE)
      }
      return(out)
  }

  res <- lapply(est$estimates, \(est) inference(estimates = est))

  res <- do.call(rbind, res)
  return(res)
}

trial_simulate <- function(self, n, .niter, ...) {
  call.args <- self$args_model()
  # model.args is initialized as an empty list. thus we can just update
  # this list with arguments that are passed to this method
  call.args[names(list(...))] <- list(...)

  res <- data.table()
  count <- 0
  nsim <- n
  while (nrow(res) < nsim) {
    ids <- seq_len(n) + n * count
    x0 <- data.table()
    x0[, `:=`(id = ids)] # Append subject id variable
    setkeyv(x0, "id")
    ## Simulate time-varying covariates
    if (!is.null(self$covariates)) {
      xt <- do.call(self$covariates, c(list(n = n), call.args))
      if (is.data.table(xt) || is.data.frame(xt)) {
        xt <- list(xt)
      }
      xt <- lapply(xt, data.table)
      if (count == 0) nsim <- n * length(xt)
      for (i in seq_len(length(xt))) {
        ## Append subject id and period variable
        xt[[i]][, `:=`(id = ids, num = i - 1)]
      }
      xt <- Reduce("rbind", xt)
      setkeyv(xt, "id")
      ## Combined data
      x <- merge(x0, xt)
    } else {
      x <- x0
    }
    ## Draw outcome given covariates
    y <- do.call(self$outcome_model, c(list(x), call.args))
    res <- rbind(
      res,
      do.call(self$exclusion, c(list(cbind(x, y)), call.args))
    )
    count <- count + 1
    if (count == .niter) {
      paste(
        "Maximum iterations reached (exclusion criterion). Either modify the",
        "exclusion criterion or increase the number of iterations (.niter)."
      ) |> stop()
    }
  }
  colcount <- table(colnames(res))
  duplicates <- names(colcount)[colcount > 1]
  if (length(duplicates) > 0) {
    sprintf(
      paste(
        "Duplicated columns found in the simulated dataset (%s).",
        "Please check the covariate and outcome models."
      ),
      paste(duplicates, collapse = ", ")
    ) |>
    stop()
  }

  return(res[seq_len(nsim)])
}

trial_estimate_samplesize <- function(self, power, estimator, interval,
  bisection.control, sa.control, refinement, R, interpolate, verbose,
  summary.args, minimum, ...) {
  .call <- rlang::call_match()
  .call$estimator <- NULL
  .call[[1]] <- estimate_samplesize_1estimator

  method_call <- rev(sys.calls())[[3]]
  .stop <- function(msg) rlang::abort(msg, call = method_call)

  if (power >= 1 || power < 0) .stop("Power must be between 0 and 1")

  if (is.null(estimator) && length(self$estimators()) > 0) {
    estimator <- self$estimators()
  } else if (is.function(estimator)) {
    estimator <- list(estimator)
  } else {
    .stop(paste(
      "Missing estimator.",
      "Provide an estimator to this method or set them as an attribute.")
    )
  }
  # use seq_along(estimator) as names when names(estimator) is NULL
  names(estimator) <- names(estimator) %||% paste0("est", seq_along(estimator))

  f <- function(i) {
    tryCatch({
      .call$estimator <- estimator[[i]]
      eval(.call)
    },
      error = \(e) {
        .stop(sprintf("Error in estimator %s.", names(estimator)[[i]]))
      }
    )
  }
  res <- lapply(seq_along(estimator), f)
  names(res) <- names(estimator)

  # return "unlisted" output when only one estimator is passed as a
  # function
  if (length(res) == 1) res <- res[[1]]

  return(res)
}

estimate_samplesize_1estimator <- function(self, power, estimator, interval,
  bisection.control, sa.control, refinement, R, interpolate, verbose,
  summary.args, minimum, ...) {

  f <- function(n, R = 1) {
    return(
      self$estimate_power(n = round(n), R = R, estimators = estimator,
        summary.args = summary.args, ...)[1] - power
    )
  }
  if (length(interval) == 2) {
    control <- list(niter = 6, R = 100)
    control[names(bisection.control)] <- bisection.control
    log_info("Finding initial sample-size with bisection algorithm")
    init <- bisection(f, interval,
      R = control$R,
      niter = control$niter,
      verbose = verbose
      )
  } else {
    init <- interval[1]
  }
  control <- list(niter = 250, stepmult = 100, alpha = 0.5, R = 1)
  control[names(sa.control)] <- sa.control
  log_info("Running stochastic approximation algorithm")
  val <- optim_sa(f, init,
    function.args = list(R = control$R),
    method = "discrete",
    control = control, projection = function(x) max(x, minimum)
  )
  log_info("Refining estimate and calculating power")
  nn <- round(refinement + val$estimate)
  nn <- unique(pmax(nn, minimum))
  pow <- numeric(length(nn))
  for (i in seq_along(nn)) {
    pow[i] <- f(nn[i], R = R)
    log_info(
      "[{i}/{length(nn)}] - power({nn[i]})",
      " = {format(pow[i]+power, digits=3)}"
    )
  }
  if (interpolate && length(nn) > 2) {
    l <- lm(pow ~ nn)
    newd <- data.frame(nn = seq(min(nn), max(nn), length.out = 100))
    pr <- predict(l, newdata = newd)
    idx <- which.min(abs(pr))
    res <- round(newd$nn[idx])
    respow <- pr[[idx]] + power
  } else {
    idx <- which.min(abs(pow))
    res <- nn[idx]
    respow <- pow[[idx]] + power
  }
  log_info(sprintf("Estimated sample size: %s", res))
  obj <- list(
    stochastic.approximation = val,
    refinement = cbind(n = nn, power = pow),
    R = R,
    power = power,
    estimate_power =  respow,
    estimate = res
  ) |>
    structure(class = "samplesize_estimate")
  return(obj)
}

trial_print <- function(self, verbose, ...) {
  if (class(self)[1] == "Trial") {
    cat(paste("\u2500\u2500", "Trial Object", "\u2500\u2500"), "\n")
  }
  if (length(self$info) > 0) {
    cat(self$info[[1]], "\n")
    if (length(self$info) > 1) {
      for (i in 2:length(self$info)) {
        cat(self$info[[i]], "\n")
      }
    }
  }
  cat("\n")

  cat("Model arguments: ", "\n")
  model.args <- self$args_model()

  for (i in seq_along(model.args)) {
    nam <- names(model.args)[i]
    cat_bullet(paste0(nam, ": "))
    str(model.args[[i]], max.level = 1, give.attr = FALSE, comp.str = "  $ ")
  }
  cat("\n")

  cat("Summary arguments: ", "\n")
  summary.args <- self$args_summary()
  for (i in seq_along(summary.args)) {
    nam <- names(summary.args)[i]
    cat_bullet(paste0(nam, ": "))
    str(summary.args[[i]], max.level = 1, give.attr = FALSE, comp.str = "  $ ")
  }
  cat("\n")

  if (verbose) {
    .cat <- \(name, fun) {
      cat(paste0(name, ":\n"))
      fun <- deparse(fun)
      cat(" ", fun[[1]], "\n")
      if (length(fun) > 1) {
        for (i in 2:length(fun)) {
          cat("   ", fun[[i]], "\n")
        }
      }
      cat("\n")
    }
    .cat("Covariates", self$covariates)
    .cat("Outcome", self$outcome_model)
    .cat("Exclusion", self$exclusion)
  } else {
    format_fun <- \(fun) capture.output(str(fun, give.attr = FALSE))
    cat("Covariates:\n", format_fun(self$covariates), "\n\n")
    cat("Outcome:\n", format_fun(self$outcome_model), "\n\n")
    cat("Exclusion:\n", format_fun(self$exclusion), "\n\n")
  }

  cat("Estimators:\n")
  if (length(estimators <- self$estimators())) {
    for (i in seq_along(estimators)) {
      msg <- names(estimators)[i]
      cat("  ", i, ". ", sep = "")
      cat(msg, "\n")
    }
  }

  cat_rule(20)
  cat("\n")
  return(invisible(self))
}

# Getter and setter method implementation for Trial$args_model and
# Trial$args_summary
trial_args_getter_setter <- function(self, args = NULL, args.name, ...) {
  if (
    !is.null(args) &&
      !(is.list(args) && !is.null(names(args))) && # list without names
      !(length(args) == 0) # make exception for empty list

  ) {
    stop(sprintf("%s must be a named list", args.name))
  }

  dots <- list(...)
  if (
    length(intersect(names(args), names(dots)))
  ) {
    stop(sprintf("Colliding arguments in %s and ...", args.name))
  }

  if (length(dots) == 0 && is.null(args)) {
    return(self[[args.name]])
  } else if (length(args) == 0 && length(dots) == 0) {
    self[[args.name]] <- list()
    return(invisible())
  }

  all_args <- c(args, dots)
  self[[args.name]][names(all_args)] <- all_args
  return(invisible())
}

trial_estimators <- function(private, .estimators, .reset, ...) {
  est_prefix <- "est"

  if (is.function(.estimators)) .estimators <- list(.estimators)
  if (is.list(.estimators)) {
    # allow adding a list with unnamed estimators
    if (is.null(names(.estimators))) {
      names(.estimators) <- rep("", length(.estimators))
    }
    .names <- names(.estimators)
    mask <- .names == ""
    # first use temporary name for unnamed estimators. rename afterwards
    # to handle name clashes
    tmp_names <- paste0("xxxxxxx", seq_len(sum(mask)))
    .names[mask] <- tmp_names
    names(.estimators) <- .names
  }
  res <- private$get_set_args_estimator(args = .estimators, reset = .reset,
    ..., attr.name = ".estimators")

  # add integer postfix to each unnamed estimator.
  if (exists("tmp_names") && length(.estimators) > 0) {
    all_names <- names(private$.estimators)
    # check if unnamed estimators have already been added
    ex_names <- all_names[grep(sprintf("%s[1-9]", est_prefix), all_names)]
    if (length(ex_names) == 0) {
      upd_names <- paste0(est_prefix, seq_len(sum(mask)))
    } else {
      upd_names <- c()
      # rename each unnamed estimator incrementally. increase number if
      # a postfix already exists
      for (i_est in seq_len(sum(mask))) {
        postfix <- i_est
        while (TRUE) {
          proposed_name <- paste0(est_prefix, postfix)
          if (proposed_name %in% union(ex_names, upd_names)) {
            postfix <- postfix + 1
          } else {
            upd_names <- c(upd_names, proposed_name)
            break()
          }
        }
      }
    }
    all_names[grep("xxxxxxx", all_names)] <- upd_names
    names(private$.estimators) <- all_names
  }

  if (is.null(res)) return(invisible())
  return(res)
}
