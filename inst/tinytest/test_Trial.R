# Unit tests for 'Trial' trial simulation class
library("tinytest")
set.seed(42)

test_estimate_samplesize <- function() {
  trial <- Trial$new(
    covariates = function(n) data.frame(a = rbinom(n, 1, 0.5)),
    outcome = function(data, p) {
      rbinom(nrow(data), 1, p[1] * (1 - data$a) + p[2] * data$a)
    }
  )

  logger::log_threshold(logger::ERROR)
  par <- c(0.5, 0.25)
  args <- list(
    interval = c(50, 300),
    bisection.control = list(niter = 0, R = 10),
    sa.control = list(niter = 5),
    power = 0.9,
    refinement = 0,
    R = 100,
    verbose = FALSE,
    p = par
  )

  # test with a single estimator supplied via method call
  est <- do.call(
    trial$estimate_samplesize,
    c(args, list(estimator = est_glm()))
  )

  # estimate field is not set when using estimate_samplesize
  expect_null(trial$estimates)

  # test that estimated sample size is printed correctly and that method returns
  # a samplesize_estimate object and not a list (which is the case when)
  # multiple estimators are supplied
  expect_stdout(print(est), sprintf("n = %s", est$estimate))

  # test that sample size is estimated approximately
  p1 <- power.prop.test(n = coef(est)/2, p1 = par[1], p2 = par[2])
  expect_equal(est$estimate_power, p1$power, tolerance = 0.1)


  # test that summary.args are passed on to estimate_power method and
  # that estimator is picked up from attribute
  trial$estimators(glm = est_glm())
  trial$estimates <- 1 # quick check to verify that estimate_samplesize
  # does not overwrite existing estimates attribute

  est1 <- do.call(trial$estimate_samplesize,
    c(args, list(summary.args = list(alternative = ">")))
  )
  # sample size cannot have any power because true E[Y(a = 1)] - E[Y(a = 0)] =
  # -0.25
  expect_equal(est1$estimate_power, 0)
  expect_equal(trial$estimates, 1)

  # inform user about failing estimator
  trial$estimators(failing_estimator = \(data) stop("error"))
  expect_error(
    do.call(trial$estimate_samplesize, args),
    pattern = "Error in estimator failing_estimator"
  )

  # error in estimate_samplesize will reset internal estimate_samplesize flag
  # to FALSE. Thus, calling run will populated the estimates field
  res <- trial$run(n = 50, R = 5, p = par, estimators = list(est_glm()))
  expect_equal(res$estimates$glm, trial$estimates$estimates$glm)


  # use est prefix as name for unnamed list of estimators and fail
  # on first estimator
  trial$estimators(list(\(data) stop("error"), est_glm()), .reset = TRUE)
  expect_error(
    do.call(trial$estimate_samplesize, args),
    pattern = "Error in estimator est1"
  )

  # Test with multiple estimators
  trial$estimators(list(est1 = est_glm(), est2 = est_glm()), .reset = TRUE)
  est_list <- do.call(trial$estimate_samplesize, args)

  expect_true(is.list(est_list))
  # names of returned list correspond to names of estimators
  expect_equal(names(est_list), names(trial$estimators()))

  for (e in est_list) {
    p1 <- power.prop.test(n = coef(e)/2, p1 = par[1], p2 = par[2])
    expect_equal(e$estimate_power, p1$power, tolerance = 0.1)
  }

  # error when no estimator is defined or passed to method call
  trial$estimators(.reset = TRUE)
  expect_error(trial$estimate_samplesize(), pattern = "Missing estimator")

  # power argument must be smaller than 1
  expect_error(
    trial$estimate_samplesize(power = 1.1),
    pattern = "Power must be between 0 and 1"
  )
}
test_estimate_samplesize()

test_estimators <- function() {
  trial <- Trial$new(outcome = function(dd) rnorm(nrow(dd)))
  # trial can be initiated without estimators
  expect_equal(trial$estimators(), list())

  # trial can be initiated with estimators
  estimators <- list(a = est_glm(), b = est_glm())
  trial <- Trial$new(
    outcome = function(dd) rnorm(nrow(dd)),
    estimators = estimators
  )
  expect_equal(trial$estimators(), estimators)

  # estimators are successfully updated and estimators can be retrieved by their
  # name
  trial$estimators(a = est_adj())
  expect_equal(trial$estimators("a"), est_adj())

  trial$estimators(list(a = est_glm()))
  expect_equal(trial$estimators("a"), est_glm())

  # only return estimators that exist
  expect_equal(trial$estimators(c("a", "aa")), estimators[["a"]])
  expect_equivalent(trial$estimators(c("aa")), list())

  # a single estimator can be removed by its name
  trial$estimators(.reset = "a")
  expect_equal(trial$estimators(), estimators["b"])

  # all estimators can be removed
  trial$estimators(.reset = TRUE)
  expect_equal(trial$estimators(), list())

  # use function in case we change the default name prefix later on
  def_name <- \(i) paste0("est", i)
  # default names are assigned when a list with unnamed estimators is passed
  trial$estimators(list(est_glm(), est_glm()))
  expect_equal(names(trial$estimators()), def_name(c(1, 2)))

  # re-use "first" unnamed estimator because it becomes available after
  # resetting est1. by default, the new est1 estimator is appended to the right
  trial$estimators(est_glm(), .reset = def_name(1))
  expect_equal(names(trial$estimators()), def_name(c(2, 1)))

  # the behavior is the same when resetting both estimators with default names
  trial$estimators(list(est_glm(), est_glm()), .reset = def_name(c(2, 1)))
  expect_equal(names(trial$estimators()), def_name(c(1, 2)))

  # new unnamed estimators "fill around" previously added unnamed estimators
  trial$estimators(list(est_adj(), est_glm()), .reset =  def_name(1))
  expect_equal(names(trial$estimators()), def_name(c(2, 1, 3)))

  # only treat estimators with est[1-9] as unnamed estimators
  trial$estimators(est.1 = est_adj(), .reset = TRUE)
  trial$estimators(est_glm())
  expect_equal(names(trial$estimators()), def_name(c(".1", "1")))

  # resetting a single unnamed estimator works when adding a) an unnamed b)
  # a named estimator
  trial$estimators(est1 = est_adj(), .reset = TRUE)
  trial$estimators(est_adj(), .reset =  def_name(1))
  expect_equal(trial$estimators(), list(est1 = est_adj()))
  trial$estimators(est1 = est_adj(), .reset =  def_name(1))
  expect_equal(trial$estimators(), list(est1 = est_adj()))

  # combination of unnamed and named estimators
  trial$estimators(est_glm(), est1 = est_adj(), .reset = TRUE)
  expect_equal(trial$estimators(), list(est2 = est_glm(), est1 = est_adj()))

  # order is left unchanged
  trial$estimators(list(est_glm(), est1 = est_adj()), .reset = TRUE)
  expect_equal(trial$estimators(), list(est2 = est_glm(), est1 = est_adj()))

  # previously set unnamed estimator can be updated
  trial$estimators(est2 = est_adj())
  expect_equal(trial$estimators()[["est2"]], est_adj())

  # a single unnamed argument cannot be used to update a previously set
  # unnamed estimator
  trial$estimators(est_glm())
  expect_equal(names(trial$estimators()), def_name(c(2, 1, 3)))
}
test_estimators()

# testing args_model method of Trial class
test_args_model <- function() {
  trial <- Trial$new(
    covariates = function(n) data.frame(a = rbinom(n, 1, 0.5)),
    outcome = function(data, ate) rnorm(nrow(data), data$a * ate)
  )
  # test setter functionalities
  ate <- 2
  args <- list(ate = ate)
  # setting parameter with list works
  trial$args_model(.args = args)
  # return args when calling without arguments
  expect_equal(trial$args_model(), args)
  # existing parameters are updated / also possible to use unnamed argument
  trial$args_model(list(ate = ate + 1))
  expect_equal(trial$args_model(), list(ate = ate + 1))

  # setting parameter with optional arguments work and overwrite parameters
  # in args attribute
  trial$args_model(ate = ate + 1)
  expect_equal(trial$args_model(), list(ate = ate + 1))
  # setting parameters with args argument and optional arguments
  add_args <- list(p = 2)
  trial$args_model(ate = ate, .args = add_args)
  expect_equal(trial$args_model(), c(args, add_args))


  # trying to set arguments without named list
  expect_error(
    trial$args_model(.args = c(a=1)),
    pattern = ".args must be a named list"
  )
  expect_error(
    trial$args_model(1),
    pattern = ".args must be a named list"
  )
  expect_error(
    trial$args_model(.args = list()),
    pattern = ".args must be a named list"
  )

  # colliding arguments in model.args and optional arguments
  expect_error(
    trial$args_model(.args = args, ate = 1),
    pattern = "Colliding arguments in .args and ..."
  )

  # reset all previous parameters and add new ones
  trial$args_model(a = 1, b = 2, c = 3, .reset = TRUE)
  expect_equal(trial$args_model(), list(a = 1, b = 2, c = 3))

  # removing a single parameter
  trial$args_model(.reset = "c")
  expect_equal(trial$args_model(), list(a = 1, b = 2))

  # reset also accepts character vector
  trial$args_model(.reset = c("a", "b"))
  expect_equal(length(trial$args_model()), 0)

  # resetting parameter that doesn't exist results in error
  expect_error(
    trial$args_model(.reset = "aa"),
    pattern = "Trying to reset parameter aa"
  )

  # correct error message when more than one parameter does not exist
  expect_error(
    trial$args_model(.reset = c("aa", "bb")),
    pattern = "Trying to reset parameters aa, bb"
  )

  # removing single parameter and adding a new parameters
  trial$args_model(a = 1, b = 2, c = 3)
  trial$args_model(c = 3, .reset = "a")
  expect_equal(trial$args_model(), list(b = 2, c = 3))

  # test getter functionality / multiple parameters are returned as a list
  # and single parameters are unlisted
  trial$args_model(a = 1, b = 2, .reset = TRUE)
  expect_equal(trial$args_model("b"), 2)
  expect_equal(trial$args_model(c("b", "a")), list(b = 2, a = 1))
  # return empty (named) list when all parameters are not found
  expect_equivalent(trial$args_model("c"), list())
  expect_equivalent(trial$args_model(c("c", "d")), list())
  # only return parameters that were found
  expect_equal(trial$args_model(c("c", "a")), 1)
  expect_equal(trial$args_model(c("c", "a", "b")), list(a = 1, b = 2))

  # test that setting and updating parameters flushes estimate attribute
  trial$args_model(.reset = TRUE)
  trial$estimates <- 2 # quicker way to populate estimates field than calling
  # run method
  trial$args_model(aa = 22)
  expect_null(trial$estimates)

  # resetting also flushes estimates
  trial$estimates <- 2
  trial$args_model(.reset = TRUE)
  expect_null(trial$estimates)

  # retrieving parameters does not flush estimates
  trial$args_model(aa = 22)
  trial$estimates <- 2
  expect_equal(trial$args_model("aa"), 22)
  expect_equal(trial$estimates, 2)

  # resetting non-existing parameter won't flush estimates
  expect_error(trial$args_model(.reset = "sdfds"))
  expect_equal(trial$estimates, 2)
}
test_args_model()

test_args_summary <- function() {
  trial <- Trial$new(
    covariates = function(n) data.frame(a = rbinom(n, 1, 0.5)),
    outcome = function(data, ate) rnorm(nrow(data), data$a * ate),
    summary.args = list(level = 0.025)
  )

  default_args <- list( # default values of Trial$summary method
    level = .05,
    null = 0,
    ni.margin = NULL,
    alternative = "!=",
    reject.function = NULL,
    true.value = NULL,
    nominal.coverage = 0.9
  )

  # arguments are set correctly when using summary.args / test getter
  args <- default_args
  args$level <- 0.025
  expect_equal(trial$args_summary(), args)
  expect_equal(trial$args_summary("level"), 0.025)

  # update existing parameter
  trial$args_summary(level = 0.05)
  expect_equal(trial$args_summary("level"), 0.05)

  # update 1 parameter and add 2 others
  trial$args_summary(list(level = 0.025, aa = 1, bb = 2))
  expect_equal(
    trial$args_summary(c("level", "aa", "bb")),
    list(level = 0.025, aa = 1, bb = 2)
  )

  # remove two parameters
  trial$args_summary(.reset = c("aa", "bb"))
  expect_equal(trial$args_summary(), args)

  # reset parameter that doesn't exist raises error
  expect_error(
    trial$args_summary(.reset = "aa"),
    pattern = "Trying to reset parameter aa which has not been set yet"
  )

  # reset and set works
  trial$args_summary(.reset = "level", level = 0.1)
  expect_equal(trial$args_summary("level"), 0.1)

  # same with list argument
  trial$args_summary(.reset = "level", list(level = 0.2, null = 1))
  expect_equal(
    trial$args_summary(c("level", "null")),
    list(level = 0.2, null = 1)
  )

  # reset all parameters. this reset parameters against the default values of
  # the summary method
  trial$args_summary(.reset = TRUE)
  expect_equal(trial$args_summary(), default_args)

  # updating summary.args won't reset estimates
  trial$estimates <- 2
  trial$args_summary(.reset = TRUE, aa = 2)
  expect_equal(trial$estimates, 2)

}
test_args_summary()

test_estimators <- function() {
  trial <- Trial$new(outcome = function(dd) rnorm(nrow(dd)))
  # trial can be initiated without estimators
  expect_equal(trial$estimators(), list())

  # trial can be initiated with estimators
  estimators <- list(a = est_glm(), b = est_glm())
  trial <- Trial$new(
    outcome = function(dd) rnorm(nrow(dd)),
    estimators = estimators
  )
  expect_equal(trial$estimators(), estimators)

  # estimators are successfully updated and estimators can be retrieved by their
  # name
  trial$estimators(a = est_adj())
  expect_equal(trial$estimators("a"), est_adj())

  trial$estimators(list(a = est_glm()))
  expect_equal(trial$estimators("a"), est_glm())

  # only return estimators that exist
  expect_equal(trial$estimators(c("a", "aa")), estimators[["a"]])
  expect_equivalent(trial$estimators(c("aa")), list())

  # a single estimator can be removed by its name
  trial$estimators(.reset = "a")
  expect_equal(trial$estimators(), estimators["b"])

  # all estimators can be removed
  trial$estimators(.reset = TRUE)
  expect_equal(trial$estimators(), list())

  # use function in case we change the default name prefix later on
  def_name <- \(i) paste0("est", i)
  # default names are assigned when a list with unnamed estimators is passed
  trial$estimators(list(est_glm(), est_glm()))
  expect_equal(names(trial$estimators()), def_name(c(1, 2)))

  # re-use "first" unnamed estimator because it becomes available after
  # resetting est1. by default, the new est1 estimator is appended to the right
  trial$estimators(est_glm(), .reset = def_name(1))
  expect_equal(names(trial$estimators()), def_name(c(2, 1)))

  # the behavior is the same when resetting both estimators with default names
  trial$estimators(list(est_glm(), est_glm()), .reset = def_name(c(2, 1)))
  expect_equal(names(trial$estimators()), def_name(c(1, 2)))

  # new unnamed estimators "fill around" previously added unnamed estimators
  trial$estimators(list(est_adj(), est_glm()), .reset =  def_name(1))
  expect_equal(names(trial$estimators()), def_name(c(2, 1, 3)))

  # only treat estimators with est[1-9] as unnamed estimators
  trial$estimators(est.1 = est_adj(), .reset = TRUE)
  trial$estimators(est_glm())
  expect_equal(names(trial$estimators()), def_name(c(".1", "1")))

  # resetting a single unnamed estimator works when adding a) an unnamed b)
  # a named estimator
  trial$estimators(est1 = est_adj(), .reset = TRUE)
  trial$estimators(est_adj(), .reset =  def_name(1))
  expect_equal(trial$estimators(), list(est1 = est_adj()))
  trial$estimators(est1 = est_adj(), .reset =  def_name(1))
  expect_equal(trial$estimators(), list(est1 = est_adj()))

  # combination of unnamed and named estimators
  trial$estimators(est_glm(), est1 = est_adj(), .reset = TRUE)
  expect_equal(trial$estimators(), list(est2 = est_glm(), est1 = est_adj()))

  # order is left unchanged
  trial$estimators(list(est_glm(), est1 = est_adj()), .reset = TRUE)
  expect_equal(trial$estimators(), list(est2 = est_glm(), est1 = est_adj()))

  # previously set unnamed estimator can be updated
  trial$estimators(est2 = est_adj())
  expect_equal(trial$estimators()[["est2"]], est_adj())

  # a single unnamed argument cannot be used to update a previously set
  # unnamed estimator
  trial$estimators(est_glm())
  expect_equal(names(trial$estimators()), def_name(c(2, 1, 3)))

  # verify that setting or updating estimators will flush estimates attribute
  trial$estimators(est_glm(), .reset = TRUE)
  trial$estimates <- 3

  # reset will flush
  trial$estimators(.reset = TRUE)
  expect_null(trial$estimates)

  # update will flush
  trial$estimators(est1 = est_glm())
  trial$estimates <- 3
  trial$estimators(est1 = est_adj())
  expect_null(trial$estimates)

  # adding estimator will flush
  trial$estimates <- 3
  trial$estimators(est2 = est_adj())
  expect_null(trial$estimates)

  # getting estimator won't flush
  trial$estimates <- 3
  expect_equal(trial$estimators("est1"), est_adj())
  expect_equal(trial$estimates, 3)
}
test_estimators()

test_carts_modelargs <- function() {
  # more tests for setting model.args. Here we are testing that the methods
  # simulate, estimate_power and estimate_samplesize are responding to changes
  # made by the `args_model` method (in case future implementations of these
  # methods will directly use the model.args and not only indirectly through
  # calls to the simulate method)
  covar <- function(n) data.frame(a = rbinom(n, 1, 0.5))
  outcome <- function(data, ate = 0) with(data, rnorm(nrow(data), a * ate))
  trial <- Trial$new(
    estimators = est_glm(),
    outcome = outcome,
    covariates = covar
    )

  trial$args_model(ate = 100)
  d <- trial$simulate(n = 100)
  expect_equal(mean(d$y), 100, tolerance = 1)
  res <- trial$run(R = 10, n = 100)
  expect_equal(mean(res$estimates[[1]][, 1]), 100, tolerance = 1)
  expect_equal(trial$estimate_power(R = 10, n = 100), 1, tolerance = 0.1)
  val <- trial$estimate_samplesize(
    R = 100,
    refinement = 0,
    interval = 100,
    minimum = 10L,
    sa.control = list(niter = 10)
    )
  expect_equal(val$estimate_power, 1, tolerance = 0.1)

  trial$args_model(ate = 0)
  expect_equal(trial$estimate_power(R = 10, n = 100), 0.05, tolerance = 0.25)
}
test_carts_modelargs()

test_simulate <- function() {
  # test simulate method interface to lower level trial_simulate function
  covar <- function(n) data.frame(a = rbinom(n, 1, 0.5))
  outcome <- function(data, ate = 10) with(data, rnorm(nrow(data), a * ate))

  trial <- Trial$new(outcome = outcome, covariates = covar)
  n <- 100
  ate <- -10

  # default values of outcome models are used
  expect_equal(mean(trial$simulate(n = n)[a == 1]$y), 10, tolerance = 1e-1)

  # passing parameters to outcome function when calling simulate method.
  # trial$#model.args should not be updated
  dd <- trial$simulate(n = n, ate = ate)
  a <- NULL
  expect_equal(mean(dd[a == 1]$y), ate,  tolerance = 1e-1)
  expect_equal(trial$args_model(), list()) # model.args will be empty because
  # only arguments without default values are added upon initialization

  # setting model.args attribute of trial object
  trial$args_model(ate = ate)
  expect_equal(trial$args_model(), list(ate = ate))
  dd <- trial$simulate(n = n)
  expect_equal(mean(dd[a == 1]$y), ate,  tolerance = 1e-1)

  # raise error when sample size argument is missing
  expect_error(trial$simulate())

  # test that niter argument is passed to method implementation (trial_simulate)
  exclusion <- function(data) return(data[1, ])
  trial <- Trial$new(outcome = outcome, covariates = covar,
    exclusion = exclusion)
  # works as expected
  dd <- trial$simulate(n = 4, .niter = 5)
  expect_equal(nrow(dd), 4)
  # test that a) niter argument is passed to trial_simulate b) error message
  # is raised informing a user that the number of maximum iterations is reached
  expect_error(
    trial$simulate(n = 4, .niter = 2),
    pattern = "Maximum iterations reached"
  )

  # more tests to check output format
  trial <- Trial$new(
    covariates = function(n) data.frame(a = rbinom(n, 1, 0.5)),
    outcome = function(dd) rnorm(nrow(dd))
  )
  # check that data.frame returned by outcome model is converted to a
  # data.table
  dd <- trial$simulate(n = 100)
  expect_true(inherits(dd, "data.table"))
  expect_true(nrow(dd) == 100L)
  # check that simulate assigns id and default y column for outcome
  # + num column because covariates are passed
  expect_true(all(colnames(dd) %in% c("id", "num", "y", "a")))

  # allow overwriting default name of outcome column
  trial <- Trial$new(
    covariates = function(n) data.frame(a = rbinom(n, 1, 0.5)),
    outcome = function(dd) data.frame(yy = 2)
  )
  dd <- trial$simulate(n = 100)
  expect_true(all(colnames(dd) %in% c("id", "num", "yy", "a")))

  # expect error when trying to simulate data with duplicated columns.
  # using y as default column name when outcome model returns vector
  x0 <- function(n) data.frame(y = rnorm(n, 0, 10), xx = 1)
  outcome <- function(dd) rnorm(nrow(dd), -1)
  trial <- Trial$new(outcome, x0)
  expect_error(trial$simulate(5))
  # error is also raised for custom outcome names
  outcome <- function(dd) data.frame(xx = rnorm(nrow(dd), -1))
  trial <- Trial$new(outcome, x0)
  expect_error(trial$simulate(5))
}
test_simulate()

test_estimate_power <- function() {
  # test that summary.args are initialized correctly and used when estimating
  # power
  trial <- Trial$new(
    covariates = function(n) data.frame(a = rbinom(n, 1, 0.5)),
    outcome = function(data, val = -10) rnorm(nrow(data), data$a  * val),
    estimators = list(unadjusted = est_glm()),
    summary.args = list(alternative = "<", null = -100)
  )

  # no power because of null value that overwrites default value of 0
  expect_equal(trial$estimate_power(n = 100, R = 20), 0)

  # estimates field is populated upon calling estimate_power method
  expect_true(inherits(trial$estimates, "trial.estimates"))

  # power of 1 because of large negative effect size
  power <- trial$estimate_power(n = 100, R = 20, summary.args = list(null = 0))
  expect_equal(power, 1)

  # provided estimator argument takes precedence over set estimators
  # test also output format (named numeric vector when multiple estimators are
  # provided)
  power <- trial$estimate_power(n = 100, R = 20,
    estimators = list(e1 = est_glm(), e2 = est_glm())
  )
  expect_equal(power, c(e1 = 0, e2 = 0))

  # error is raised even though only one estimator produces an error
  expect_error(
    trial$estimate_power(n = 100, R = 20,
      estimators = list(e1 = est_glm(), e2 = \(d) stop("some error"))
    ),
    pattern = "Estimator 'e2' failed: some error"
  )

  # test that arguments to outcome model are passed on
  expect_error(
    trial$estimate_power(n = 100, R = 20, val = "sdf"),
    pattern = "non-numeric argument to binary operator"
  )
}
test_estimate_power()

test_run <- function() {
  trial <- Trial$new(
    covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
    outcome = \(d) rnorm(nrow(d), d$a * 5),
    estimators = list(est1 = est_glm())
  )
  res <- trial$run(n = 100, R = 10)

  # estimates field is successfully populated upon calling run method
  expect_equal(trial$estimates$estimates$est1, res$estimates$est1)

  # estimates attribute is flushed when changing the estimator (more tests
  # are implemented in test_args_model and test_estimators)
  trial$estimators(est1 = est_glm(), est2 = est_glm())
  expect_null(trial$estimates)

  # verify that estimates of both estimators are assigned to attribute
  trial$run(n = 100, R = 10)
  expect_equal(names(trial$estimates$estimates), c("est1", "est2"))

  # updating an existing estimator will also flush the estimates attribute
  trial$estimators(est1 = est_glm())
  expect_null(trial$estimates)
}
test_run()

test_summary <- function() {
  m <- Trial$new(outcome = function(data, p = c(0.5, 0.5), ...) {
    a <- rbinom(nrow(data), 1, 0.5)
    data.frame(a = a, y = rbinom(
      nrow(data), 1,
      p[1] * (1 - a) + p[2] * a
    ))
  }, estimators = list(mymodel = est_glm()))
  res <- m$run(n = 100, R = 500, p = c(0.5, 0.25))

  # test that summary method calculates descriptive statistic and power
  # correctly for the estimates of a given estimator
  s <- m$summary()
  expect_equal(mean(res$estimates[[1]][, "Estimate"]), s[1, "estimate"])
  expect_equal(mean(res$estimates[[1]][, "Std.Err"]), s[1, "std.err"])
  expect_equal(sd(res$estimates[[1]][, "Estimate"]), s[1, "std.dev"])
  expect_equal(mean(res$estimates[[1]][, "P-value"] < 0.05), s[1, "power"])

  p1 <- power.prop.test(n = 50, p1 = 0.5, p2 = 0.25) # n = obs. per group
  expect_equal(p1$power, s[1, "power"], tolerance = 0.2)

  s2 <- m$summary(true.value = -.25, alternative = "<", level = 0.05)
  p2 <- power.prop.test(n = 50, p1 = 0.5, p2 = 0.25, alternative = "one.sided")
  expect_equal(p2$power, s2[1, "power"], tolerance = 0.1)
  expect_true(s[1, "power"] < s2[1, "power"])

  # test that nominal coverage around true.value is correctly calculated
  s3 <- m$summary(true.value = -.25, nominal.coverage = 0.9)
  expect_equal(s3[, "coverage"], 0.9, tolerance = 0.05)
  s3 <- m$summary(true.value = -.25, nominal.coverage = 0.5)
  expect_equal(s3[, "coverage"], 0.5, tolerance = 0.05)

  # test ni.margin; expect power around 5% since true value is -0.25
  s <- m$summary(ni.margin = -.25, alternative = ">")
  expect_equal(s[1, "power"], 0.05, tolerance = 0.1)

  # power should go up to 10% when changing significance level to .1
  # implicitly verify that alternative hypothesis is derived correctly from
  # the margin
  s <- m$summary(ni.margin = -.25, level = 0.1)
  expect_equal(s[1, "power"], 0.1, tolerance = 0.1)

  # test null value argument. power should equal significance level since
  # true value equals null hypothesis
  s <- m$summary(null = -.25, alternative = ">", level = 0.05)
  expect_equal(s[1, "power"], 0.05, tolerance = 0.1)

  # summary only supports !=, < and > for alternative argument
  expect_error(
    m$summary(alternative = "greater"),
    pattern = 'alternative should be one of "!=", "<", ">"'
  )

  # test that error occurs when no estimates are available
  m_new <- Trial$new(
    outcome = function(data) data.frame(y = rnorm(nrow(data))),
    estimators = list(mymodel = est_glm())
  )
  expect_error(m_new$summary(), "No estimates available")

  # test that summary works as expected for estimates from more than one
  # estimator
  res1 <- m$run(n = 100, R = 10, p = c(0.5, 0.25),
    estimators = list(est1 = est_glm(), est2 = est_glm()))
  s <- m$summary()
  expect_equal(rownames(s), c("est1", "est2"))

  # Add new test block for estimates parameter
  # Test that providing estimates directly works the same as using stored estimates
  res2 <- m$run(n = 100, R = 500, p = c(0.5, 0.25))
  s1 <- m$summary()
  s2 <- m$summary(estimates = res2)
  expect_equal(s1, s2)

  # Test that providing estimates doesn't modify the object's stored estimates
  original_estimates <- m$estimates
  m$summary(estimates = res2)
  expect_equal(m$estimates, original_estimates)

  # Test that estimates parameter takes precedence over stored estimates
  different_res <- m$run(n = 200, R = 500, p = c(0.5, 0.25))  # Different n
  s3 <- m$summary(estimates = different_res)
  expect_false(identical(s1, s3))  # Should be different due to different n
}
test_summary()
