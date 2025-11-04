library("tinytest")
set.seed(42)

test_trial_print <- function() {
  # simple checks to ensure that changes won't break print method
  trial <- Trial$new(
    covariates = function(n) data.frame(a = rbinom(n, 1, 0.5)),
    outcome = function(data, sd = 1) rnorm(nrow(data), data$a * -1, sd),
    estimators = list(marginal = est_glm()),
    info = "Some trial info"
  )
  trial$args_model(sd = 2)
  trial$args_summary(level = 0.025)

  # trial info, parameters and summary args are printed
  expect_stdout(print(trial), "Some trial info")
  expect_stdout(print(trial), "sd:  num 2")
  expect_stdout(print(trial), "level:  num 0.025")

  # covariates, outcome and exclusion model are printed
  expect_stdout(print(trial), "Outcome:\n function \\(data, sd = 1, ...\\)")
  expect_stdout(print(trial), "Covariates:\n function \\(n, ...\\)")
  expect_stdout(print(trial), "Exclusion:\n function \\(x, ...\\)")

  # estimators
  expect_stdout(print(trial), "Estimators:\\s*\n\\s*1\\.\\s*marginal")

  # verbose prints function body
  expect_stdout(
    print(trial, verbose = TRUE),
    "rnorm\\(nrow\\(data\\), data\\$a \\* -1, sd\\)"
  )
}
test_trial_print()

test_trial_run <- function() {
  m <- Trial$new(
    covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
    outcome = \(d) rnorm(nrow(d), d$a * 5),
    estimators = list(mymodel = est_glm())
  )

  d <- m$simulate(n = 100)
  # check that data.frame returned by outcome model is converted to a
  # data.table
  expect_true(inherits(d, "data.table"))
  expect_true(nrow(d) == 100L)
  expect_true(all(colnames(d) %in% c("id", "y", "a", "num")))

  res <- m$run(n = 100, R = 80)
  # Checking standard-out
  expect_stdout(print(res), "mymodel")
  expect_stdout(print(res), "n = 100")
  expect_stdout(print(res), "R = 80")

  expect_equal(res$sim.args$n, 100L) # 100 samples per MC replicate
  expect_equal(length(res$estimates), 1L) # only 1 estimator
  expect_equal(nrow(res$estimates[[1]]), 80L) # 80 MC replicates

  # test that .niter argument is passed on to simulate method
  exclusion <- function(data) return(data[1, ])
  trial <- Trial$new(
    outcome = \(d) data.frame(y = rnorm(nrow(d)), a = rbinom(nrow(d), 1, 0.5)),
    exclusion = exclusion,
    estimators = est_glm()
  )
  expect_error(
    trial$run(n = 5, R = 2, .niter = 2),
    pattern = "Either modify the exclusion criterion or increase"
  )

  failing_estimator <- list(est1 = \(d) stop("some error"))
  expect_error(
    m$run(n = 100, R = 5, estimators = failing_estimator),
    pattern = "Estimator 'est1' failed: some error"
  )

  multiple_failing <- list(
    est1 = \(d) stop("error 1"),
    est2 = \(d) stop("error 2"),
    est3 = est_glm()
  )

  expect_error(
    m$run(n = 100, R = 5, estimators = multiple_failing),
    pattern = "Estimator 'est1' failed: error 1\nEstimator 'est2' failed: error 2"
  )

}
test_trial_run()
