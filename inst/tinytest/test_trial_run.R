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

test_summary.runtrials <- function() {
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
  s <- summary(res)
  expect_equal(mean(res$estimates[[1]][, "Estimate"]), s[1, "estimate"])
  expect_equal(mean(res$estimates[[1]][, "Std.Err"]), s[1, "std.err"])
  expect_equal(sd(res$estimates[[1]][, "Estimate"]), s[1, "std.dev"])
  expect_equal(mean(res$estimates[[1]][, "P-value"] < 0.05), s[1, "power"])

  p1 <- power.prop.test(n = 50, p1 = 0.5, p2 = 0.25) # n = obs. per group
  expect_equal(p1$power, s[1, "power"], tolerance = 0.2)

  s2 <- summary(res, true.value = -.25, alternative = "<", level = 0.05)
  p2 <- power.prop.test(n = 50, p1 = 0.5, p2 = 0.25, alternative = "one.sided")
  expect_equal(p2$power, s2[1, "power"], tolerance = 0.1)
  expect_true(s[1, "power"] < s2[1, "power"])

  # test that nominal coverage around true.value is correctly calculated
  s3 <- summary(res, true.value = -.25, nominal.coverage = 0.9)
  expect_equal(s3[, "coverage"], 0.9, tolerance = 0.05)
  s3 <- summary(res, true.value = -.25, nominal.coverage = 0.5)
  expect_equal(s3[, "coverage"], 0.5, tolerance = 0.05)


  # test ni.margin; expect power around 5% since true value is -0.25
  s <- summary(res, ni.margin = -.25, alternative = ">")
  expect_equal(s[1, "power"], 0.05, tolerance = 0.1)
  # power should go up to 10% when changing significance level to .1
  # implicitly verify that alternative hypothesis is derived correctly from
  # the margin
  s <- summary(res, ni.margin = -.25, level = 0.1)
  expect_equal(s[1, "power"], 0.1, tolerance = 0.1)

  # test null value argument. power should equal significance level since
  # true value equals null hypothesis
  s <- summary(res, null = -.25, alternative = ">", level = 0.05)
  expect_equal(s[1, "power"], 0.05, tolerance = 0.1)
  # test that alternative argument also works with "less" and "greater" values
  s <- summary(res, null = -.25, alternative = "greater", level = 0.05)
  s1 <- summary(res, null = -.25, alternative = ">", level = 0.05)
  expect_equal(s[1, "power"], s1[1, "power"])

  s <- summary(res, null = -.25, alternative = "less", level = 0.05)
  s1 <- summary(res, null = -.25, alternative = "<", level = 0.05)
  expect_equal(s[1, "power"], s1[1, "power"])

  # test that error occurs when estimates do not contain Estimate and Std.Err
  # columns
  res1 <- res
  res1$estimates[[1]] <- res1$estimates[[1]][, c("Estimate")]
  expect_error(summary(res1))

  # test that summary works as expected for estimates from more than one
  # estimator
  res1 <- m$run(n = 100, R = 10, p = c(0.5, 0.25),
    estimators = list(est1 = est_glm(), est2 = est_glm()))
  s <- summary(res1)
  expect_equal(rownames(s), c("est1", "est2"))
}
test_summary.runtrials()
