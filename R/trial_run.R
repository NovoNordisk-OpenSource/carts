trial_run <- function(self, .private, n, R, estimators, ...) {
  if (is.null(estimators)) estimators <- self$estimators()

  if (is.function(estimators)) { # User only provided a single estimator
    estimators <- list(estimators)
  }
  if (is.null(names(estimators))) {
    names(estimators) <- paste0("est", seq_along(estimators))
  }

  sim.args <- c(list(...), list(n = n))
  samp <- do.call(self$simulate, sim.args) |> head()

  # Test run for error checking
  test_estimators(self, sim.args, estimators)

  res <- lava::sim( # MC simulation wrapper
    x = run1trial,
    R = R,
    args = list( # args passed to run1trial
      self = self, estimators = estimators, sim.args = sim.args
  ))
  est_names <- names(estimators)
  resl <- list()
  # Place results of each estimator in a list of matrices and remove the
  # estimator-name from the column names again
  for (m in est_names) {
    pattern <- paste0("^", m, ":")
    idx <- grep(pattern, colnames(res))
    res0 <- res[, idx, drop = FALSE]
    colnames(res0) <- gsub(pattern, "", colnames(res0))
    resl <- c(resl, list(res0))
  }
  names(resl) <- est_names

  # combine arguments to passed to Trial$run() call and previously set arguments
  # via args_model(). Trial$simulate() takes care of it, thus we don't need to
  # do it before the lava::sim call
  sim.args.call <- self$args_model()
  sim.args.call[names(sim.args)] <- sim.args
  obj <- structure(
    list(
      model = self, estimates = resl,
      sample.data = samp,
      estimators = estimators, sim.args = sim.args.call,
      R = R
    ),
    class = "trial.estimates"
  )
  if (!.private$state$estimate_samplesize) {
    self$estimates <- obj
  }
  return(invisible(obj))
}

# @title Simulate a single clinical trial and perform statistical analysis
# @inheritParams trial_run
# @param sim.args (list) Additional calling arguments to [Trial$run()][Trial]
# and sample size
run1trial <- function(self, sim.args, estimators) {
  data <- do.call(self$simulate, sim.args)
  est <- lapply(estimators, function(f) {
    res <- f(data)
    if (inherits(res, "estimate")) {
      res <- lava::parameter(res)[1, , drop = TRUE]
    }
    return(res)
  })
  nam <- lapply(est, names)
  for (i in seq_along(nam)) {
    nam[[i]] <- paste0(paste0(names(nam)[[i]], ":"), nam[[i]])
  }
  res <- unlist(est)
  names(res) <- unlist(nam)
  return(res)
}

test_estimators <- function(self, sim.args, estimators) {
  data <- do.call(self$simulate, sim.args)
  # Test all estimators using lapply and tryCatch
  results <- lapply(names(estimators), function(est_name) {
    tryCatch({
      res <- estimators[[est_name]](data)
      if (inherits(res, "estimate")) {
        res <- lava::parameter(res)[1, , drop = TRUE]
      }
      return(NULL)  # Success case
    }, error = function(e) {
      return(sprintf("Estimator '%s' failed: %s", est_name, e$message))
    })
  })
  # Filter out NULL results (successful runs) and keep error messages
  errors <- unlist(results[!sapply(results, is.null)])
  # If there are any errors, throw them all at once
  if (length(errors) > 0) {
    stop(paste(errors, collapse = "\n"), call. = FALSE)
  }
  return(invisible())
}

format_sim_arg <- function(arg, level = 1) {
  # basic types we want to print directly
  basic_funs <- c(is.numeric, is.character, is.integer, is.logical)
  if (is.null(arg)) {
    return("NULL")
  } else if (inherits(arg, "family")) {
    return(paste0(arg$family[1], " <family>"))
  } else if (is.matrix(arg)) {
    return("<matrix>")
  } else if (is.array(arg)) {
    return("<array>")
  } else if (any(sapply(basic_funs, \(f) f(arg)))) {
    return(arg)
  } else {
    return(paste0("<", class(arg)[[1]], ">"))
  }
}

#' @export
print.trial.estimates <- function(x, ...) {
  cat_header("trial.estimates")
  model_args <- x$sim.args[!names(x$sim.args) %in% c("n")]
  args_str <- paste(
    mapply(
      function(name, value) paste0(name, " = ", format_sim_arg(value)),
      names(model_args),
      model_args
    ),
    collapse = ", "
  )
  cat("Model arguments:", args_str)
  cat("\n")
  cat("Estimators: ")
  cat(paste(names(x$estimates), collapse = ", "))
  cat("\n")
  cat("Simulation parameters:", paste0("n = ", x$sim.args$n, ", R = ", x$R))
  cat("\n\nSample data:\n")
  print(x$sample.data)
}

#' @export
#' @description [Deprecated] Summary method for the Monte Carlo studies of
#'   different estimators for the treatment effect in a randomized clinical
#'   trial. Please use Trial$summary() instead.
#' @title Summary method for simulated trials (trial.estimates objects)
#' @param object (trial.estimates)
#' @param level significance level
#' @param null null hypothesis to test
#' @param ni.margin non-inferiority margin
#' @param alternative alternative hypothesis (not equal !=, less <, greater >)
#' @param reject.function Optional function calculating whether to reject the
#'   null hypothesis. Should be a function of one or more of the following
#'   arguments: estimate (parameter estimate), stderr (Standard error of the
#'   estimate), lower (lower confidence limit), upper (upper confidence limit)
#' @param ... additional arguments to lower level functions
#' @param true.value Optional true parameter value. When provided the root-mean
#'   square error (RMSE) and coverage of the estimated confidence limits will
#'   also be calculated.
#' @param nominal.coverage Width of confidence limits used to calculate the
#' empirical coverage around `true.value`. The default value calculates the
#' coverage for the central 90% confidence interval.
#' @return matrix with results of each estimator stored in separate rows
summary.trial.estimates <- function(object,
                              level = .05,
                              null = 0,
                              ni.margin = NULL,
                              alternative = c("!=", "<", ">"),
                              reject.function = NULL,
                              true.value = NULL,
                              nominal.coverage = 0.9,
                              ...) {
  .Deprecated(
    "Trial$summary",
    msg = paste("summary.trial.estimates is deprecated.",
                "Use Trial$summary() instead.")
  )
  m <- Trial$new(
    outcome = object$outcome,
    covariates = object$covariates,
    exclusion = object$exclusion,
    estimators = object$estimators,
    info = object$info
  )
  m$estimates <- object
  args <- list(
    level = level,
    null = null,
    ni.margin = ni.margin,
    alternative = alternative,
    true.value = true.value,
    nominal.coverage = nominal.coverage
  )
  # Only add reject.function to args if it's not NULL
  if (!is.null(reject.function)) {
    args$reject.function <- reject.function
  }
  do.call(m$summary, c(args, list(...)))
}

#' @title trial.estimates class object
#'
#' @description [Trial$run()][Trial] returns an S3 class object
#' `trial.estimates`. The object contains all information to reproduce the
#' estimates as shown in the example.
#' The object is a list with the following components:
#' \describe{
#'  \item{model}{[Trial] object used to generate the estimates.}
#'  \item{estimates}{(list) Estimates of Monte-Carlo runs for each of the
#'  estimators.}
#'  \item{sample.data}{(data.table) Sample data returned from
#'    [Trial$simulate()][Trial].}
#'  \item{estimators}{ (list) Estimators applied to simulated data in each
#'  Monte-Carlo run.}
#'  \item{sim.args}{ (list) Arguments passed on to [Trial$simulate()][Trial]
#'  when simulating data in each Monte-Carlo run.}
#'  \item{R}{ (numeric) Number of Monte-Carlo replications.}
#' }
#'
#' @section S3 generics:
#' The following S3 generic functions are available for an object
#' of class \code{trial.estimates}:
#' \describe{
#'   \item{\code{print}}{Basic print method.}
#'   \item{\code{summary}}{Apply decision-making to estimates of each run and
#'   estimator.}
#'  }
#' @aliases trial.estimates-class
#' @examples
#' trial <- Trial$new(
#'   covariates = function(n) data.frame(a = rbinom(n, 1, 0.5)),
#'   outcome = function(data) rnorm(nrow(data), data$a * -1)
#'  )
#' res <- trial$run(n = 100, R = 10, estimators = est_glm())
#' print(res)
#'
#' # assuming previous estimates have been saved to disk.
#' # load estimates object and repeat simulation with more Monte-Carlo runs
#' res2 <- do.call(
#'   res$model$run,
#'   c(list(R = 20, estimators = res$estimators), res$sim.args)
#' )
#' print(res2)
#' @docType class
#' @name trial.estimates-class
NULL
