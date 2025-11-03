#' @title Calculate linear predictor from covariates
#' @description Calculate linear predictor \deqn{\text{par}^\top X} where
#'   \eqn{X} is the design matrix specified by the formula
#' @param data covariate matrix
#' @param mean formula specifying design from 'data' or a function that maps x
#'   to the mean value. If NULL all main-effects of the covariates will be used
#' @param par Regression coefficients (default zero). Can be given as a named
#'   list corresponding to the column names of `model.matrix`
#' @param model Optional model object ([glm], [mets::phreg], ...)
#' @param offset Optional offset variable name
#' @param treatment Optional name of treatment variable
#' @param intercept When FALSE the intercept will removed from the design matrix
#' @param default.parameter when `model` and `treatment` is specified,
#'   interaction terms between `treatment` and all other covariates in `model`
#'   is added to the simulation model. `default.parameter` specifies the default
#'   parameter of these extra parameters which can be changed individually with
#'   the `par` argument.
#' @param family family (default 'gaussian(identity)'). The inverse
#'   link-function is used to map the mean to the linear predictor scale (if
#'   mean is given as a function)
#' @param remove variables that will be removed from input data (if formula is
#'   not specified)
#' @param ... Additional arguments passed to `mean` function
#' @return data.table
#' @seealso [outcome_count] [outcome_binary] [outcome_continuous]
#'   [outcome_phreg]
#' @export
outcome_lp <- function(data,
                       mean = NULL,
                       par = NULL,
                       model = NULL,
                       offset = NULL,
                       treatment = NULL,
                       intercept = TRUE,
                       default.parameter = 0,
                       family = gaussian(),
                       remove = c("id", "num"),
                       ...) {
  # If fitted model object is supplied
  if (!is.null(model)) {
    des <- designmatrix(
      model = model,
      data = data,
      treatment = treatment,
      offset = offset,
      intercept = intercept
    )
    X <- des$X
    p <- update_parameter(X = X, default.parameter = default.parameter)
    coefs <- coef(model)
    idx <- which(names(coefs) %in% colnames(X))
    if (length(idx) > 0) {
      p[names(coefs)[idx]] <- coefs[idx]
    }
    idx <- match(names(p), names(par)) |> na.omit()
    if (length(idx) > 0) {
      par <- par[idx]
      p[names(par)] <- unlist(par)
    }
    lp <- as.vector(X %*% p)
    if (!is.null(des$offset)) lp <- lp + des$offset
    return(structure(lp, family = family, par = p))
  }
  if (is.null(mean)) {
    # Specify linear predictor structure from columns of data
    remove <- intersect(colnames(data), remove)
    mean <- paste0("~ .")
    if (length(remove) > 0) {
      mean <- paste(
        mean, "-",
        paste(remove, collapse = "-")
      )
    }
    mean <- as.formula(mean)
  }
  family <- get_family(family)
  g <- family$linkfun
  if (inherits(mean, "formula")) {
    tt <- terms(mean, data = as.data.frame(data))
    X <- model.matrix(tt, data = as.data.frame(data))
    if (!intercept && attr(tt, "intercept")) {
      X <- X[, -1, drop = FALSE]
    }
    par <- update_parameter(par, X, default.parameter = default.parameter)
    lp <- X %*% par
  } else {
    if (!is.function(mean)) stop("Expecting a function or a formula")
    lp <- g(add_dots(mean)(data, ...))
    if (is.data.table(lp)) {
      lp <- lp[[1]]
    }
  }
  lp <- as.vector(lp)
  return(structure(lp, family = family, par = par))
}

#' @title Simulate from count model given covariates
#' @description Simulate from count model with intensity \deqn{\lambda =
#'   \text{exposure-time}\exp(\text{par}^\top X)} where \eqn{X} is the design
#'   matrix specified by the formula
#' @param data covariate matrix
#' @param mean formula specifying design from 'x' or a function that maps x to
#'   the mean value. The response variable needs to be left undefined, i.e. `~
#'   x1 + x2` defines the mean on the link-scale as a linear function of
#'   covariates x1 and x2 (see examples). If NULL all main-effects of the
#'   covariates will be used.
#' @param par Regression coefficients (default zero). Can be given as a named
#'   list corresponding to the column names of `model.matrix`
#' @param outcome.name Name of outcome variable ("y")
#' @param exposure Exposure times. Either a scalar, vector or function.
#' @param remove Variables that will be removed from input x (if formula is not
#'   specified)
#' @param zero.inflation vector of probabilities or a function of the covariates
#'   'x' including an extra column 'rate' with the rate parameter.
#' @param overdispersion variance of gamma-frailty either given as a numeric
#'   vector or a function of the covariates 'x' with an extra column 'rate'
#'   holding the rate parameter 'rate'
#' @param ... Additional arguments passed to lower level functions
#' @seealso [outcome_binary] [outcome_continuous] [outcome_lp]
#' @return data.table with added exposure column
#' @examples
#' set.seed(24)
#' covariates <- function(n) data.frame(a = rbinom(n, 1, 0.5))
#' outcome <- setargs(
#'   outcome_count,
#'   mean = ~ 1 + a,
#'   par = log(c(2.5, 0.65)),
#'   overdispersion = 1 / 2,
#'   exposure = 2 # identical exposure time for all subjects
#' )
#' trial <- Trial$new(covariates = covariates, outcome = outcome)
#' trial$simulate(5)
#' # alternating exposure times between subjects
#' trial$args_model(exposure = c(1, 2))
#' trial$simulate(5)
#' # treatment-dependent exposure times
#' trial$args_model(exposure = function(dd) 1 - 0.5 * dd$a)
#' trial$simulate(5)
#' @export
outcome_count <- function(data,
                          mean = NULL,
                          par = NULL,
                          outcome.name = "y",
                          exposure = 1,
                          remove = c("id", "num"),
                          zero.inflation = NULL,
                          overdispersion = NULL,
                          ...) {
  lp <- outcome_lp(data,
    mean = mean,
    par = par,
    remove = remove,
    family = poisson(log),
    ...
  )
  ## Gamma frailty
  rate <- add_frailty(
    rate = attr(lp, "family")$linkinv(lp), data = data,
    overdispersion = overdispersion
  )
  if (is.function(exposure)) exposure <- add_dots(exposure)(data, ...)

  exposure <- rep(exposure, length.out = nrow(data))
  res <- cbind(rpois(nrow(data), rate * exposure)) *
    draw_zero_inflation(cbind(rate = rate, x = data),
      zero.inflation = zero.inflation
    )
  colnames(res) <- outcome.name
  return(structure(
    cbind(data.table(res),
      exposure = exposure
    ),
    par = attr(lp, "par")
  ))
}

#' @title Simulate from binary model given covariates
#' @description Simulate from binary model with probability \deqn{\pi =
#'   g(\text{par}^\top X)} where \eqn{X} is the design matrix specified by the
#'   formula, and \eqn{g} is the link function specified by the family argument
#' @param data covariate matrix
#' @param mean formula specifying design from 'data' or a function that maps x
#'   to the mean value (see examples). If NULL all
#'   main-effects of the covariates will be used.
#' @param par Regression coefficients (default zero). Can be given as a named
#'   list corresponding to the column names of `model.matrix`
#' @param outcome.name Name of outcome variable ("y")
#' @param remove variables that will be removed from input x (if formula is not
#'   specified)
#' @param family exponential family (default `binomial(logit)`)
#' @param ... Additional arguments passed to `mean` function (see examples)
#' @return data.table
#' @seealso [outcome_count] [outcome_lp] [outcome_continuous]
#' @export
#' @examples
#' trial <- Trial$new(
#'   covariates = \(n) data.frame(a = rbinom(n, 1, 0.5)),
#'   outcome = outcome_binary
#' )
#' est <- function(data) glm(y ~ a, data = data, family = binomial(logit))
#' trial$simulate(1e4, mean = ~ 1 + a, par = c(1, 0.5)) |> est()
#'
#' # default behavior is to set all regression coefficients to 0
#' trial$simulate(1e4, mean = ~ 1 + a) |> est()
#'
#' # intercept defaults to 0 and regression coef for a takes provided value
#' trial$simulate(1e4, mean = ~ 1 + a, par = c(a = 0.5)) |> est()
#' # trial$simulate(1e4, mean = ~ 1 + a, par = c("(Intercept)" = 1))
#'
#' # define mean model that directly works on whole covariate data, incl id and
#' # num columns
#' trial$simulate(1e4, mean = \(x) with(x, lava::expit(1 + 0.5 * a))) |>
#'   est()
#'
#' # par argument of outcome_binary is not passed on to mean function
#' trial$simulate(1e4,
#'   mean = \(x,  reg.par) with(x, lava::expit(reg.par[1] + reg.par[2] * a)),
#'   reg.par = c(1, 0.8)
#' ) |> est()
outcome_binary <- function(data,
                           mean = NULL,
                           par = NULL,
                           outcome.name = "y",
                           remove = c("id", "num"),
                           family = binomial(logit),
                           ...) {
  lp <- outcome_lp(data,
    mean = mean,
    par = par,
    remove = remove,
    family = family,
    ...
  )
  h <- attr(lp, "family")$linkinv
  res <- data.table(rbinom(length(lp), 1, h(lp)))
  colnames(res) <- outcome.name
  return(res)
}

#' @title Simulate from continuous outcome model given covariates
#' @description Simulate from continuous outcome model with mean
#'   \deqn{g(\text{par}^\top X)} where \eqn{X} is the design matrix specified by
#'   the formula, and \eqn{g} is the link function specified by the family
#'   argument
#' @param data covariate matrix
#' @param mean formula specifying design from 'data' or a function that maps x
#'   to the mean value. The response variable needs to be left undefined, i.e.
#'   `~ x1 + x2` defines the mean as a linear function of covariates x1 and x2
#'   (see examples). If NULL all main-effects of the covariates will be used.
#' @param par (numeric) Regression coefficients (default zero). Can be given as
#'   a named vector corresponding to the column names of `model.matrix`.
#' @param sd (numeric) standard deviation of Gaussian measurement error
#' @param het Introduce variance hetereogeneity by adding a residual term
#'   \eqn{het \cdot \mu_x \cdot e}, where \eqn{\mu_x} is the mean given
#'   covariates and \eqn{e} is an independent standard normal distributed
#'   variable. This term is in addition to the measurement error introduced by
#'   the `sd` argument.
#' @param outcome.name Name of outcome variable ("y")
#' @param remove variables that will be removed from input x (if formula is not
#'   specified)
#' @param family exponential family (default `gaussian(identity)`)
#' @param ... Additional arguments passed to lower level functions
#' @return data.table
#' @seealso [outcome_count] [outcome_binary] [outcome_lp]
#' @export
#' @examples
#' trial <- Trial$new(
#'   covariates = \(n) data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n)),
#'   outcome = outcome_continuous
#' )
#' est <- function(data) glm(y ~ a + x, data = data)
#' trial$simulate(1e4, mean = ~ 1 + a + x, par = c(1, 0.5, 2)) |> est()
#'
#' # default behavior is to set all regression coefficients to 0
#' trial$simulate(1e4, mean = ~ 1 + a + x) |> est()
#'
#' # intercept defaults to 0 and regression coef for a takes provided value
#' trial$simulate(1e4, mean = ~ 1 + a, par = c(a = 0.5)) |> est()
#' # trial$simulate(1e4, mean = ~ 1 + a, par = c("(Intercept)" = 0.5)) |> est()
#'
#' # define mean model that directly works on whole covariate data, incl id and
#' # num columns
#' trial$simulate(1e4, mean = \(x) with(x, -1 + a * 2 + x * -3)) |>
#'   est()
#'
#' # par argument is not passed on to mean function
#' trial$simulate(1e4,
#'   mean = \(x,  reg.par) with(x, reg.par[1] + reg.par[2] * a),
#'   reg.par = c(1, 5)
#' ) |> est()
outcome_continuous <- function(data,
                               mean = NULL,
                               par = NULL,
                               sd = 1.0,
                               het = 0.0,
                               outcome.name = "y",
                               remove = c("id", "num"),
                               family = gaussian(),
                               ...) {
  lp <- outcome_lp(data,
    mean = mean,
    par = par,
    remove = remove,
    family = family,
    ...
  )
  h <- attr(lp, "family")$linkinv
  res <- h(lp) + rnorm(length(lp), sd = sd) +
    het * abs(lp) * rnorm(length(lp)) |>
      as.data.table()
  colnames(res) <- outcome.name
  return(res)
}

# TODO:  need more documentation if we aim to reuse this function across
# several projects
#' @title Outcome model for time-to-event end-points (proportional hazards)
#' @param data data.frame (covariates)
#' @param lp linear predictor (formula or function)
#' @param par optional list of model parameter
#' @param outcome.name names of outcome (time and censoring status)
#' @param remove variables that will be removed from input data (if formula is
#'   not specified)
#' @param model optional [mets::phreg] object
#' @param cens.model optional model for censoring mechanism
#' @param cens.lp censoring linear predictor argument (formula or function)
#' @param cens.par list of censoring model parameters
#' @param ... Additional arguments to [outcome_lp]
#' @return function (random generator)
#' @author Klaus Kähler Holst
#' @seealso [outcome_count] [outcome_lp] [outcome_binary] [outcome_continuous]
#' @export
#' @examples
#' library("survival")
#' data(pbc, package = "survival")
#' pbc0 <- na.omit(pbc) |>
#'   transform(trt = factor(trt, labels = c("Active", "Placebo")) |>
#'     relevel(ref = "Placebo"))
#'
#' fit1 <- mets::phreg(Surv(time, status > 0) ~ age + sex * trt, data = pbc0)
#'
#' covar <- covar_bootstrap(pbc0, subset = c("age", "sex")) %join%
#'   function(n, ...) data.frame(trt = sample(pbc0$trt, n, replace = TRUE))
#'
#' outcome <- setargs(
#'   outcome_phreg,
#'   model = fit1,
#'   par = list("trtActive" = 0)
#' )
#'
#' xx <- covar(5e3)
#' pbc1 <- outcome(xx) |> cbind(xx)
#' mets::phreg(formula(fit1), data = pbc1)
#'
#'
#' ## Introducing additional interactions
#' fit2 <- mets::phreg(Surv(time, status > 0) ~ age + sex + trt, data = pbc0)
#'
#' outcome <- setallargs(
#'   outcome_phreg,
#'   model = fit2,
#'   par = list("trtActive" = -.5, "age:trtActive" = 0),
#'   treatment = "trt",
#'   default.parameter = -0.2
#' )
#'
#' xx <- covar(1e4)
#' attr(outcome(xx), "par")
#' pbc1 <- outcome(xx) |> cbind(xx)
#' mets::phreg(Surv(time, status) ~ (age + sex) * trt, pbc1)
#' rm(pbc1, xx)
#'
outcome_phreg <- function(data,
                          lp = NULL,
                          par = NULL,
                          outcome.name = c("time", "status"),
                          remove = c("id", "num"),
                          model = NULL,
                          cens.model = NULL,
                          cens.lp = NULL,
                          cens.par = NULL,
                          ...) {
  timemod <- proc_phreg(model)
  model <- timemod$model
  if (is.null(cens.model)) {
    if (is.null(model)) stop(
        "Need 'phreg' object (argument 'model') or specify parametric model"
    )
    fcens <- with(model, Surv(time, !status) ~ 1)
    cens.model <- mets::phreg(fcens, data = model.frame(model))
    cens.lp <- ~fcens
  }
  censmod <- proc_phreg(cens.model)
  cens.model <- censmod$model
  if (any(is.na(data))) stop("Remove missing data from `data`")
  rr <- outcome_lp(data,
    mean = lp,
    par = par,
    remove = remove,
    intercept = FALSE,
    model = model,
    ...
  ) |> exp()
  # Censoring
  rr0 <- outcome_lp(data,
    mean = cens.lp,
    par = cens.par,
    remove = remove,
    intercept = FALSE,
    model = cens.model,
  ) |> exp()

  # Simulation
  event <- timemod$gen(rr)
  cens <- censmod$gen(rr0)
  obstime <- pmin(event$time, cens$time)
  status <- (event$time <= cens$time) * event$status
  res <- data.table(time = obstime, status = status)
  colnames(res) <- outcome.name
  return(structure(res, par = attr(rr, "par"), cens.par = attr(rr0, "par")))
}

#' @title EXPERIMENTAL: Outcome model for recurrent events with terminal
#' events end-points
#' @description This function is still in an experimental state where the
#' interface and functionality might change in the future
#' @param data data.frame (covariates)
#' @param lp linear predictor (formula or function)
#' @param par optional list of model parameter
#' @param outcome.name names of outcome (time and censoring status)
#' @param remove variables that will be removed from input data (if formula is
#'   not specified)
#' @param model optional [mets::phreg] object
#' @param death.model optional model for death (terminal) events
#' @param death.lp optional death linear predictor argument (formula or
#' function)
#' @param death.par optional list of death model parameters
#' @param cens.model optional model for censoring mechanism
#' @param cens.lp optional censoring linear predictor argument
#' (formula or function)
#' @param cens.par optional list of censoring model parameters
#' @param ... Additional arguments to [outcome_lp]
#' @return function (random generator)
#' @seealso [outcome_count] [outcome_lp] [outcome_binary] [outcome_continuous]
#' [outcome_phreg]
#' @export
outcome_recurrent <- function(data,
                              lp = NULL,
                              par = NULL,
                              outcome.name = c("time", "status"),
                              remove = c("id", "num"),
                              model = NULL,
                              death.model = NULL,
                              death.lp = NULL,
                              death.par = NULL,
                              cens.model = NULL,
                              cens.lp = NULL,
                              cens.par = NULL,
                              ...) {
  timemod <- proc_phreg(model)
  model <- timemod$model
  ## death model
  if (!is.null(death.model)) {
    deathmod <- proc_phreg(death.model)
    death.model <- deathmod$model
  }
  ## censoring model
  if (is.null(cens.model)) {
    if (!is.null(death.model)) { # use death model for censoring
      fcens <- with(death.model, Surv(time, !status) ~ 1)
      cens.model <- mets::phreg(fcens, data = model.frame(death.model))
      cens.lp <- ~fcens
    } else {
      # TODO: this cannot happen because proc_phreg fails for model = NULL.
      if (is.null(model)) stop(
        "Need 'phreg' object (argument 'model') or specify parametric model"
      )
      event_data_cens <- cbind(model.frame(model), exit = model$exit)
      cens_data <- event_data_cens[
        !duplicated(event_data_cens$cluster, fromLast = TRUE),
      ]
      cens_times <- cens_data$exit
      cens_data <- data.frame(
        time = cens_times,
        status = 1
      )
      cens.model <- mets::phreg(Surv(time, status) ~ 1, data = cens_data)
    }
  }
  censmod <- proc_phreg(cens.model)
  cens.model <- censmod$model
  ## drop na
  if (any(is.na(data))) stop("Remove missing data from `data`")
  xr <- outcome_lp(data,
    mean = lp,
    par = par,
    remove = remove,
    intercept = FALSE,
    model = model,
    ...
  ) |> exp()
  if (!is.null(death.model)) {
    dr <- outcome_lp(data,
      mean = death.lp,
      par = death.par,
      remove = remove,
      intercept = FALSE,
      model = death.model,
      ...
    ) |> exp()
  } else {
    dr <- NULL
  }
  rc <- outcome_lp(data,
    mean = cens.lp,
    par = cens.par,
    remove = remove,
    intercept = FALSE,
    model = cens.model,
  ) |> exp()
  n <- length(xr)

  res <- simRecurrent_with_censoring(n,
    cumhaz = model$cumhaz,
    death.cumhaz = death.model$cumhaz,
    r1 = xr,
    rd = dr,
    rc = rc,
    cens = cens.model$cumhaz
  )

  res <- data.table(
    id = res$id,
    start = res$start,
    stop = res$stop,
    status = res$status,
    fdeath = res$fdeath,
    death = res$death,
    rr = res$rr1
  )
  return(structure(res, par = attr(xr, "par"), death.par = attr(dr, "par")))
}

# Set up random generator for time-to-event outcome (ph model). 'model' can
# either be a named vector/list with a 'scale' and 'shape' argument in which
# case a weibull baseline hazard is used. Alternatively, a fitted phreg model
# can be used to simulate directly from the estimated cumulative hazard. This
# function is reused for both the time-to-event of interest and the censoring
# mechansim.
proc_phreg <- function(model) {
  if (inherits(model, c("list", "numeric"))) {
    # scale, shape for Weibull model
    model <- as.list(model)
    if (!all(c("scale", "shape") %in% names(model))) {
      stop("expecting 'scale' and 'shape' components")
    }
    rangen <- function(rr) {
      f <- with(
        model,
        lava::coxWeibull.lvm(scale = scale, shape = shape, param = 2)
      )
      return(data.frame(time = f(length(rr), mu = log(rr)), status = 1))
    }
    return(list(model = NULL, gen = rangen))
  }
  if (!inherits(model, "phreg")) stop("`phreg` object expected")
  return(
    list(model = model, gen = function(rr) mets::rchaz(model$cumhaz, rr))
  )
}

# Sets the parameter vector 'par' according to column names of the design
# matrix X. If par is NULL then a zero-vector (or default.parameter) is
# returned. If par is named list, then a zero-vector is returned except at the
# positions corresponding to the parameter names in the par list.
update_parameter <- function(par = NULL, X, default.parameter = 0) {
  pname <- colnames(X)
  if (is.null(par)) {
    par <- rep(default.parameter, ncol(X))
    names(par) <- pname
    return(par)
  }
  if (length(par) == ncol(X) && is.null(names(par))) {
    names(par) <- pname
    return(par)
  }
  parl <- as.list(par)
  intercept <- grep("^1$|intercept", names(parl))
  if (length(intercept) > 0) {
    names(parl)[intercept] <- "(Intercept)"
  }
  par <- rep(default.parameter, ncol(X))
  names(par) <- pname
  overlap <- intersect(names(parl), pname)
  if (length(overlap) > 0) {
    par[overlap] <- unlist(parl[overlap])
  }
  return(par)
}

# Calculate the design matrix from model object `model` and new data.frame
# `data`. If treatment is specified additional first order interactions between
# all the covariates in `model` and `treatment` are appended.
# This is a helper function for `outcome_lp`
designmatrix <- function(model,
                         data,
                         treatment = NULL,
                         offset = NULL,
                         intercept = TRUE) {
  tt <- tryCatch(terms(model), error = function(...) terms(model$formula)) |>
    delete.response()
  f <- formula(tt)
  if (!is.null(treatment)) {
    xx <- c(attr(tt, "term.labels"), treatment)
    f <- update(f, as.formula(paste(
      "~ . + (", paste(xx, collapse = "+"),
      ") :", treatment
    )))
  }
  if (!is.null(offset)) {
    f <- update(f, as.formula(paste(
      "~ . + offset(",
      paste(offset, collapse = "+"), ")"
    )))
  }
  mf <- stats::model.frame(f, data = data)
  X <- stats::model.matrix(mf, data = data)
  if (!intercept && attr(tt, "intercept")) {
    X <- X[, -1, drop = FALSE]
  }
  offs <- stats::model.offset(mf)
  return(list(X = X, offset = offs))
}

# Draw a bernoulli distributed RV with probability zero.inflation(x, ...)
draw_zero_inflation <- function(data, zero.inflation = NULL, ...) {
  if (!is.null(zero.inflation)) { # Mixture model
    pr <- zero.inflation
    if (is.function(zero.inflation)) {
      pr <- zero.inflation(data, ...)
    }
    u <- rbinom(nrow(data), 1, pr)
    return(u)
  }
  return(1)
}

# Add gamma-frailty term to rate with variance defined by
# overdispersion((rate,x))
add_frailty <- function(rate, data, overdispersion = NULL, ...) {
  if (!is.null(overdispersion)) { # Gamma-poisson / NB
    if (is.function(overdispersion)) {
      nu <- 1 / overdispersion(cbind(rate = rate, data), ...)
    } else {
      nu <- 1 / overdispersion
    }
    z <- rgamma(nrow(data), nu, nu)
    return(rate * z)
  }
  return(rate)
}

# This function is the same as implementation of simRecurrentII
# from mets package with cumhaz2 = NULL and censoring being
# implemented as a "phreg" model object.
# Note: it is possible to later add multiple recurrent
# event option and random-effects
simRecurrent_with_censoring <- function(n, cumhaz, death.cumhaz = NULL,
                                        r1 = NULL, rd = NULL, rc = NULL,
                                        gap.time = FALSE,
                                        max.recurrent = 100,
                                        dhaz = NULL, dependence = 0,
                                        var.z = 0.22, cor.mat = NULL,
                                        cens = NULL, ...) {
  status <- fdeath <- dtime <- NULL # to avoid R-check

  if (dependence == 0) {
    z <- z1 <- zd <- rep(1, n)
  } else if (dependence == 1) {
    z <- rgamma(n, 1 / var.z[1]) * var.z[1]
    z1 <- z
    zd <- z
  } else if (dependence == 2) {
    stdevs <- var.z^.5
    b <- stdevs %*% t(stdevs)
    covv <- b * cor.mat
    z <- matrix(rnorm(3 * n), n, 3)
    z <- exp(z %*% chol(covv))
    z1 <- z[, 1]
    zd <- z[, 3]
  } else if (dependence == 3) {
    z <- matrix(rgamma(3 * n, 1), n, 3)
    z1 <- (z[, 1]^cor.mat[1, 1] + z[, 2]^cor.mat[1, 2] + z[, 3]^cor.mat[1, 3])
    zd <- (z[, 1]^cor.mat[3, 1] + z[, 2]^cor.mat[3, 2] + z[, 3]^cor.mat[3, 3])
    z <- cbind(z1, zd)
  } else if (dependence == 4) {
    zz <- rgamma(n, 1 / var.z[1]) * var.z[1]
    z1 <- zz
    zd <- rep(1, n)
    z <- z1
  } else {
    stop("dependence 0-4")
  }

  if (is.null(r1)) r1 <- rep(1, n)
  if (is.null(rd)) rd <- rep(1, n)
  if (is.null(rc)) rc <- rep(1, n)

  cumhaz <- rbind(c(0, 0), cumhaz)

  ## extend cumulative for death to full range  of cause 1
  if (!is.null(death.cumhaz)) {
    out <- mets::extendCums(list(cumhaz, death.cumhaz), NULL)
    cumhaz <- out$cum1
    cumhazd <- out$cum2
  }
  max.time <- tail(cumhaz[, 1], 1)

  ### recurrent first time
  tall <- mets::rchaz(cumhaz, z1 * r1)
  tall$id <- 1:n

  ### death time simulated
  if (!is.null(death.cumhaz)) {
    timed <- mets::rchaz(cumhazd, zd * rd)
    tall$dtime <- timed$time
    tall$fdeath <- timed$status
    if (!is.null(cens)) {
      ctime <- mets::rchaz(cens, rc)$time # <- rexp(n)/(rc*cens)
      tall$fdeath[tall$dtime > ctime] <- 0
      tall$dtime[tall$dtime > ctime] <- ctime[tall$dtime > ctime]
    }
  } else {
    tall$dtime <- max.time
    tall$fdeath <- 0
    cumhazd <- NULL
    if (!is.null(cens)) {
      ctime <- mets::rchaz(cens, rc)$time # rexp(n)/(rc*cens)
      tall$fdeath[tall$dtime > ctime] <- 0
      tall$dtime[tall$dtime > ctime] <- ctime[tall$dtime > ctime]
    }
  }

  ### fixing the first time to event
  tall$death <- 0
  tall <- mets::dtransform(tall, death = fdeath, time > dtime)
  tall <- mets::dtransform(tall, status = 0, time > dtime)
  tall <- mets::dtransform(tall, time = dtime, time > dtime)
  tt <- tall

  ### setting aside memory
  i <- 1
  while (any((tt$time < tt$dtime) & (tt$status != 0) & (i < max.recurrent))) {
    i <- i + 1
    still <- subset(tt, time < dtime & status != 0)
    nn <- nrow(still)
    tt <- mets::rchaz(
      cumhaz,
      r1[still$id] * z1[still$id],
      entry = (1 - gap.time) * still$time
    )
    if (gap.time) {
      tt$entry <- still$time
      tt$time <- tt$time + still$time
    }
    ###
    tt <- cbind(
      tt,
      mets::dkeep(still, ~ id + dtime + death + fdeath),
      row.names = NULL
    )
    tt <- mets::dtransform(tt, death = fdeath, time > dtime)
    tt <- mets::dtransform(tt, status = 0, time > dtime)
    tt <- mets::dtransform(tt, time = dtime, time > dtime)
    tall <- rbind(tall, tt[1:nn, ], row.names = NULL)
  }
  mets::dsort(tall) <- ~ id + entry + time
  tall$start <- tall$entry
  tall$stop <- tall$time
  tall$rr1 <- tall$rr
  tall$rr <- NULL

  attr(tall, "death.cumhaz") <- cumhazd
  attr(tall, "cumhaz") <- cumhaz
  attr(tall, "z") <- z
  return(tall)
}
