#' @title Construct estimator for the treatment effect in RCT
#' @description Returns an estimator for the estimation of a treatment effect
#' with parametric models ([stats::glm] and [MASS::glm.nb]). The returned
#' estimator is a function with a single argument (data) and returns the
#' treatment effect estimate, which is estimated with [lava::estimate]
#' @param response (character) Response variable
#' @param treatment (character) Treatment variable. Additional care must be
#' taken when the treatment variable is encoded as a factor (see examples).
#' @param covariates (character; optional) Single or vector of covariates
#' @param offset (character; optional) Model offset
#' @param id (character; optional) Subject id variable
#' @param level (numeric) Confidence interval level
#' @param family (family or character) Exponential family that is supported by
#' [stats::glm] and [MASS::glm.nb]
#' @param target.parameter (character) Target parameter from model output
#' @param ... Additional arguments to [lava::estimate]
#' @return function
#' @seealso [Trial]
#' @aliases est_glm est_gee est_geebin est_glmbin
#' @author Klaus Kähler Holst
#' @export
#' @examples
#' trial <- Trial$new(
#'     covariates = function(n) data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n)),
#'     outcome = setargs(outcome_count,
#'       mean = ~ 1 + a*x,
#'       par = c(1, -0.1, 0.5, 0.2),
#'       overdispersion = 2)
#' )
#' dd <- trial$simulate(3e2)
#'
#' # crude mean comparison between arms (default behavior; y ~ a)
#' est <- est_glm(family = poisson)
#' est(dd)
#'
#' # linear adjustment with one covariate (y ~ a + x)
#' est <- est_glm(family = poisson, covariates = "x")
#' est(dd)
#'
#' # return estimates of all linear coefficients (useful for debugging)
#' est <- est_glm(family = poisson, covariates = "x", target.parameter = NULL)
#' est(dd)
#'
#' # comparing robust and non-robust standard errors of poisson estimator by
#' # passing robust argument via ... to lava::estimate
#' estimators <- list(
#'   robust = est_glm(family = poisson),
#'   non.robust = est_glm(family = poisson, robust = FALSE)
#' )
#' res <- do.call(rbind, lapply(estimators, \(est) est(dd)$coefmat))
#' rownames(res) <- names(estimators)
#' res
#'
#' dd_factor <- dd
#' dd_factor$a <- as.factor(dd_factor$a)
#' # target parameter needs to be changed because the name of the estimated
#' # regression coefficient changes when encoding the treatment variable as a
#' # factor
#' est_glm(family = poisson, target.parameter = "a1")(dd_factor)
est_glm <- function(response = "y",
                    treatment = "a",
                    covariates = NULL,
                    offset = NULL,
                    id = NULL,
                    level = 0.95,
                    family = gaussian(),
                    target.parameter = treatment,
                    ...) {

  op <- options(
    contrasts = c(unordered = "contr.treatment", ordered = "contr.poly")
  )
  on.exit(options(op))
  formula <- make_formula(
    treatment = treatment, response = response,
    covariates = covariates, offset = offset
  )

  fun <- function(data) {
    data <- as.data.frame(data)
    if (is.character(family) && tolower(family) %in% c("nb", "negbin")) {
      g <- MASS::glm.nb(formula, data = data)
    } else {
      g <- glm(formula, data = data, family = family)
    }
    args <- list(g, level = level, keep = target.parameter, ...)
    if (!is.null(id)) args$id <- data[, id, drop = TRUE]
    e <- tryCatch(do.call(lava::estimate, args),
      error = function(...) {
        return(lava::estimate(
          coef = coef(g),
          vcov = NA, level = level,
          keep = target.parameter, ...
        ))
      }
    )
    return(e)
  }
  return(fun)
}

#' @export
est_gee <- function(..., id = "id") {
  return(est_glm(..., id = id))
}

#' @export
est_geebin <- function(..., id = "id") {
  return(
    est_glm(response = "I(y>0)", ..., id = id, family = binomial(logit))
  )
}

#' @export
est_glmbin <- function(...) {
  return(
    est_glm(response = "I(y>0)", ..., family = binomial(logit))
  )
}

#' @description Efficient estimator of the treatment effect based on the
#'   efficient influence function. This involves a model for the conditional
#'   mean of the outcome variable given covariates (Q-model).
#' @title Construct estimator for the treatment effect in RCT based on covariate
#'   adjustment
#' @param response (character, formula, [targeted::learner]) The default
#' behavior when providing a character is to use a [glm] with
#' treatment-covariate interactions for the Q-model. The covariates are
#' specified via the `covariates` argument, where the default behavior is to use
#' no covariates. When providing a formula, a [glm] is used for the Q-model,
#' where the design matrix is specified by the formula. The last option is to
#' provide a [targeted::learner] object that specifies the Q-model (see
#' examples).
#' @param treatment (character) Treatment variable. Additional care must be
#' taken when the treatment variable is encoded as a factor (see examples).
#' @param covariates (character) List of covariates. Only applicable when
#' `response` is a character.
#' @param offset (character) Optional offset to include in the [glm] model when
#' `response` is a character.
#' @param id (character) Subject id variable
#' @param family (family) Family argument used in the [glm] when `response` is a
#' character or formula.
#' @param level (numeric) Confidence interval level
#' @param treatment.effect (character, function) Default is the average
#'   treatment effect, i.e. difference in expected outcomes (x, y) -> x - y,
#'  with x = E\[Y(1)\] and y = E\[Y(0)\]). Other options are "logrr" (x, y) ->
#' log(x / y) ) and "logor" (x, y) -> log(x / (1 - x) * y / (1 - y)). A
#' user-defined function can alternatively be provided to target a population
#' parameter other than the absolute difference, log rate ratio or log odds
#' ratio (see details).
#' @param nfolds (integer) Number of folds for estimating the conditional
#' average treatment effect with double machine learning.
#' @param ... Additional arguments to [targeted::learner_glm] when `response` is
#' a character or formula.
#' @details The user-defined function for `treatment.effect` needs to accept a
#' single argument `x` of estimates of (E\[Y(1)\],E\[Y(0)\]). The estimates are
#' a vector, where the order of E\[Y(1)\] and E\[Y(0)\] depends on the encoding
#' of the `treatment` variable. E\[Y(0)\] is the first element when the
#' treatment variable is drawn from a Bernoulli distribution and kept as a
#' numeric variable or corresponds to the first level when the treatment
#' variable is encoded as a factor.
#' @return function
#' @seealso [Trial] [est_glm]
#' @author Klaus Kähler Holst
#' @export
#' @examples
#' trial <- Trial$new(
#'     covariates = function(n) data.frame(a = rbinom(n, 1, 0.5), x = rnorm(n)),
#'     outcome = setargs(outcome_count,
#'       mean = ~ 1 + a*x,
#'       par = c(1, -0.1, 0.5, 0.2),
#'       overdispersion = 2)
#' )
#' dd <- trial$simulate(1e4)
#'
#' # equivalent specifications to estimate log(E[Y(1)] / E[Y(0)])
#' estimators <- list(
#'   est_adj(family = poisson, treatment.effect = "logrr"),
#'   est_glm(family = poisson),
#'   est_adj(response = y ~ a, family = poisson, treatment.effect = "logrr"),
#'   est_adj(response = targeted::learner_glm(y ~ a, family = poisson),
#'     treatment.effect = "logrr"
#'   )
#' )
#' lapply(estimators, \(est) est(dd))
#'
#'
#' # now with covariates, estimating E[Y(1)] - E[Y(0)]
#' estimators <- list(
#'   est_adj(covariates = "x", family = poisson),
#'   est_adj(response = y ~ a * x, family = poisson),
#'   est_adj(response = targeted::learner_glm(y ~ a * x, family = poisson))
#' )
#' lapply(estimators, \(est) est(dd))
#'
#' # custom treatment.effect function
#' estimator <- est_adj(response = y ~ a * x, family = poisson,
#'   treatment.effect = \(x) x[2] - x[1] # x[1] contains the estimate of E[Y(0)]
#' )
#' estimator(dd)
#'
#' dd_factor <- dd
#' # when using factors, the control/comparator treatment needs to be the first
#' # level to estimate the contrasts defined by the `treatment.level` argument
#' estimator <- est_adj(response = y ~ a * x, family = poisson)
#' dd_factor$a <- factor(dd_factor$a, levels = c(0, 1))
#' estimator(dd_factor) # E[Y(1)] - E[Y(0)]
#'
#' dd_factor$a <- factor(dd_factor$a, levels = c(1, 0))
#' estimator(dd_factor) # E[Y(1)] - E[Y(0)]
est_adj <- function(response = "y",
                    treatment = "a",
                    covariates = NULL,
                    offset = NULL,
                    id = NULL,
                    family = gaussian(),
                    level = 0.95,
                    treatment.effect = "absolute",
                    nfolds = 1,
                    ...) {
  if (inherits(response, "formula")) {
    response <- targeted::learner_glm(response, family = family, ...)
  }
  if (!inherits(response, "learner")) {
    f <- make_formula(
      response = response, treatment = treatment,
      covariates = covariates, treatment.interaction = TRUE, offset = offset
    )
    response <- targeted::learner_glm(f, family = family, ...)
  }
  nam <- "treatment effect"
  if (is.character(treatment.effect)) {
    if (!(treatment.effect %in% c("absolute", "logrr", "logor"))) {
      rlang::abort(paste(
        'treatment.effect should be one of "absolute", "logrr",',
        '"logor" or a function')
      )
    }
    if (treatment.effect == "logrr") {
      treatment.effect <- function(x) log(x[2] / x[1])
      nam <- "logRR"
    } else if (treatment.effect == "logor") {
      treatment.effect <- function(x) log(x[2] / (1 - x[2]) * (1 - x[1]) / x[1])
      nam <- "logOR"
    } else {
      treatment.effect <- function(x) x[2] - x[1]
      nam <- "ATE"
    }
  }
  fun <- function(data, ...) {
    data <- as.data.frame(data)
    e <- adj1(response, data = data, treatment = treatment, nfolds = nfolds)
    if (!is.null(id)) {
      e <- lava::estimate(e, id = data[, id, drop = TRUE])
    }
    return(
      lava::estimate(e, treatment.effect, labels = nam, level = level)
    )
  }
  return(fun)
}

#' @title Marginal Cox proportional hazards model for the treatment effect in
#'   RCT
#' @param response Response variable (character or formula). Default:
#'   "Surv(time, status)"
#' @param treatment Treatment variable (character)
#' @param id Optional subject id variable (character)
#' @param level Confidence interval level
#' @return function
#' @seealso [Trial] [est_adj]
#' @author Klaus Kähler Holst
est_phreg <- function(response = "Surv(time, status)",
                      treatment = "a",
                      level = 0.95,
                      id = NULL) {
  if (!inherits(response, "formula")) {
    response <- stats::reformulate(treatment, response = response)
  }
  fun <- function(data, ...) {
    e <- mets::phreg(response, data = data)
    args <- list(level = level, x = e)
    if (!is.null(id)) args[["id"]] <- data[, id, drop = TRUE]
    return(do.call(lava::estimate, args))
  }
  return(fun)
}

make_formula <- function(treatment = "a",
                         response = "y",
                         covariates = NULL,
                         treatment.interaction = FALSE,
                         offset = NULL, ...) {
  formula <- paste(response, "~", treatment)
  if (!is.null(covariates)) {
    covar <- paste(covariates, collapse = " + ")
    if (treatment.interaction) {
      covar <- paste0(treatment, " * (", covar, ")")
    }
    formula <- paste(formula, "+", covar)
  }
  if (!is.null(offset)) formula <- paste0(formula, " + offset(", offset, ")")
  return(as.formula(formula))
}

adj1 <- function(qmodel, data, treatment = "a", nfolds = 1, ...) {
  f <- as.formula(paste0(treatment, "~ 1"))

  ce <- targeted::cate(f,
    silent = TRUE, nfolds = nfolds, ...,
    response.model = qmodel, propensity.model = f, data = data,
    mc.cores = 1
  )

  return(subset(ce, 2:1))
}
