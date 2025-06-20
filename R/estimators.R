#' @title Construct estimator for the treatment effect in RCT
#' @description Returns an estimator for the estimation of a treatment effect
#' with parametric models ([stats::glm] and [MASS::glm.nb]). The returned
#' estimator is a function with a single argument (data) and returns the
#' treatment effect estimate, which is estimated with [lava::estimate]
#' @param response (character) Response variable
#' @param treatment (character) Treatment variable
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
est_glm <- function(response = "y",
                    treatment = "a",
                    covariates = NULL,
                    offset = NULL,
                    id = NULL,
                    level = 0.95,
                    family = gaussian(),
                    target.parameter = treatment,
                    ...) {
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

# TODO: fix or remove @details. we definitely need more information in
# details/examples
#' @description Efficient estimator of the treatment effect based on the
#'   efficient influence function. This involves a model for the conditional
#'   mean of the outcome variable given covariates.
#' @details ...
#' @title Construct estimator for the treatment effect in RCT based on covariate
#'   adjustment
#' @param response Response variable (character)
#' @param treatment Treatment variable (character)
#' @param covariates Optional covariates (character)
#' @param offset Optional offset (character)
#' @param id Optional subject id variable (character)
#' @param family Exponential family (default gaussian)
#' @param level Confidence interval level
#' @param treatment.effect (optional) function describing treatment effect given
#'   mean potential outcomes. Default is the average treatment effect
#'   (difference in expected outcome, (x,y) -> (y-x)) unless the family is
#'   binomial or poisson, in which case the treatment effect is the difference
#'   in log expected outcomes (x,y) -> log(y/x).
#' @param nfolds Number of folds for estimating the conditional average
#' treatment effect with double machine learning.
#' @param ... Additional arguments to the [ml_model] method
#' @return function
#' @seealso [Trial] [est_glm]
#' @author Klaus Kähler Holst
#' @export
est_adj <- function(response = "y",
                    treatment = "a",
                    covariates = NULL,
                    offset = NULL,
                    id = NULL,
                    family = gaussian(),
                    level = 0.95,
                    treatment.effect = NULL,
                    nfolds = 1,
                    ...) {
  if (inherits(response, "formula")) {
    response <- targeted::learner_glm(response, family = family)
  }
  if (!inherits(response, "ml_model")) {
    f <- make_formula(
      response = response, treatment = treatment,
      covariates = covariates, treatment.interaction = TRUE, offset = offset
    )
    response <- targeted::learner_glm(f, family = family, ...)
  }
  nam <- "treatment effect"
  if (is.null(treatment.effect)) treatment.effect <- "absolute"
  if (is.character(treatment.effect)) {
    if (tolower(treatment.effect) %in% c("rel", "relative", "rr", "logrr")) {
      treatment.effect <- function(x) log(x[2] / x[1])
      nam <- "logRR"
    } else if (tolower(treatment.effect) %in% c("or", "logor")) {
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
#' @export
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
  est <- with(ce, c(
    "E[Y(0)]" = mean(scores[["0"]]),
    "E[Y(1)]" = mean(scores[["1"]])
  ))
  IFs <- with(ce, cbind(
    scores[["0"]] - mean(scores[["0"]]),
    scores[["1"]] - mean(scores[["1"]])
  ))
  e <- lava::estimate(coef = est, IC = IFs)
  return(e)
}
