#' @keywords internal
"_PACKAGE"

#' @title Simulation-based assessment of Covariate Adjustment in Randomized
#' Trials
#' @description Monte Carlo simulation framework for different randomized
#'   clinical trial designs with a special emphasis on estimators based on
#'   covariate adjustment.
#' @name carts-package
#' @importFrom stats glm poisson binomial gaussian rpois rnbinom pnbinom rbinom
#'   rgamma nlminb na.omit coef update qnorm model.offset model.frame as.formula
#'   formula delete.response predict terms model.matrix rnorm lm runif sd time
#' @importFrom targeted ml_model cv predictor_glm
#' @importFrom utils head tail str capture.output
#' @importFrom rlang abort call_match
#' @import data.table
#' @importFrom lava lvm sim estimate regression distribution
#' @importFrom logger log_info log_warn log_error
#' @importFrom survival survSplit Surv strata
#' @importFrom methods formalArgs
#' @importFrom R6 R6Class
#' @aliases carts-package carts
#' @keywords package
#' @useDynLib carts, .registration=TRUE
#' @author Benedikt Sommer, Klaus Holst, Foroogh Shamsi
#' @examples
#' rnb(10, 1, 1)
NULL
