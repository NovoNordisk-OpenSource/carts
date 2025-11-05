#' @title Sample from an estimated parametric covariate model
#' @param n Sample size
#' @param model lava::lvm object with estimated coefficients
#' @param model.path Path to dumped model object (RDS file) on disk (optional)
#' @return data.table
#' @author Benedikt Sommer
#' @examples
#' data <- data.table::data.table(
#'   x = rnorm(1e3), y = as.factor(rbinom(1e3, size = 1, prob=0.5))
#' )
#'
#' m <- estimate_covar_model_full_cond(data)
#' samples <- sample_covar_parametric_model(n=10, model = m)
#' print(head(samples))
#' @export
sample_covar_parametric_model <- function(n, model = NULL, model.path = NULL) {
  if (!is.null(model.path)) {
    model <- readRDS(model.path)
  }
  data <- lava::sim(model, n)
  factor_levels <- attributes(model)$factor_levels
  for (factor.name in names(factor_levels)){
    factor.level <- factor_levels[[factor.name]]
    data[[factor.name]] <- as.factor(factor.level[data[[factor.name]] + 1])
  }
  return(data)
}

#' @title Sample from empirical distribution of covariate data
#' @param data data.frame
#' @param subset optional columns to select from data frame
#' @return random generator (function)
#' @export
covar_bootstrap <- function(data, subset = NULL) {
  if (!is.null(subset)) {
    if (inherits(subset, "formula")) {
      subset <- all.vars(subset)
    }
    data <- subset(data, select = subset)
  }
  return(
    function(n, ...) mets::dsample(data, size = n, replace = TRUE)
  )
}

#' @title Derive covariate distribution from covariate data type
#' @description Derive covariate distribution (for outcome regression) for
#' integers (Poisson), numeric (Gaussian) and binary (Binomial) data. Raise an
#' error for other data types.
#' @param covar Vector with covariates
#' @return lava package random generator function
#' @author Benedikt Sommer
#' @export
derive_covar_distribution <- function(covar) {
  if (is.factor(covar)) {
    if (data.table::uniqueN(covar) > 2) {
      stop("Categorical data with cardinality > 2 is not supported")
    }
    fun <- lava::binomial.lvm
  } else if (is.integer(covar)) {
    fun <- lava::poisson.lvm
  } else if (is.numeric(covar)) {
    fun <- lava::normal.lvm
  } else {
    stop(paste0("Data type '", class(covar), "' is not supported!"))
  }
  return(fun)
}

#' @title Full conditional covariate simulation model
#' @description Estimates a full conditional model to approximate the joint
#' distribution of covariate data. Each factor \eqn{p(x_i | x_1, \dots
#' x_{i-1})} is modelled with a `glm`, with mean \eqn{E[x_i | x_1, \dots
#' x_{i-1}] = g^{-1}(\beta_0 + \sum_{j=1}^{i-1}\beta_j x_j)}. The parametric
#' distribution of each factor is either derived from the column type (see
#' \code{\link{derive_covar_distribution}}) or specified by `cond.dist`.
#' @param data Covariate `data.table`
#' @param cond.dist `list` with random generator functions for the conditional
#' distribution of each covariate
#' @return lava::lvm object with estimated coefficients
#' @author Benedikt Sommer
#' @examples
#' data <- data.table::data.table(
#' y = as.factor(rbinom(1e3, size = 1, prob=0.1))
#' )
#'
#' # infer distribution of y from column type
#' m.est <- estimate_covar_model_full_cond(data)
#' y <- sample_covar_parametric_model(1e4, m.est)$y |> as.integer() - 1
#' print(mean(y))
#'
#' # specify distribution of y
#' m.est <- estimate_covar_model_full_cond(
#'   data, cond.dist = list(y = binomial.lvm)
#' )
#' y <- sample_covar_parametric_model(1e4, m.est)$y |> as.integer() - 1
#' print(mean(y))
#' @export
estimate_covar_model_full_cond <- function(data, cond.dist = NULL) {
  if (any(is.na(data))) {
    stop("NAs detected in data.")
  }

  data.cols <- colnames(data)
  # setup model with marginal distribution for first column in data. that is,
  # specify p(covar_1)
  covar.name <- data.cols[[1]]
  covar.dist <- ifelse(
    covar.name %in% names(cond.dist),
    yes = cond.dist[[covar.name]],
    no = derive_covar_distribution(data[[covar.name]])
  )

  m <- lava::lvm() |>
    regression(formula(paste(covar.name, "~ 1"))) |>
    distribution(formula(paste("~", covar.name)), covar.dist())

  # specify conditional distributions p(covar_i | covar_{i-1}, ... covar_{1})
  # in a forward fashion
  if (ncol(data) > 1) {
    for (i in seq(2, ncol(data))) {
      covar.name <- data.cols[[i]]
      # assume additive structure without interactions
      regres.expr <- paste(data.cols[seq_len(i-1)], collapse="+")
      m <- regression(m, formula(paste(covar.name, "~", regres.expr)))

      covar.dist <- ifelse(
        covar.name %in% names(cond.dist),
        yes = cond.dist[[covar.name]],
        no = derive_covar_distribution(data[[covar.name]])
      )
      m <- distribution(m, formula(paste("~", covar.name)), covar.dist())
    }
  }
  m.est <- lava::estimate(m, data, estimator = "glm")
  # instantiate new lava lvm object with estimated parameters. this allows
  # sampling from the model later on via sim(m, n = 50)
  m <- lava::sim(m, p = stats::coef(m.est))
  # factor levels needs to be derived here for sample_covar_parametric_model to
  # replace the sampled integers with the right factor levels.
  m.factor_levels <- get_factor_levels(data)
  return(structure(m, factor_levels = m.factor_levels))
}

#' @export
#' @title Simulate from a log gamma-gaussian copula distribution
#' @description Simulate from the logarithmic transform of a Gaussian copula
#'   model with compound symmetry correlation structure and with Gamma
#'   distributed marginals with mean one.
#' @param n Number of samples
#' @param gamma.var Variance of gamma distribution (n x p or 1 x p matrix)
#' @param normal.cor Correlation parameter (n x r) or (1 x r) matrix
#' @param type of correlation matrix structure (cs: compound-symmetry /
#'   exchangable, ar: autoregressive, un: unstructured, to: toeplitz). The
#'   dimension of `normal.cor` must match, i.e., for a Toeplitz correlation
#'   matrix r = p-1, and for a cs and ar r=1.
#' @param names Column name of the column vector (default "z")
#' @param ... Additional arguments passed to lower level functions
#' @details We simulate from the Gaussian copula by first drawing \eqn{X\sim
#'   N(0,R)} and transform the margins with \eqn{x\mapsto
#'   \log(F_\nu^{-1}\{\Phi(x)\})} where \eqn{\Phi} is the standard normal CDF
#'   and \eqn{F_\nu^{-1}} is the quantile function of the Gamma distribution
#'   with scale and rate parameter equal to \eqn{\nu}.
#' @return list of data.tables
#' @seealso [outcome_count] [Trial] [covar_normal]
covar_loggamma <- function(n,
                           normal.cor = NULL,
                           gamma.var = 1,
                           names = c("z"),
                           type = "cs",
                           ...) {
  inp <- par2cor(n, normal.cor, gamma.var, type = type)
  normal.cor <- inp$cor
  gamma.var <- inp$var
  p <- inp$p
  if (p == 1) {
    res <- data.table(
      rgamma(n,
        shape = 1 / gamma.var,
        rate = 1 / gamma.var
      )
    ) |> log()
    base::colnames(res) <- names
    return(res)
  }
  z <- rmvn(n, cor = normal.cor)
  for (i in seq_len(p)) {
    z[, i] <- stats::qgamma(stats::pnorm(z[, i]),
      shape = 1 / gamma.var[, i],
      rate = 1 / gamma.var[, i]
    )
  }
  res <- list()
  for (i in 1:p) {
    z1 <- log(z[, i, drop = FALSE])
    base::colnames(z1) <- names
    res <- c(res, list(data.table(z1)))
  }
  return(structure(res, names = seq_len(p)-1))
}

#' @export
#' @title Simulate from multivariate normal distribution
#' @description Simulate from MVN with compound symmetry variance structure and
#'   mean zero. The result is returned as a list where the ith element is the
#'   column vector with n observations from the ith coordinate of the MVN.
#' @inheritParams covar_loggamma
#' @param normal.var marginal variance (can be specified as a p-dim. vector or
#'   a nxp matrix)
#' @return list of data.tables
#' @seealso [outcome_count] [Trial] [covar_loggamma]
covar_normal <- function(n,
                         normal.cor = NULL,
                         normal.var = 1,
                         names = c("z"),
                         type = "cs",
                         ...) {
  inp <- par2cor(n, normal.cor, normal.var, type = type)
  normal.cor <- inp$cor
  normal.var <- inp$var
  p <- inp$p
  if (p == 1) {
    res <- data.table(
      rnorm(n, 0, sd = normal.var**.5)
    )
    base::colnames(res) <- names
    return(res)
  }
  z <- rmvn(n, cor = normal.cor)
  res <- list()
  for (i in 1:p) {
    z1 <- z[, i, drop = FALSE] * (normal.var[, i]^.5)
    base::colnames(z1) <- names
    res <- c(res, list(data.table(z1)))
  }
  return(structure(res, names = seq_len(p)-1))
}

#' @description For use with [Trial] objects, this function makes it
#'   possible to easily add additional covariates to an existing list of
#'   covariates (in the form of a data.frame or data.table).
#' @title Add additional covariates to existing list of covariates
#' @param covars list of covariates (data.frame's or data.table's)
#' @param x new covariates (function or list of functions/scalars)
#' @param names optional names of new covariates
#' @param ... additional arguments to function `x` or functions in `x`
#' @return matching format of covariates in `covars`
#' @author Klaus Kähler Holst
#' @examples
#' aaaaa
#' # adding "fixed" treatment indicator in each period
#' n <- 5
#' xt <- function(n, ...) {
#'  covar_loggamma(n, normal.cor = 0.2) |>
#'    covar_add(list(a = 0, a = 1))
#' }
#' xt(n)
#' # adding randomized treatment indicator
#' xt <- function(n, ...) {
#'  covar_loggamma(n, normal.cor = 0.2) |>
#'    covar_add(list(a = rbinom(n, 1, 0.5), a = rbinom(n, 1, 0.5)))
#' }
#' xt(5)
#' # adding baseline covariates
#' xt <- function(n, ...) {
#'  covar_loggamma(n, normal.cor = 0.2) |>
#'    covar_add(rnorm(n), names = "w1") |> # data
#'    covar_add(list(w2 = rnorm(n))) |> # data
#'    covar_add(data.frame(w3 = rnorm(n))) |> # data
#'    covar_add(\(n) data.frame(w4 = rnorm(n))) |> # function
#'    covar_add(\(n) rnorm(n), names = "w5") # function
#' }
#' xt(5)
#' @export
covar_add <- function(covars, x, names = NULL, ...) {
  if (!is.list(x)) x <- list(x)
  for (i in seq_along(x)) {
    if (is.function(x[[i]])) {
      x[[i]] <- add_dots(x[[i]])(nrow(covars[[i]]), ...)
    }
  }

  x <- rep(x, length.out = length(covars))
  for (i in seq_along(x)) {
    x[[i]] <- as.data.frame(x[i])
    if (!is.null(names)) base::names(x[[i]]) <- names
  }

  res <- list()
  for (i in seq_along(covars)) {
    res[[i]] <- cbind(covars[[i]], x[[i]])
  }
  base::names(res) <- base::names(covars)

  # do not allow duplicated column names because this may cause unintended
  # behavior downstream which is potentially difficult to detect
  stop_duplicated_covariates(covar = res[[1]])

  # disable disjoint column names across periods because Trial$simulate does not
  # support joining covariate data.tables with differing column names
  cols <- colnames(res[[1]])
  if (length(unique(unlist(Map(colnames, res)))) != length(cols)) {
    stop("Trying to add covariates with different names per period.")
  }

  return(res)
}

#' @export
`%join%` <- function(x, y) covar_join(x, y)

#' @export
#' @aliases join_covar %join%
#' @description For use with [Trial] objects, this function makes it possible to
#'   easily add additional covariates to an existing random generator
#'   (function(n ...) returning a data.frame or data.table)
#' @title Add additional covariates to existing covariate random generator
#' @param f covariate random generator
#' @param ... additional covariate generators or constant covariates
#' @return function, with returned data type matching that of `f`
#' @examples
#' # single period
#' n <- 5
#' c1 <- function(n) data.frame(a = rnorm(n))
#' c2 <- function(n) data.frame(b = rnorm(n))
#' x <- c1 %join% c2
#' x(n)
#'
#' # adding covariates that remain constant when sampling
#' x <- c1 %join% data.frame(b = rnorm(n))
#' all.equal(x(n)$b, x(n)$b)
#'
#' # adding multiple anonymous functions require parenthesis enclosing, with
#' # the exception of the last function
#' x <- c1 %join%
#'  (\(n) data.frame(b = rnorm(n))) %join%
#'  \(n) data.frame(c = rnorm(n))
#' x(n)
#'
#' # multiple periods
#' base <- setargs(covar_loggamma, normal.cor = .5)
#' x <- base %join%
#'   function(n) list(
#'       data.frame(a = rbinom(n, 1, 0.5)),
#'       data.frame(a = rbinom(n, 1, 0.5))
#'     )
#' x(n)
#'
#' # constant covariate
#' x <- base %join% list(data.frame(a = 0), data.frame(a = 1))
#' x(n)
#'
#' # baseline covariate
#' x <- base %join% function(n) data.frame(w = rnorm(n))
#' x(n)
covar_join <- function(f, ...) { #nolint (cyclomatic complexity)
  objects <- list(f, ...)
  if (length(objects) < 2) return(f)
  if (!is.null(names(objects))) stop(
    "Joining covariates using named optional arguments is not supported."
  )

  fun <- function(n, ...) {
    res <- add_dots(f)(n, ...)
    if (inherits(res, "data.frame")) res <- list(res)
    for (m in objects[-1]) {
      if (is.function(m)) {
        val <- add_dots(m)(n, ...)
      } else {
        val <- m
      }
      if (inherits(val, c("data.frame", "numeric", "array"))) {
        val <- rep(list(val), length.out = length(res))
      }
      if (length(val) != length(res)) stop("Wrong number of periods")
      for (i in seq_along(res)) {
        res[[i]] <- cbind(res[[i]], val[[i]])
      }
    }
    # do not allow duplicated column names because this may cause unintended
    # behavior downstream which is potentially difficult to detect
    stop_duplicated_covariates(covar = res[[1]])

    # this might occur when trying to attempt adding constant/pre-generated
    # covariates to the output of random number generators. adding this test
    # because it might be challenging to debug
    if (nrow(res[[1]]) != n) stop(paste(
      "Generated covariate data has length", nrow(res[[1]]), "but n =", n,
      "is expected"
    ))

    if (length(res)==1) return(res[[1]])
    return(res)
  }
  return(fun)
}

#' @title Get levels for factor columns in data.table
#' @param data Covariate `data.table`
get_factor_levels <- function(data) {
  factor_levels <- list()
  for (col.name in colnames(data)){
    if (is.factor((d <- data[[col.name]]))) {
      factor_levels[[col.name]] <- levels(d)
    }
  }
  return(factor_levels)
}

# Given a vector of correlation coefficients this function returns the
# corresponding correlation matrix (or variance matrix if the diagonal variance
# term is specified through the `var` argument). The input `cor` is a single
# number if `type` is "cs" (compound-symmetry) or "ar" (autoregressive AR(1)).
# The dimension, \(p\), is in this case by default 2, but can be controlled by
# the length of the `var` argument. For type "to" (Toeplitz) a vector of of
# length \(p-1\) is expected. For an unstructured correlation matrix (type
# "un"), \(p*(p-1)/2\) elements must be specified (row-wise: r[12], r[13], ...,
# ,r[1p], r[23], ... r[2p], ... r[p(p-1)]). If `n` is supplied then `cor` can be
# specified as matrix with \(n\) rows (i.e., in the case of type ="to" it will
# be a \(n\times (p-1)\) matrix) and a \(n\times ((p-1)p/2)\) matrix is returned
# with all the correlation coefficients (row-wise).
par2cor <- function(n,
                   cor = NULL,
                   var = 1,
                    # compound-symmetric, autoregressive AR-power, unstructured
                   type = c("un", "cs", "ar")
                   ) {
  if (is.null(cor)) {
    # 1-dim.
    return(list(p = 1, cor = NULL, var = rep(var, length.out = n)))
  }
  var <- rbind(var)
  type <- tolower(substr(type[1], 1, 2))
  if (type %in% c("cs", "ar")) {
    if (NCOL(cor) != 1L) stop(">1 parameter for CS/AR(1) correlations")
    if (NCOL(var) == 1 && type %in% c("cs", "ar")) {
      ## 2-dim.
      var <- cbind(var, var)
    }
    p <- ncol(var)
    cor <- setup_cor(cor, p, type)
  }
  if (type == "to") {
    cor <- setup_cor(cor, type="to")
  }
  cor <- rbind(cor)
  var <- rbind(var)
  p <- (1 + sqrt(1 + 8 * ncol(cor))) / 2
  if (abs(p - round(p)) > (.Machine$double.eps)^0.5 || p < ncol(var)) {
    stop("Wrong number of correlation coefficients. Use type='cs' or 'ar'?")
  }
  if (ncol(var) < p) {
    idx <- rep(seq_len(ncol(var)), length.out = p)
    var <- var[, idx, drop = FALSE]
  }
  if (missing(n)) { # Return correlation matrix
    R <- matrix(0, p, p)
    diag(R) <- .5
    R[lower.tri(R)] <- cor
    R <- (R + t(R))
    return(structure(R, var = var))
  }
  idx <- rep(seq_len(nrow(cor)), length.out = n)
  cor <- cor[idx, , drop=FALSE]
  idx <- rep(seq_len(nrow(var)), length.out = n)
  var <- var[idx, , drop=FALSE]
  return(list(p = p, cor = cor, var = var))
}

stop_duplicated_covariates <- function(covar) {
  duplicates <- table(colnames(covar))
  duplicates <- duplicates[duplicates > 1]
  if (length(duplicates) > 0) stop(paste0(
    "Duplicated column name (",
    paste(names(duplicates), collapse = ","),
    ") found in joined covariate data.")
  )
}
