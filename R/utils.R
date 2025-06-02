#' @export
"append<-" <- function(x, name, value) UseMethod("append<-")

#' @export
#' @title Assignment function to append values to existing list
#' @param x existing list
#' @param name optional name of new element
#' @param value new value to add to existing list
#' @aliases append<- append<-.list
#' @examples
#' x <- list()
#' append(x) <- 1
#' append(x, name = "a") <- 2
#' # duplicated names are allowed
#' append(x, name = "a") <- 3
#' x
"append<-.list" <- function(x, name = NULL, value) {
  res <- c(x, list(value))
  if (!is.null(name)) names(res)[length(res)] <- name

  # replace NAs in res when appending named values to an unnamed list. this then
  # achieves the same behavior when adding an "unnamed" value to a named list,
  # which itself mimics names(list(a = 1, 2)) >> c("a", "")
  mask <- is.na(names(res))
  if (sum(mask) > 0) names(res)[mask] <- ""

  return(res)
}

#' @name setargs
#' @export
#' @title Set default arguments of a function
#' @description Sets default values for arguments in `f`. Care should be
#' taken when `missing` is used in `f` (see examples).
#' @param f function
#' @param ... arguments to set
#' @param setargs.warn cast warning when trying to set default values for
#' arguments that do not exist in `f`
#' @author Benedikt Sommer
#' @aliases setallargs
#' @examples
#' foo <- function(x, a = 5, ...) {
#'   foo1 <- function(x, b = 5) return(b)
#'   c(a = a, b = foo1(x, ...), x = x)
#' }
#' foo(1)
#'
#' f <- setargs(foo, a = 10) # set new default value for a
#' f(1)
#'
#' # default value of b in lower-level function is unaffected and warning is
#' # cast to inform that b is not an argument in f
#' f <- setargs(foo, b = 10)
#' f(1)
#' # disable warning message
#' setargs(foo, b = 10, setargs.warn = FALSE)(1)
#'
#' # arguments of lower-level functions can be set with setallargs
#' f <- setallargs(foo, a = 10, b = 10)
#' f(1)
#'
#' # does not work when `missing` checks for missing formal arguments.
#' foo1 <- function(x, a) {
#'   if (missing(a)) a <- 5
#'   return(c(x = x, a = a))
#' }
#' f <- setargs(foo1, a = 10)
#' f(1)
setargs <- function(f, ..., setargs.warn = TRUE) {
  if (any(grepl("missing\\(", body(f)))) {
    warning("`missing` calls not working with `formals`")
  }
  dots <- list(...)
  f_base <- formals(f)

  f_unique_args <- setdiff(names(f_base), names(dots))
  shared_args <- intersect(names(f_base), names(dots))
  args_not_in_f <- setdiff(names(dots), shared_args)
  if ((length(args_not_in_f) > 0) && setargs.warn) {
    warning(
      sprintf("%s %s not found in function f.",
        ifelse(length(args_not_in_f) == 1, "Argument", "Arguments"),
        paste(args_not_in_f, collapse = ", ")
      )
    )
  }

  formals(f) <- c(
    f_base[c(f_unique_args)], # don't update pars that are unique to f
    dots[c(shared_args)] # update pars only when defined in formals(f) and dots
  )
  return(f)
}

#' @export
setallargs <- function(f, ...) {
  # cast no warning because ... will also contain arguments of lower-level
  # functions in f
  f <- setargs(f, ..., setargs.warn = FALSE)
  # parameters in dots that are not parameters in f, i.e. parameters of lower
  # level functions in f
  dots_defined <- list(...)
  dots_defined_unique <- setdiff(names(dots_defined), formalArgs(f))

  if (length(dots_defined_unique > 0)) {
    args_lower_defined <- dots_defined[c(dots_defined_unique)]
    return(
      function(x, ...) {
        dots_call <- list(...)
        # use default arguments for lower level functions if no new values are
        # defined in the current function call. args will be empty if dots_call
        # include all parameters that are defined in args_lower_defined
        args <- args_lower_defined[
          c(setdiff(names(args_lower_defined), names(dots_call)))
        ]
        # now only add parameters in current call that are not already in args
        args <- c(
          args,
          dots_call[c(setdiff(names(dots_call), names(args)))]
          )

        return(do.call(f, c(list(x), args)))
    }
    )
  } else {
    return(f)
  }
}

# Add optional arguments to function
add_dots <- function(f) {
  if (is.function(f) && !("..." %in% formalArgs(f))) {
    formals(f) <- c(formals(f), alist(... = ))
  }
  return(f)
}

get_family <- function(family) {
  err <- function(family) stop("'family' not recognized: ", family)

  if (is.character(family)) {
    tryCatch(
      family <- get(family, mode = "function", envir = parent.frame()),
      error = function(e) err(family)
    )
  }
  if (is.function(family)) return(family())
  if (inherits(family, "family")) {
    return(family)
  } else {
    return(err(family))
  }
}

# Given parameters in 'cor' returns 'p'x'p' correlation matrix
# specified by 'type' (cs compound/symmetry, ar autoregressive,
# to toeplitz)
setup_cor <- function(cor, p, type) {
  if (type == "cs") {
    cor <- cbind(cor)[, rep(1, p * (p - 1) / 2), drop = FALSE]
  } else { # Toeplitz / AR(1)
    if (type == "ar") {
      cor0 <- cor
      cor <- c()
      for (i in seq(1, p - 1)) {
        cor <- cbind(cor, cor0**i)
      }
    }
    if (type == "to") {
      cor <- rbind(cor)
      p <- ncol(cor) + 1
    }
    newcor <- c()
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        newcor <- cbind(newcor, cor[, j - i])
      }
    }
    cor <- newcor
  }
  return(cor)
}

cat_header <- function(x, ...) {
  cat(sprintf(
    paste("\u2500\u2500", x, "\u2500\u2500"),
    ...
  ), "\n\n")
}

cat_rule <- function(len = 8) {
  cat(rep("\u2500", length.out = len), sep = "")
}

cat_bullet <- function(x, ...) {
  cat(sprintf(paste0(" \u2022 ", x), ...))
}
