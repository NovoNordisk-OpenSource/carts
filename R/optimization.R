#' @description Root finding by bisection
#' @title Root finding by bisection
#' @param f function to find root of (monotonic)
#' @param interval a vector containing the end-points of the interval to be
#'   searched for the root
#' @param niter number of iterations
#' @param tol stopping criterion (absolute difference in function evaluated at
#'   end points of current interval)
#' @param verbose if TRUE additional messages are printed throughout the
#'   optimization
#' @param ... additional arguments passed to `f`
#' @return numeric specifying the root
# #' @examples
# #' testf <- function(x) (x - sqrt(2))
# #' bisection(testf, c(0, 1000), niter = 30)
bisection <- function(f, interval, niter = 6, tol = 1e-12,
                      verbose = TRUE, ...) {
  interval <- sort(interval)
  if (verbose) log_info("Evaluating left point: {interval[1]}")
  fmin <- f(interval[1], ...)
  if (verbose) log_info("Evaluating right point: {interval[2]}")
  fmax <- f(interval[2], ...)

  sign <- 1
  if (fmin > fmax) {
    sign <- -1
    fmin <- sign * fmin
    fmax <- sign * fmax
  }
  for (i in seq_len(niter)) {
    if (fmin > 0) return(interval[1])
    if (fmax < 0) return(interval[2])
    if (abs(fmin - fmax) < tol) break
    new <- (interval[2] - interval[1]) / 2 + interval[1]
    if (verbose) {
      log_info("Evaluating mid point: {new}  [iteration {i}/{niter}]")
    }
    fnew <- sign * f(new, ...)
    if (fnew < 0) {
      interval[1] <- new
      fmin <- fnew
    } else {
      interval[2] <- new
      fmax <- fnew
    }
  }
  return((interval[2] - interval[1]) / 2 + interval[1])
}


#' @title Root solver by Stochastic Approximation
#' @param f Stochastic function
#' @param init Initial value to evaluate `f` in
#' @param function.args Additional arguments passed to `f`
#' @param ... Additional arguments passed to `f`
#' @param method Method ('standard': standard Robbins-Monro, 'discrete' or
#'   'interpolate' for integer stochastic optimization. See details section)
#' @param projection Optional projection function that can be used to constrain
#'   the parameter value (e.g., function(x) max(x, tau)). Applied at the end of
#'   each iteration of the optimization.
#' @param control Control arguments (`niter` number of iterations, `alpha` and
#'   `stepmult` control the step-length of the Polyak-Ruppert algorithm as
#'   described in the details section).
#' @param verbose if TRUE additional messages are printed throughout the
#'   optimization
#' @param burn.in Burn-in (fraction of initial values to disregard) when
#'   applying Polyak–Ruppert averaging (alpha<1). Alternatively, \code{burn.in}
#'   can be given as an integer defining the absolut number of iterations to
#'   disregard.
#' @details The aim is to find the root of the function \eqn{ M(\theta) = E
#'   f(\theta)}, where we are only able to observe the stochastic function
#'   \eqn{f(\theta)}. We will assume that $M$ is non-decreasing with a unique
#'   root \eqn{\theta^\ast}. The Robbins-Monro algorithm works through the
#'   following update rule from an initial start value \eqn{\theta_0}:
#'   \deqn{\theta_{n+1} = \theta_{n} - a_n f(\theta_n)} where
#'   \eqn{\sum_{n=0}^\infty a_n = \infty} and \eqn{\sum_{n=0}^\infty a_n^2 <
#'   \infty}.
#'
#' By averaging the iterates \deqn{\overline{\theta}_n =
#' \frac{1}{n}\sum_{i=1}^{n-1}\theta_i} we can get improved stability of the
#' algorithm that is less sensitive to the choice of the step-length \eqn{a_n}.
#' This is known as the Polyak-Ruppert algorithm and to ensure convergence
#' longer step sizes must be made which can be guaranteed by using \eqn{a_n =
#' Kn^{-\alpha}} with \eqn{0<\alpha<1}. The parameters \eqn{K} and \eqn{\alpha}
#' are controlled by the \code{stepmult} and \code{alpha} parameters of the
#' \code{control} argument.
#'
#'
#' For discrete problems where \eqn{\theta} must be an integer, we follow (Dupac
#' & Herkenrath, 1984). Let \eqn{\lfloor x\rfloor} denote the integer part of
#' \eqn{x}, and define either (method="discrete"): \deqn{g(\theta) =
#' I(U<\theta-\lfloor\theta\rfloor)f(\lfloor\theta\rfloor) +
#' I(U\geq\theta-\lfloor\theta\rfloor)f(\lfloor\theta\rfloor+1)} where
#' \eqn{U\sim Uniform([0,1])}, or (method="interpolate"): \deqn{g(\theta) =
#' (1-\theta+\lfloor\theta\rfloor)f(\lfloor\theta\rfloor) +
#' (\theta-\lfloor\theta\rfloor)f(\lfloor\theta\rfloor+1).} The stochastic
#' approximation method is then applied directly on \eqn{g}.
#'
#' Dupač, V., & Herkenrath, U. (1984). On integer stochastic approximation.
#' Aplikace matematiky, 29(5), 372-383.
# #' @examples
# #' # Finding approximate median u, P(X<=u) = .5, of X~Exp(1):
# #' f <- function(x) mean(rexp(10) <= x) - 0.5
# #' res <- optim_sa(f, 0, control=list(niter=2000, alpha=.5))
# #' res$estimate
optim_sa <- function(f, init = 0, #nolint
                     function.args = list(),
                     ...,
                     method = c("standard", "discrete", "interpolate"),
                     projection = identity,
                     control = list(
                       niter = 100,
                       alpha = 0.25,
                       stepmult = 1
                     ),
                     verbose = TRUE,
                     burn.in = 0.75) {
  control0 <- list(niter = 100, alpha = 0.25, stepmult = 1)
  control0[names(control)] <- control
  if (burn.in < 0) stop("Invalid burn.in (fraction or integer expected)")
  if (burn.in < 1) {
    # burn.in given as a fraction
    burn.in <- floor(burn.in * control0$niter)
  } else {
    if (burn.in >= control0$niter) stop("Too large burn.in")
  }

  res <- numeric(control0$niter)
  p <- progressr::progressor(control0$niter)
  cur <- init
  method <- tolower(method[1])
  discrete_optim <- method %in% c("discrete", "interpolate")

  for (i in seq_len(control0$niter)) {
    if (control0$alpha == 1) { # Standard RM
      stepsize <- control0$stepmult / i
    } else { # Polyak–Ruppert averaging
      stepsize <- with(control0, exp(-alpha * log(i)) * stepmult)
    }
    dots <- list(...)
    cur <- projection(cur)
    function.args[names(dots)] <- dots
    if (discrete_optim) {
      # discrete domain
      cur.integer <- floor(cur)
      cur.real <- cur - cur.integer
      if (method == "discrete") {
        u <- runif(1)
        if (u < cur.real) {
          val <- do.call(f, c(list(cur.integer), function.args))
        } else {
          val <- do.call(f, c(list(cur.integer + 1), function.args))
        }
      } else { # interpolation
        val <- (1 - cur.real) *
          do.call(f, c(list(cur.integer), function.args)) +
          cur.real *
          do.call(f,  c(list(cur.integer + 1), function.args))
      }
    } else {
      val <- do.call(f,  c(list(cur), function.args))
    }
    cur <- projection(cur - stepsize * val)
    res[i] <- cur
    if (discrete_optim) res[i] <- floor(res[i])

    if (verbose) p(message = sprintf("f(%.3f)=%.3f", cur, val))
  }
  if (control0$alpha == 1) { # Standard RM
    return(list(sd = sd(res), raw = res, estimate = tail(res, 1)))
  }
  val <- list(raw = res)
  if (burn.in > 0) {
    n <- round(length(res) - burn.in)
    res <- tail(res, n)
  }
  stddev <- sd(res)
  res <- cumsum(res) / seq_along(res)
  val <- c(val, list(
    running.mean = res,
    sd = stddev,
    estimate = tail(res, 1)
    ))
  if (discrete_optim) val$estimate <- floor(val$estimate)
  return(val)
}
