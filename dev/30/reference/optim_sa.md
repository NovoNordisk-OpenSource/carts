# Root solver by Stochastic Approximation

Root solver by Stochastic Approximation

## Usage

``` r
optim_sa(
  f,
  init = 0,
  function.args = list(),
  ...,
  method = c("standard", "discrete", "interpolate"),
  projection = identity,
  control = list(niter = 100, alpha = 0.25, stepmult = 1),
  verbose = TRUE,
  burn.in = 0.75
)
```

## Arguments

- f:

  Stochastic function

- init:

  Initial value to evaluate `f` in

- function.args:

  Additional arguments passed to `f`

- ...:

  Additional arguments passed to `f`

- method:

  Method ('standard': standard Robbins-Monro, 'discrete' or
  'interpolate' for integer stochastic optimization. See details
  section)

- projection:

  Optional projection function that can be used to constrain the
  parameter value (e.g., function(x) max(x, tau)). Applied at the end of
  each iteration of the optimization.

- control:

  Control arguments (`niter` number of iterations, `alpha` and
  `stepmult` control the step-length of the Polyak-Ruppert algorithm as
  described in the details section).

- verbose:

  if TRUE additional messages are printed throughout the optimization

- burn.in:

  Burn-in (fraction of initial values to disregard) when applying
  Polyak–Ruppert averaging (alpha\<1). Alternatively, `burn.in` can be
  given as an integer defining the absolut number of iterations to
  disregard.

## Details

The aim is to find the root of the function \\ M(\theta) = E
f(\theta)\\, where we are only able to observe the stochastic function
\\f(\theta)\\. We will assume that \$M\$ is non-decreasing with a unique
root \\\theta^\ast\\. The Robbins-Monro algorithm works through the
following update rule from an initial start value \\\theta_0\\:
\$\$\theta\_{n+1} = \theta\_{n} - a_n f(\theta_n)\$\$ where
\\\sum\_{n=0}^\infty a_n = \infty\\ and \\\sum\_{n=0}^\infty a_n^2 \<
\infty\\.

By averaging the iterates \$\$\overline{\theta}\_n =
\frac{1}{n}\sum\_{i=1}^{n-1}\theta_i\$\$ we can get improved stability
of the algorithm that is less sensitive to the choice of the step-length
\\a_n\\. This is known as the Polyak-Ruppert algorithm and to ensure
convergence longer step sizes must be made which can be guaranteed by
using \\a_n = Kn^{-\alpha}\\ with \\0\<\alpha\<1\\. The parameters \\K\\
and \\\alpha\\ are controlled by the `stepmult` and `alpha` parameters
of the `control` argument.

For discrete problems where \\\theta\\ must be an integer, we follow
(Dupac & Herkenrath, 1984). Let \\\lfloor x\rfloor\\ denote the integer
part of \\x\\, and define either (method="discrete"): \$\$g(\theta) =
I(U\<\theta-\lfloor\theta\rfloor)f(\lfloor\theta\rfloor) +
I(U\geq\theta-\lfloor\theta\rfloor)f(\lfloor\theta\rfloor+1)\$\$ where
\\U\sim Uniform(\[0,1\])\\, or (method="interpolate"): \$\$g(\theta) =
(1-\theta+\lfloor\theta\rfloor)f(\lfloor\theta\rfloor) +
(\theta-\lfloor\theta\rfloor)f(\lfloor\theta\rfloor+1).\$\$ The
stochastic approximation method is then applied directly on \\g\\.

Dupač, V., & Herkenrath, U. (1984). On integer stochastic approximation.
Aplikace matematiky, 29(5), 372-383.

## Examples

``` r
# Finding approximate median u, P(X<=u) = .5, of X~Exp(1):
f <- function(x) mean(rexp(10) <= x) - 0.5
res <- carts:::optim_sa(f, 0, control=list(niter=2000, alpha=.5))
res$estimate
#> [1] 0.6813602
```
