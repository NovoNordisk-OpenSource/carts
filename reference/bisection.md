# Root finding by bisection

Root finding by bisection

## Usage

``` r
bisection(f, interval, niter = 6, tol = 1e-12, verbose = TRUE, ...)
```

## Arguments

- f:

  function to find root of (monotonic)

- interval:

  a vector containing the end-points of the interval to be searched for
  the root

- niter:

  number of iterations

- tol:

  stopping criterion (absolute difference in function evaluated at end
  points of current interval)

- verbose:

  if TRUE additional messages are printed throughout the optimization

- ...:

  additional arguments passed to `f`

## Value

numeric specifying the root

## Examples

``` r
testf <- function(x) (x - sqrt(2))
carts:::bisection(testf, c(0, 1000), niter = 30)
#> INFO [2026-07-03 07:29:09] Evaluating left point: 0
#> INFO [2026-07-03 07:29:09] Evaluating right point: 1000
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 500  [iteration 1/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 250  [iteration 2/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 125  [iteration 3/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 62.5  [iteration 4/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 31.25  [iteration 5/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 15.625  [iteration 6/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 7.8125  [iteration 7/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 3.90625  [iteration 8/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.953125  [iteration 9/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 0.9765625  [iteration 10/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.46484375  [iteration 11/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.220703125  [iteration 12/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.3427734375  [iteration 13/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.40380859375  [iteration 14/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.434326171875  [iteration 15/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.4190673828125  [iteration 16/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.41143798828125  [iteration 17/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.41525268554688  [iteration 18/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.41334533691406  [iteration 19/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.41429901123047  [iteration 20/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.41382217407227  [iteration 21/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.41406059265137  [iteration 22/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.41417980194092  [iteration 23/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.41423940658569  [iteration 24/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.41420960426331  [iteration 25/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.4142245054245  [iteration 26/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.4142170548439  [iteration 27/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.4142133295536  [iteration 28/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.41421519219875  [iteration 29/30]
#> INFO [2026-07-03 07:29:09] Evaluating mid point: 1.41421426087618  [iteration 30/30]
#> [1] 1.414214
```
