# AGENTS.md

## Project overview

`carts` is an R package: a **Monte Carlo simulation framework for randomized
clinical trial (RCT) designs**, with special emphasis on treatment-effect
estimators based on covariate adjustment. Users define a trial (covariate
distributions, outcome model, estimators) and simulate power / sample size.

- License: MIT
- Homepage: https://novonordisk-opensource.github.io/carts/
- Issues: https://github.com/NovoNordisk-OpenSource/carts/issues

## Architecture

Central abstraction is the **`Trial` R6 class** (`R/Trial.R`). A trial is
configured with three user-supplied pieces:

- `covariates` — a generator function of sample size `n` returning a
  `data.table`/`data.frame` (may be a list for multiple observation periods).
- `outcome` — an outcome model whose first positional arg is the covariate data.
- `estimators` — a list (or single) of treatment-effect estimators.

`trial$run(n, R)` runs `R` Monte Carlo replications; `trial$summary()`
summarizes results. Sample size / power search uses a bisection +
Robbins-Monro stochastic approximation (`R/optimization.R`).

### Source layout (`R/`)

- `Trial.R` — the `Trial` R6 class (core entry point, ~1100 lines).
- `trial_run.R` — simulation run logic.
- `covar_sim.R`, `distributions.R` — covariate simulation & distributions
  (`covar_*`, `rmvn`, `rnb`, ...).
- `outcome_models.R` — outcome models (`outcome_count`, `outcome_binary`,
  `outcome_continuous`, ...).
- `estimators.R` — estimators (`est_glm`, `est_adj`, `est_gee`, ...).
- `optimization.R` — sample-size/power search algorithms.
- `aggrsurv.R`, `utils.R` — helpers.
- `RcppExports.R` — **generated**, do not edit by hand.

### Compiled code (`src/`)

C++ via `Rcpp` + `RcppArmadillo` (`mvn.cpp`). Regenerate bindings with
`make rcpp` (`Rcpp::compileAttributes(".")`) after changing exported C++
functions. Do not hand-edit `RcppExports.cpp` / `RcppExports.R`.

## Key dependencies

Depends on `lava`. Imports `data.table`, `R6`, `targeted`, `survival`,
`logger`, `progressr`, `rlang`.

## Commands

Uses R's package tooling plus a `Makefile` wrapper.

- Lint: `make lint` (`lintr::lint_package`)
- Roxygen docs: `make roxygen` (`devtools::document`)
- Regenerate Rcpp bindings: `make rcpp`
- Build README from `inst/README.Rmd`: `make readme`
- Full docs (roxygen + rcpp + readme): `make doc`
- Install locally: `make install`
- Install deps: `make install-deps`
- Test installed package: `make test` (tinytest, parallel)
- Test with load_all (dev): `make test-loadall`
- Slow tests: `make slowtest`
- `R CMD check` (lint + rcmdcheck): `make check` (the default target)
- CRAN-style check: `make check-cran`
- Check examples: `make check-examples`
- Build tarball: `make build`
- pkgdown site: `make pkgdown`

## Conventions

- Follow the [tidyverse style guide](https://style.tidyverse.org/), enforced by
  `lintr` (config in `.lintr`, line length 80). Also respect `.editorconfig`
  (2-space indent, LF, UTF-8, trailing newline for `.R`/C/C++).
- **Notable deviation:** long function argument names use **dots**, not
  underscores — e.g. `outcome.name`, not `outcome_name`. Function names and R6
  methods still use snake_case.
- Document argument types in roxygen, e.g.
  `#' @param n (integer) Number of observations (sample size)`.
- Roxygen with markdown enabled (`Roxygen: list(markdown = TRUE)`); regenerate
  `man/*.Rd` with `make roxygen` — never edit `man/*.Rd` by hand.

## Testing

- Framework: **tinytest**. Tests live in `inst/tinytest/`; slow tests (more than
  a few seconds, excluded from `R CMD check`) in `inst/slowtest/`.
- Test file organization mirrors `R/`: tests for `R/Trial.R` go in
  `inst/tinytest/test_Trial.R`.
- Test internal (non-exported) functions with `carts:::internal_fn(...)`.

## Git / contribution workflow

- Development happens on `main`
- Branch prefixes: `feature/`, `bugfix/`, `hotfix/`, `docs/`, `develop/` with a
  hyphenated description, e.g. `feature/new-trial-interface`.
- PRs target `main` and use
  [conventional commits](https://www.conventionalcommits.org/) titles. Submit as
  draft to skip slow-test/vignette CI until ready for review.

## Do not touch

- Generated files: `R/RcppExports.R`, `src/RcppExports.cpp`, `man/*.Rd`,
  `README.md` (edit `inst/README.Rmd` instead), `NAMESPACE` (from roxygen).
- Build artifacts: `build/`, `carts.Rcheck/`, `src/*.o`, `src/*.so`, and
  vignette `*_cache` / `*_files` dirs.
