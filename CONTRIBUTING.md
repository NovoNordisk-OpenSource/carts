# Contributing to carts

First off, thanks for taking the time to contribute! ❤️

All types of contributions are encouraged and valued. See the
[Table of Contents](#table-of-contents)
for details about how to contribute code to this project. Please make sure to
read the relevant section before making your contribution. It will make it a lot
easier for us maintainers and smooth out the experience for all involved.

## Table of Contents

- [Branches and branch prefixes](#branches-and-branch-prefixes)
- [Style guide](#style-guide)
- [Unit testing](#unit-testing)
- [Continuous Integration](#continuous-integration)

## Branches and branch prefixes

The package is developed on `dev` while the latest stable release is made
available on `main`. Releases follow a frequency of about 4 weeks, except for
hotfixes or when larger features become available. All feature branches should
be created of `dev`. Branches containing hotfixes should contrary be created of
`main` to avoid merging unfinished features from `dev` into `main`.

### Branch prefixes

We use prefixes to label branches. A meaningful short description follows the
prefix and hyphens (-) are used for separation. For example,
`feature/new-trial-interface` is a valid feature branch name.

*feature/*: Branches for developing new features.\
*bugfix/*: Branches for fixing non-critical bugs.\
*hotfix/*: Branches for fixing critical bugs.\
*docs/*: Branches for writing, updating, or fixing documentation.\
*develop/*: Branches for anything else.

### Pull requests

Except for hotfixes, all pull requests (PRs) must be made on `dev`.
The title of the PR should follow the format of
[conventional commits](https://www.conventionalcommits.org/en/v1.0.0/) and a
summary of the proposed changes must be provided in the body of the PR. This
makes it easier for maintainers as title and body can be reused once all commits
are squashed before merging the feature branch into `dev`.

## Style guide

Contributors should set up their development environment to comply with the
project specified [editorconfig](https://editorconfig.org/) configuration and
[lintr](https://lintr.r-lib.org/) static code analysis tool. We follow to the
largest extent the
[tidyverse style guide](https://style.tidyverse.org/index.html), which is
checked via the code linter. A notable difference is that we use dots for long
argument names of functions. For example, instead of `outcome_name` we use
`outcome.name`. We remain using snake case for names of functions and R6 class
methods. We declare argument types of parameters in the roxygen documentation by following

```{r}
#' @param n (integer) Number of observations (sample size)
#' @param estimators (list or function) List of estimators or a single unnamed estimator
```

as a convention.

## Unit testing

Unit tests are written using the
[tinytest](https://cran.r-project.org/web/packages/tinytest/index.html)
framework and go into the `inst/tinytest` directory. Tests which
take considerable time to complete (more than a few seconds) go into
`inst/slowtest`. These tests are not performed when checking with
package with `R CMD check`. The organization of test files should match the
organization of `R` files. That is, tests for a function in `R/Trial.R` go into
`tinytest/test_Trial.R`.

**Tinytest** does not make internal functions directly callable like some other
unit testing packages do. Thus, testing internal functions requires using `:::`
inside the test files like

```{r}
output <- carts:::some_internal_function(1)
expect_equal(output, 2)
```

More information about unit testing with **tinytest** is provided in this
[vignette](https://cran.r-project.org/web/packages/tinytest/vignettes/using_tinytest.pdf).

## Continuous integration

A variety of continuous integration tests are set up in
[.github/workflows](.github/workflows) to mitigate the risk of committing
malfunctioning code in to `main`. Linting checks (`lint-project.yaml`), checking
synchronicity between Rd files and roxygen code (`check-roxygen.yaml`), unit
tests and rcmdcheck (`r-cmd-check.yaml`) workflows are triggered for every PR on
`dev` and `main`. As CI builds are triggered with each new commit to a feature
branch, to reduce waiting times and compute costs, slow unit tests (`slow-tests.yaml`) and building vignettes (`vignettes.yaml`) are only triggered for PRs that are marked as
ready for review. That is, running tests workflows is prevented by submitting pull
requests as drafts.

## Attribution

This guide is based on the **contributing.md**.
[Make your own](https://contributing.md/)!
