library("tinytest")
library("lava")
library("data.table")


set.seed(1)
x <- data.table(z = rnorm(1e4), v = rnorm(1e4))

test_outcome_count <- function() {
  pp <- c(1, 0.5, 0.25)
  y <- outcome_count(x, mean = ~ v + z, par = pp)
  expect_true(NROW(outcome_count(x)) == 1e4)

  dt <- cbind(y, x)
  g <- glm(y ~ v + z, data = as.data.frame(dt), family = poisson)
  eps <- 0.1
  expect_true(all(abs(coef(g) - pp) < eps))

  ## Setting parameters via list
  pp <- list(intercept = -1, v = 0.25)
  y <- outcome_count(x, mean = ~ v + z, par = pp)
  dt <- cbind(y, x)
  g <- glm(y ~ v + z, data = as.data.frame(dt), family = poisson)
  eps <- 0.1
  pp <- c(unlist(pp), z = 0)
  expect_true(all(abs(coef(g) - pp) < eps))

  y <- outcome_count(x, function(x, ...) {
    with(x, exp(z * v))
  })
  dt <- cbind(y, x)
  g <- glm(y ~ v:z, data = as.data.frame(dt), family = poisson)
  expect_true(all(abs(coef(g) - c(0, 1)) < eps))

  # test exposure argument with scalar
  dd <- outcome_count(data = x, mean = ~ 1, exposure = 2)
  expect_true(all(dd$exposure == 2))
  # ... with vector. expect very second subject to have exposure time 2
  dd <- outcome_count(data = x, mean = ~ 1, exposure = c(1, 2))
  expect_equal(dd$exposure, rep(c(1, 2), length.out = nrow(dd)))
  # exposure times correctly affect the outcomes
  exposure <- NULL
  expect_equal(mean(dd[exposure == 1]$y * 2), mean(dd[exposure == 2]$y),
    tolerance = 1e-1)
  # ... with function.
  dd <- outcome_count(data = x, mean = ~ 1,
    exposure = function(data) (data$z > 0) + 1
  )
  expect_equal(dd$exposure, (x$z > 0) + 1)
  # one can pass arguments to exposure function (useful when updating parameters
  # via parameter method of Trial class)
  dd <- outcome_count(data = x, mean = ~ 1, const = 3,
    exposure = function(data, const) (data$z > 0) + const
  )
  expect_equal(dd$exposure, (x$z > 0) + 3)
}
test_outcome_count()

test_outcome_binary <- function() {
  pp <- c(1, 0.5, 0.25)
  y <- outcome_binary(x, mean = ~ v + z, par = pp)
  dt <- cbind(y, x)
  g <- glm(y ~ v + z, data = as.data.frame(dt), family = binomial)
  eps <- 0.1
  expect_true(all(abs(coef(g) - pp) < eps))

  y <- outcome_binary(x, function(x, ...) {
    with(x, lava::expit(z * v))
  })
  dt <- cbind(y, x)
  g <- glm(y ~ v:z, data = as.data.frame(dt), family = binomial)
  expect_true(all(abs(coef(g) - c(0, 1)) < eps))
}
test_outcome_binary()

test_outcome_continuous <- function() {
  pp <- c(1, 0.5, 0.25)
  dt <- outcome_continuous(x, mean = ~ v + z, par = pp) |>
    cbind(x)
  g <- glm(y ~ v + z, data = as.data.frame(dt), family = gaussian)
  eps <- 0.1
  expect_true(all(abs(coef(g) - pp) < eps))
  expect_equal(mean(residuals(g)^2), 1.0, tolerance = eps)

  ## Modifying measurement error
  dt <- outcome_continuous(x, mean = ~ v + z, par = pp, sd=0.5) |>
    cbind(x)
  g <- glm(y ~ v + z, data = as.data.frame(dt), family = gaussian)
  expect_true(all(abs(coef(g) - pp) < eps))
  expect_equal(mean(residuals(g)^2), .25, tolerance = eps)

  # Variance heterogeneity
  dt <- outcome_continuous(x, mean = ~ v + z, par = pp, sd = 0, het = 1.0) |>
    cbind(x)
  g <- glm(y ~ v + z, data = dt)
  lr <- lm(abs(residuals(g)) ~ predict(g)) |>
    lava::estimate() |>
    lava::parameter()
  expect_true(lr[2, 1] > 0.5 && lr[2, 5] < 0.05)

  # Flexible mean structure
  y <- outcome_continuous(x, function(x, ...) {
    with(x, z * v)
  })
  dt <- cbind(y, x)
  g <- glm(y ~ v:z, data = as.data.frame(dt), family = gaussian)
  expect_true(all(abs(coef(g) - c(0, 1)) < eps))
}
test_outcome_continuous()

test_outcome_phreg <- function() {
  phreg <- mets::phreg
  phreg.par <- mets::phreg.par
  Surv <- survival::Surv #nolint

  par1 <- list(scale = 1 / 100, shape = 2)
  par0 <- list(scale = 1 / 100, shape = 1)
  m <- lava::lvm() |>
    regression(y ~ a + x) |>
    regression(c ~ 1) |>
    distribution(~y,
      value = coxWeibull.lvm(scale = par1$scale, shape = par1$shape, param = 2)
    ) |>
    distribution(~c,
      value = coxWeibull.lvm(scale = par0$scale, shape = par0$shape, param = 2)
    ) |>
    distribution(~a, value = binomial.lvm()) |>
    eventTime(time ~ min(y, c=0), eventName = "status")

  p1 <- c("y~a" = 1, "y~x" = 1)
  a <- NULL
  d <- sim(m, 1e4, p=p1) |>
    transform(a = factor(a, labels=c("Placebo", "Active")) |>
                relevel(ref="Placebo"))

  fit1 <- phreg(Surv(time, status > 0) ~ a + x, data = d)
  fit0 <- phreg(Surv(time, status == 0) ~ a + x, data = d)

  covar <- covar_bootstrap(d, subset=c("a", "x"))
  xx <- covar(5e4)

  test_phreg1 <- function(fit1, fit0) {
    # Testing `outcome_phreg` with fitted cox models for both event and
    # censoring
    outcome <- setargs(outcome_phreg,
      model = fit1,
      cens.model = fit0
    )
    y <- outcome(xx)
    expect_equivalent(attr(y, "par"), coef(fit1))
    if (is.null(fit0)) {
      expect_true(length(attr(y, "cens.par")) == 0)
    } else {
      expect_equivalent(attr(y, "cens.par"), coef(fit0))
    }

    dd <- cbind(y, xx)
    cox1 <- phreg(Surv(time, status) ~ a + x, data = dd)
    expect_equivalent(coef(cox1), p1, tolerance = 0.2)

    cox0 <- phreg(Surv(time, status==0) ~ a + x, data = dd)
    expect_equivalent(coef(cox0), c(0, 0), tolerance = 0.2)

    pr0 <- prop.table(table(d$status))
    pr <- prop.table(table(dd$status))
    expect_equivalent(pr, pr0, tolerance = 0.1)
  }
  test_phreg1(fit1, fit0)
  test_phreg1(fit1, NULL)  # KM estimator based on outcome model

  test_weibull <- function() {
    outcome <- setargs(outcome_phreg,
      model = list(scale = par1$scale, shape = par1$shape),
      cens.model = list(scale = par0$scale, shape = par0$shape)
    )

    xx0 <- covar(1e4)
    dd <- outcome(xx0) |> cbind(xx0)

    cox1 <- phreg.par(Surv(time, status) ~ a + x, data = dd)
    expect_equivalent(coef(cox1), c(log(unlist(par1)), 0, 0),
                      tolerance = 0.2)

    cox0 <- phreg.par(Surv(time, status) ~ a + x, data = dd)
    expect_equivalent(coef(cox0), c(log(unlist(par0)), 0, 0),
                      tolerance = 0.2)

    ## Specify parameters
    outcome <- setargs(outcome_phreg,
      model = list(scale = par1$scale, shape = par1$shape),
      cens.model = list(scale = par0$scale, shape = par0$shape),
      par = coef(fit1),
      cens.par = coef(fit1)
    )
    dd <- outcome(xx0) |> cbind(xx0)

    cox1 <- phreg.par(Surv(time, status) ~ a + x, data = dd)
    expect_equivalent(coef(cox1), c(log(unlist(par1)), coef(fit1)),
                      tolerance = 0.2)

    cox0 <- phreg.par(Surv(time, status) ~ a + x, data = dd)
    expect_equivalent(coef(cox0), c(log(unlist(par0)), coef(fit1)),
                      tolerance = 0.2)


    # Specify design via `lp` argument
    outcome <- setallargs(
      outcome_phreg,
      model = list(scale = par1$scale, shape = par1$shape),
      cens.model = list(scale = par0$scale, shape = par0$shape),
      lp = ~ a + x,
      cens.lp = ~a,
      par = c(x = 0.5),
      cens.par = c(x = 1),
      default.parameter = 1
    )
    y <- outcome(xx)
    expect_true(length(attr(y, "par")) == 2)
    expect_true(length(attr(y, "cens.par")) == 1)

    dd <- cbind(y, xx)
    cox1 <- phreg(Surv(time, status) ~ a + x, data = dd)
    expect_equivalent(coef(cox1), c(1, 0.5), tolerance = 0.1)
  }
  test_weibull()

  test_interaction <- function() {
    outcome <- setallargs(
      outcome_phreg,
      model = fit1,
      # Adds main effect and first order interactions to
      # simulation model even if they are not present in the
      # original model
      treatment = "a",
      default.parameter = -0.2,
      par = list(x = 0)
    )
    dd <- outcome(xx) |> cbind(xx)

    cox1 <- phreg(Surv(time, status) ~ a * x, data = dd)
    true <- rep(-0.2, 3)
    true[which(names(coef(cox1)) == "x")] <- 0
    true[which(names(coef(cox1)) == "aActive")] <- 1
    expect_equivalent(coef(cox1), true, tolerance = 0.11)
  }
  test_interaction()
}
test_outcome_phreg()
