# Unit tests for estimators

library("tinytest")
library("data.table")
library("lava")
learner_glm <- targeted::learner_glm

set.seed(1)
m <- lvm() |>
  regression(y1 ~ a * a1 + b * w1 + u) |>
  regression(y2 ~ a * a2 + b * w2 + u) |>
  regression(a1 ~ c * w1) |>
  regression(a2 ~ c * w2) |>
  distribution(~ y1 + y2 + a1 + a2, value = binomial.lvm())

# Data set with two observations per subject
# Response y, treatment a, covariate w
d <- sim(m, 1e3) |>
  mets::fast.reshape(varying = c("y", "a", "w"))
# Cross-sectional data at time-point 1
d0 <- subset(d, num == 1)

test_est1 <- function() {
  # verify that est_adj estimates the same average treatment effect with
  # different argument types. That is, by providing either a character or
  # targeted::learning object to the response argument of `est_adj`
  est0 <- estimate(lm(y ~ a, data = d0), keep = "a")
  e1 <- est_adj(response = "y", treatment = "a", family = gaussian)
  est1 <- e1(d0)
  expect_equivalent(parameter(est0), parameter(est1))
  e2 <- est_adj(response = learner_glm(y ~ a, family = gaussian))
  est2 <- e2(d0)
  expect_equivalent(parameter(est0), parameter(est2))

  # User-specified outcome and treatment variables
  dr <- d0
  colnames(dr)[which(colnames(dr) == "y")] <- "Y"
  colnames(dr)[which(colnames(dr) == "a")] <- "A"
  est0r <- est_adj(response = "Y", treatment = "A")(dr)
  est1r <- est_adj(
    learner_glm(Y ~ A, family = gaussian),
    treatment = "A")(dr)
  expect_equivalent(parameter(est0), parameter(est0r))
  expect_equivalent(parameter(est0), parameter(est1r))

  # Relative risk
  rr <- with(d0, mean(y[a == 1]) / mean(y[a == 0]))
  e1 <- est_adj(family = binomial(), treatment.effect = "rr")
  expect_equal(rr, exp(parameter(e1(d0))[1]))
  e2 <- est_adj(
    response = learner_glm(y ~ a, family = gaussian),
    treatment.effect = "rr"
  )
  expect_equal(rr, exp(parameter(e2(d0))[1]))
}
test_est1()

# Adjustment for baseline covariates
test_est_adj1 <- function() {
  e1 <- est_adj(learner_glm(y ~ a * w, family = gaussian))
  est1 <- e1(d0)
  l <- lm(y ~ a * w, data = d0)

  q1 <- predict(l, newdata = transform(d0, a = 1))
  q0 <- predict(l, newdata = transform(d0, a = 0))
  if1 <- (d0$a == 1) / mean(d0$a == 1) * (d0$y - q1) + q1
  if0 <- (d0$a == 0) / mean(d0$a == 0) * (d0$y - q0) + q0
  est0 <- estimate(
    coef = c(mean(if0), mean(if1), mean(if1 - if0)),
    IC = cbind(if0 - mean(if0), if1 - mean(if1), if1 - if0 - mean(if1 - if0)),
    labels = c("a0", "a1", "ate")
  )
  expect_equivalent(
    parameter(est1),
    parameter(estimate(est0, keep = "ate"))
  )

  # Link function
  e2 <- est_adj(learner_glm(y ~ a * w, family = binomial))
  est2 <- e2(d0)
  expect_equal(coef(est2), coef(est1), tolerance = 0.1)

  l <- glm(y ~ a * w, data = d0, family = binomial)
  q1 <- predict(l, newdata = transform(d0, a = 1), type = "response")
  q0 <- predict(l, newdata = transform(d0, a = 0), type = "response")
  if1 <- (d0$a == 1) / mean(d0$a == 1) * (d0$y - q1) + q1
  if0 <- (d0$a == 0) / mean(d0$a == 0) * (d0$y - q0) + q0
  est0 <- estimate(
    coef = c(mean(if0), mean(if1), mean(if1 - if0)),
    IC = cbind(if0 - mean(if0), if1 - mean(if1), if1 - if0 - mean(if1 - if0)),
    labels = c("a0", "a1", "ate")
  )
  expect_equivalent(
    parameter(est2),
    parameter(estimate(est0, keep = "ate"))
  )
}
test_est_adj1()

# Confidence limits
test_est_adj2 <- function() {
  est <- est_adj(learner_glm(y ~ a * w, family = gaussian), level = 0.8)(d0)
  l <- lm(y ~ a * w, data = d0)
  q1 <- predict(l, newdata = transform(d0, a = 1))
  q0 <- predict(l, newdata = transform(d0, a = 0))
  if1 <- (d0$a == 1) / mean(d0$a == 1) * (d0$y - q1) + q1
  if0 <- (d0$a == 0) / mean(d0$a == 0) * (d0$y - q0) + q0
  est0 <- estimate(
    coef = c(mean(if1 - if0)),
    IC = cbind(if1 - if0 - mean(if1 - if0)),
    labels = c("ate")
  )
  z <- (1-0.8)/2
  ci <- coef(est0) + c(-1, 1) * vcov(est0)[1]**.5 * qnorm(1 - z)
  expect_equal(parameter(est)[3:4], ci, tolerance=1e-3)
}
test_est_adj2()

# Non-iid case
test_est_gee <- function() {
  est <- est_adj(learner_glm(y ~ a, family = gaussian), id = "id")(d)
  g <- geepack::geeglm(y ~ a,
    data = d, id = d$id,
    corstr = "independence", family = gaussian
    )
  expect_equivalent(coef(est), coef(g)[2], tolerance = 1e-5)
  expect_equivalent(vcov(est)[1], vcov(g)[2, 2], tolerance = 1e-5)
}
test_est_gee()

test_est_glm <- function() {
  dd <- data.frame(a = rbinom(10, 1, 0.5), y = rnorm(10))
  expect_equal(
    est_glm()(dd)$coef,
    glm(y ~ a, data = dd)$coef["a"]
  )

  # return NA when standard errors cannot be calculated for target parameter
  dd <- data.frame(a = 1, y = rnorm(10))
  expect_equal(est_glm()(dd)$coef, c(a=NA_real_))
}
test_est_glm()
