### Unit tests for 'aggrsurv'
### Aggregation of data in counting process format

dat <- data.table::data.table(
  subj = c(1, 1, 1, 1, 2, 2, 2),
  entrydt = as.Date(c(
    "2021-01-01", "2021-01-20", "2021-02-28", "2021-06-01",
    "2021-01-01", "2021-01-14", "2021-09-01"
  )),
  event = c(1, 1, 1, 1, 1, 1, 0),
  x = rnorm(7)
)
dat[, exitdt := data.table::shift(entrydt, 1, type = "lead"), by = subj]
dat[, exitdt := replace(exitdt, .N, as.Date("2021-12-31")), by = subj]

res <- aggrsurv(dat,
  breaks = c(182),
  entry = "entrydt", exit = "exitdt", status = "event", id = "subj",
  names = c("episode", "entry", "exit", "events", "exposure")
)

expect_equal(nrow(res), 4)
expect_equal(sum(res$exposure), 364 * 2)
expect_equal(sum(res$events), sum(dat$event))
# baseline covariates are ported to res
x_base <- dat[, list(x = .SD[order(entrydt)]$x[1]), by = "subj"][order(subj)]
expect_equivalent(x_base, res[episode == 1, c("subj", "x")][order(subj)])

res_wo_x <- aggrsurv(dat[, !"x"],
  breaks = c(182),
  entry = "entrydt", exit = "exitdt", status = "event", id = "subj",
  names = c("episode", "entry", "exit", "events", "exposure")
)
expect_equivalent(res_wo_x, res[, !"x"])
