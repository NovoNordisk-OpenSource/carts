#' Aggregate data in counting process format. The aggregation is done within
#' subject only.
#'
#' @title Aggregate data in counting process format
#' @param data data.frame
#' @param breaks vector of time points
#' @param entry name of entry date variable
#' @param exit name exit date variable
#' @param status censoring / event variable
#' @param id id variable
#' @param names character vector of names of new variables
#' @param ... additional arguments to lower level functions
#' @return data.table
#' @export
#' @examples
#'
#' dat <- data.table::data.table(
#'   id = c(1, 1, 1, 1, 2, 2, 2),
#'   entry = as.Date(c(
#'     "2021-01-01", "2021-01-20", "2021-02-28", "2021-06-01",
#'     "2021-01-01", "2021-01-14", "2021-09-01"
#'  )),
#'   status = c(1, 1, 1, 1, 1, 1, 0),
#'   x = rnorm(7)
#' )
#' dat[, exit := data.table::shift(entry, 1, type="lead"), by=id]
#' dat[, exit := replace(exit, .N, as.Date("2021-12-31")),
#'      by = id]
#'
#' res <- aggrsurv(dat,
#'   breaks = c(182),
#'   entry = "entry", exit = "exit", status = "status", id = "id"
#' )
#' print(res)
aggrsurv <- function(data, breaks,
                     entry = "entry",
                     exit = "exit",
                     status = "status",
                     id = "id",
                     names = c(
                       "episode", "entry", "exit",
                       "events", "exposure"
                     ),
                     ...) {
  data <- data.table::as.data.table(data)
  data.table::setkeyv(data, cols = id) ## Set index (for merging)
  data.table::setorderv(data, cols = c(id, entry)) ## Order by id+time
  ## Extract baseline information at first time point (duration zero)
  ## for each subject (we omit the original time-to-event variables)
  cols <- setdiff(names(data), c(id, entry, exit, status))
  base <- data[, .SD[1], by = c(id), .SDcols = cols]
  ## Create new data-set with duration variable from entry, exit variables
  newd <- data[, list(
    dur.entry_ = as.Date(.SD[[1]]) - min(as.Date(.SD[[1]])),
    dur.exit_ = as.Date(.SD[[2]]) - as.Date(min(.SD[[1]])),
    status_ = .SD[[3]]
  ), by = c(id), .SDcols = c(entry, exit, status)]
  ## Process point-process data into additional time splits defined by 'breaks'
  dd <- survival::survSplit(
    start = "dur.entry_", end = "dur.exit_", event = "status_",
    cut = breaks, data = newd, episode = "episode_"
  ) |> data.table::data.table()
  ## Aggregate for each individual the total duration and number of events in
  ## the time-intervals defined by 'breaks'
  agd <- dd[, list(
    entry_ = min(.SD[[1]]),
    exit_ = max(.SD[[2]]),
    events_ = sum(.SD[[3]])
  ),
  by = c(id, "episode_"),
  .SDcols = c("dur.entry_", "dur.exit_", "status_")
  ]
  agd <- agd[, "exposure_" := .SD[[2]] - .SD[[1]],
    .SDcols = c("entry_", "exit_")
  ]
  ## Rename columns according to 'names' argument
  data.table::setnames(agd,
    old = c("episode_", "entry_", "exit_", "events_", "exposure_"),
    new = names
  )
  ## Merge with baseline information
  data.table::setkeyv(agd, cols = id)
  # all.x is required to prevent empty `res` when `nrow(base) == 0`, which
  # occurs when no baseline information is available in data.
  res <- merge(x = agd, y = base, all.x = TRUE)

  return(res)
}
