# Aggregate data in counting process format

Aggregate data in counting process format. The aggregation is done
within subject only.

## Usage

``` r
aggrsurv(
  data,
  breaks,
  entry = "entry",
  exit = "exit",
  status = "status",
  id = "id",
  names = c("episode", "entry", "exit", "events", "exposure"),
  ...
)
```

## Arguments

- data:

  data.frame

- breaks:

  vector of time points

- entry:

  name of entry date variable

- exit:

  name exit date variable

- status:

  censoring / event variable

- id:

  id variable

- names:

  character vector of names of new variables

- ...:

  additional arguments to lower level functions

## Value

data.table

## Examples

``` r

dat <- data.table::data.table(
  id = c(1, 1, 1, 1, 2, 2, 2),
  entry = as.Date(c(
    "2021-01-01", "2021-01-20", "2021-02-28", "2021-06-01",
    "2021-01-01", "2021-01-14", "2021-09-01"
 )),
  status = c(1, 1, 1, 1, 1, 1, 0),
  x = rnorm(7)
)
dat[, exit := data.table::shift(entry, 1, type="lead"), by=id]
#>       id      entry status          x       exit
#>    <num>     <Date>  <num>      <num>     <Date>
#> 1:     1 2021-01-01      1 -0.5522281 2021-01-20
#> 2:     1 2021-01-20      1  0.0161406 2021-02-28
#> 3:     1 2021-02-28      1 -0.1279783 2021-06-01
#> 4:     1 2021-06-01      1  0.3917558       <NA>
#> 5:     2 2021-01-01      1  1.7860606 2021-01-14
#> 6:     2 2021-01-14      1 -1.7503669 2021-09-01
#> 7:     2 2021-09-01      0  0.8433364       <NA>
dat[, exit := replace(exit, .N, as.Date("2021-12-31")),
     by = id]
#>       id      entry status          x       exit
#>    <num>     <Date>  <num>      <num>     <Date>
#> 1:     1 2021-01-01      1 -0.5522281 2021-01-20
#> 2:     1 2021-01-20      1  0.0161406 2021-02-28
#> 3:     1 2021-02-28      1 -0.1279783 2021-06-01
#> 4:     1 2021-06-01      1  0.3917558 2021-12-31
#> 5:     2 2021-01-01      1  1.7860606 2021-01-14
#> 6:     2 2021-01-14      1 -1.7503669 2021-09-01
#> 7:     2 2021-09-01      0  0.8433364 2021-12-31

res <- aggrsurv(dat,
  breaks = c(182),
  entry = "entry", exit = "exit", status = "status", id = "id"
)
print(res)
#> Key: <id>
#>       id episode entry  exit events exposure          x
#>    <num>   <num> <num> <num>  <num>    <num>      <num>
#> 1:     1       1     0   182      3      182 -0.5522281
#> 2:     1       2   182   364      1      182 -0.5522281
#> 3:     2       1     0   182      1      182  1.7860606
#> 4:     2       2   182   364      1      182  1.7860606
```
