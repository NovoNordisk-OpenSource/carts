library("tinytest")

test_setargs <- function() {
  foo <- function(x, b, a = 5) c(a=a, b=b, x=x)

  # overwriting argument with existing default value works
  x <- 2
  f <- setargs(foo, a=2)
  expect_equal(f(x, b=5), c(a=2, b=5, x=x))

  # error because b is missing
  expect_error(f(x))

  f <- setargs(foo, b=5)
  # no error because default value for b is set
  expect_equal(f(x), c(a=5, b=5, x=x))

  # default value can be overruled / a=2 is still default
  expect_equal(f(x, b = 50), c(a=5, b=50, x=x))

  # updating multiple arguments at once works
  expect_equal(setargs(foo, a=2, b = 2)(x), c(a=2, b=2, x=x))

  foo0 <- function(x, a = 5, ...) {
    foob <- function(x, b = 5) return(b)
    return(c(a=a, b=foob(x, ...), x=x))
  }
  # do not overwrite default value in lower level functions through ... . That
  # is, b keeps it default value of 5
  expect_equal(
    setargs(foo0, a = 10, b = 10, setargs.warn = FALSE)(x),
    c(a = 10, b = 5, x = x)
  )

  # expect warning when trying to set default value for argument that does
  # not exist in f
  expect_warning(setargs(foo, aa = 1, warn = TRUE))
}
test_setargs()

test_setallargs <- function() {
  foo <- function(x, a = 5, ...) {
    foob <- function(x, b = 5) return(b)
    return(c(a=a, b = foob(x, ...), x = x))
  }

  # test that b is set correctly in lower level function + a in main function
  x <- 2
  f <- setallargs(foo, a = 10, b = 20)
  expect_equal(f(x), c(a = 10, b = 20, x = x))
  # test that a and b can be overwritten during call of f
  expect_equal(f(x, a = 20, b = 30), c(a = 20, b = 30, x = x))
  # test that setting only a or b works independently of each other
  expect_equal(setallargs(foo, a = 10)(x), c(a = 10, b = 5, x = x))
  expect_equal(setallargs(foo, b = 10)(x), c(a = 5, b = 10, x = x))
}
test_setallargs()

test_append <- function() {
  x <- list()
  append(x) <- 1
  expect_equal(list(1), x)
  # adding named value to an unnamed list
  append(x, name = "a") <- 2
  expect_equal(c("", "a"), names(x))
  # adding unnamed value to a named list
  x <- list(a = 1)
  append(x) <- 1
  expect_equal(c("a", ""), names(x))

  # allow repeated names
  x <- list(a = 1)
  append(x, name="a") <- 2
  expect_equal(c("a", "a"), names(x))

  # value is always converted to a list
  x <- list()
  val <- c(1, 2)
  append(x) <- val
  expect_equal(list(val), x)
}
test_append()
