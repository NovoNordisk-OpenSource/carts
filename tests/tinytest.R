if (requireNamespace("tinytest", quietly = TRUE)) {
  options(Ncpus = 2)
  data.table::setDTthreads(2)
  tinytest::test_package("carts")
}
